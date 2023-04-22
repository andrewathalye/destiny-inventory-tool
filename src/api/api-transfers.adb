pragma Ada_2022;
with Interfaces;  use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

--  AWS
with AWS.Client;
with AWS.Response; use AWS;

--  VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
use VSS.JSON.Pull_Readers;
use VSS.JSON;
with VSS.Text_Streams; use VSS.Text_Streams;

--  Local Packages
with API.Memberships;
with API.Profiles;
use all type API.Profiles.Transfer_Status_Type;
with API.Manifest.Tools;
use all type API.Manifest.Tools.Bucket_Location_Type;
with API.Error_Codes;
use all type API.Error_Codes.Error_Code_Type;

with GUI.Base;
with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared;

package body API.Transfers is
   --  Server Check

   procedure Server_Check (Data : AWS.Response.Data) is

      Stream : Memory_UTF8_Input_Stream_Access :=
        Get_Stream (Response.Message_Body (Data));
      Reader     : JSON_Simple_Pull_Reader;
      Error_Code : Error_Codes.Error_Code_Type;

   begin
      Set_Stream (Reader, Input_Text_Stream_Access (Stream));
      --  Check passes if S200 returned

      if Query_Status (Data) then
         Free (Stream);
         return;
      end if;
      Wait_Until_Key (Reader, "ErrorCode");
      Read_Next (Reader); -- NUMBER_VALUE
      Error_Code :=
        Error_Codes.Error_Code_Type'Enum_Val
          (As_Integer (Number_Value (Reader)));
      Free (Stream);
      Put_Line
        (Standard_Error,
         "[Error] API.Transfers got " & Error_Code'Image &
         " after passing all local checks");
      --  Wipe profile data and reload Reloading profile data requires the lock
      --  to be unlocked, but we return to the Gtk event handler, so it should
      --  be locked afterwards

      GUI.Locked_Wrapper (GUI.Base.Reload_Profile_Data'Access);

      case Error_Code is
         when DestinyNoRoomInDestination =>
            raise Out_Of_Space;

         when DestinyItemActionForbidden =>
            raise Actions_Disallowed;

         when DestinyItemNotFound =>
            raise Item_Not_Found;

         when DestinyItemNotTransferrable =>
            raise Cannot_Transfer;

         when others =>
            raise Unknown_Error;
      end case;
   end Server_Check;
   --  Local Checks
   --  These return no value and raise an exception if the check fails

   procedure Check_Character_Has_Room
     (Inventory : Inventories.Character.Character_Inventory_Type;
      Character : Profiles.Character_Type;
      M         : Manifest.Manifest_Type;
      D         : Manifest.Tools.Item_Description)
   is

      Bucket_Item_Count : constant Natural :=
        Inventories.Character.Item_Count
          (Inventory, Character, D.Default_Bucket_Location);
      Max_Item_Count : constant Integer_32 :=
        M.Destiny_Inventory_Buckets (D.Default_Bucket_Hash).Item_Count;

   begin
      --  +2 because space is needed for the equipped item and the new item
      if Integer_32 (Bucket_Item_Count + 2) > Max_Item_Count then
         raise Out_Of_Space;
      end if;
   end Check_Character_Has_Room;

   procedure Check_Vault_Has_Room
     (Inventory : Inventories.Global.Global_Inventory_Type;
      M         : Manifest.Manifest_Type;
      D         : Manifest.Tools.Item_Description)
   is

      Bucket_Item_Count : constant Natural :=
        Inventories.Global.Item_Count (Inventory, D.Bucket_Location);
      Max_Item_Count : constant Integer_32 :=
        M.Destiny_Inventory_Buckets (General'Enum_Rep).Item_Count;
      Item_Stack_Quantity : constant Integer_32 :=
        Inventories.Global.Get_Item_Stack (Inventory, D.Item_Hash).Quantity;

   begin
      --  +1 because space is needed for the new item
      if Integer_32 (Bucket_Item_Count + 1) > Max_Item_Count then
         raise Out_Of_Space;
      end if;
      --  Check if adding this item would overflow a stack in the vault This
      --  only occurs for non-transferrable items

      if D.Transfer_Status /= Can_Transfer then
         begin
            if D.Quantity + Item_Stack_Quantity > D.Max_Stack_Size then
               raise Out_Of_Space;
            end if;
         exception
            when Inventories.Item_Not_Found =>
               null;
         end;
      end if;
   end Check_Vault_Has_Room;

   procedure Check_Actions_Permitted (D : Manifest.Tools.Item_Description) is
   begin
      if not D.Allow_Actions then
         raise Actions_Disallowed;
      end if;
   end Check_Actions_Permitted;
   --  The below subprograms perform both local and remote checks See the
   --  specification for more information

   procedure Vault
     (Vault_Inventory : Inventories.Global.Global_Inventory_Type;
      M               : Manifest.Manifest_Type;
      D               : Manifest.Tools.Item_Description;
      Source          : Profiles.Character_Type)
   is

      Data : Response.Data;

   begin
      Debug.Put_Line ("Vault item");
      --  Local Check
      --  An exception will be raised if any of these fail

      Check_Actions_Permitted (D);
      --  Check_Item_Belongs_Elsewhere

      case D.Bucket_Location is
         when Consumable | Modification =>
            raise Already_Here;

         when others =>
            null;
      end case;
      Check_Vault_Has_Room (Vault_Inventory, M, D);
      --  Check_Item_Not_Vaulted

      case D.Location is
         when Manifest.Vault =>
            raise Already_Here;

         when others =>
            null;
      end case;
      --  Check_Item_Can_Transfer

      case D.Transfer_Status is
         when Can_Transfer =>
            null;

         when others =>
            case D.Bucket_Location is
               when Postmaster =>
                  null;

               when others =>
                  raise Cannot_Transfer;
            end case;
      end case;
      Data :=
        Client.Post
          (URL =>
             Bungie_Root & API_Root & "/Destiny2/Actions/Items/TransferItem/",
           Data =>
             "{" & '"' & "itemReferenceHash" & '"' & ':' & D.Item_Hash'Image &
             ',' & '"' & "stackSize" & '"' & ':' & D.Quantity'Image & ',' &
             '"' & "transferToVault" & '"' & ": true," & '"' & "itemId" & '"' &
             ':' & ' ' & (+D.Item_Instance_ID) & ',' & '"' & "characterId" &
             '"' & ':' & ' ' & (+Source.Character_ID) & ',' & '"' &
             "membershipType" & '"' & ':' &
             Memberships.Find_Default_Platform_ID (GUI.Membership) & "}",
           Headers => GUI.Headers);
      Server_Check (Data);
   end Vault;

   procedure Unvault
     (Character_Inventory : Inventories.Character.Character_Inventory_Type;
      M                   : Manifest.Manifest_Type;
      D                   : Manifest.Tools.Item_Description;
      Target              : Profiles.Character_Type)
   is

      Data : Response.Data;

   begin
      Debug.Put_Line ("Unvault item");
      --  Local Check
      Check_Actions_Permitted (D);
      --  Check_Item_Belongs_Elsewhere

      case D.Bucket_Location is
         when Consumable | Modification =>
            raise Already_Here;

         when others =>
            null;
      end case;
      Check_Character_Has_Room (Character_Inventory, Target, M, D);
      Data :=
        Client.Post
          (URL =>
             Bungie_Root & API_Root & "/Destiny2/Actions/Items/TransferItem/",
           Data =>
             "{" & '"' & "itemReferenceHash" & '"' & ':' & D.Item_Hash'Image &
             ',' & '"' & "stackSize" & '"' & ':' & D.Quantity'Image & ',' &
             '"' & "transferToVault" & '"' & ": false," & '"' & "itemId" &
             '"' & ':' & ' ' & (+D.Item_Instance_ID) & ',' & '"' &
             "characterId" & '"' & ':' & ' ' & (+Target.Character_ID) & ',' &
             '"' & "membershipType" & '"' & ':' &
             Memberships.Find_Default_Platform_ID (GUI.Membership) & "}",
           Headers => GUI.Headers);
      Server_Check (Data);
   end Unvault;

   procedure Transfer
     (Vault_Inventory     : Inventories.Global.Global_Inventory_Type;
      Character_Inventory : Inventories.Character.Character_Inventory_Type;
      M                   : Manifest.Manifest_Type;
      D                   : Manifest.Tools.Item_Description;
      Source, Target      : Profiles.Character_Type)
   is
   begin
      --  Local Check
      --  (No Additonal Checks)

      Vault (Vault_Inventory, M, D, Source);
      delay 0.1; -- Throttle timer
      Unvault (Character_Inventory, M, D, Target);
   end Transfer;

   procedure Postmaster_Pull
     (Vault_Inventory     : Inventories.Global.Global_Inventory_Type;
      Character_Inventory : Inventories.Character.Character_Inventory_Type;
      M                   : Manifest.Manifest_Type;
      D                   : Manifest.Tools.Item_Description;
      Source              : Profiles.Character_Type)
   is

      Data : Response.Data;

   begin
      Debug.Put_Line ("Pull from Postmaster");
      --  Local Check
      --  An exception will be raised if any of these fail
      Check_Actions_Permitted (D);
      --  The item will end up in the vault in this case

      if D.Transfer_Status /= Can_Transfer then
         Check_Vault_Has_Room (Vault_Inventory, M, D);

      else
         Check_Character_Has_Room (Character_Inventory, Source, M, D);
      end if;
      Data :=
        Client.Post
          (URL =>
             Bungie_Root & API_Root &
             "/Destiny2/Actions/Items/PullFromPostmaster/",
           Data =>
             "{" & '"' & "itemReferenceHash" & '"' & ':' & D.Item_Hash'Image &
             ',' & '"' & "stackSize" & '"' & ':' & D.Quantity'Image & ',' &
             '"' & "itemId" & '"' & ':' & ' ' & (+D.Item_Instance_ID) & ',' &
             '"' & "characterId" & '"' & ':' & ' ' & (+Source.Character_ID) &
             ',' & '"' & "membershipType" & '"' & ':' &
             Memberships.Find_Default_Platform_ID (GUI.Membership) & "}",
           Headers => GUI.Headers);
      Server_Check (Data);
   end Postmaster_Pull;

   procedure Equip
     (D : Manifest.Tools.Item_Description; Source : Profiles.Character_Type)
   is

      Data : Response.Data;

   begin
      Debug.Put_Line ("Equip item");
      --  Local Check
      --  An exception will be raised if any of these fail
      Check_Actions_Permitted (D);
      Data :=
        Client.Post
          (URL =>
             Bungie_Root & API_Root & "/Destiny2/Actions/Items/EquipItem/",
           Data =>
             "{" & '"' & "itemId" & '"' & ':' & ' ' & (+D.Item_Instance_ID) &
             ',' & '"' & "characterId" & '"' & ':' & ' ' &
             (+Source.Character_ID) & ',' & '"' & "membershipType" & '"' &
             ':' & Memberships.Find_Default_Platform_ID (GUI.Membership) & "}",
           Headers => GUI.Headers);
      Server_Check (Data);
   end Equip;

end API.Transfers;
