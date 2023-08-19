pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

--  AWS
with AWS.Client;
with AWS.Response;
use AWS;

--  GNATCOLL
with GNATCOLL.JSON; use GNATCOLL.JSON;

--  Local Packages
with API.Memberships;

with API.Profiles;
use all type API.Profiles.Transfer_Status_Type;

with API.Manifest.Tools;
use all type API.Manifest.Tools.Bucket_Location_Type;

with API.Definitions.Destiny_Inventory_Item;
use all type API.Definitions.Destiny_Inventory_Item.Destiny_Tier_Type;
use all type API.Definitions.Destiny_Inventory_Item.Destiny_Item_Type;
with API.Definitions.Destiny_Inventory_Bucket;
use all type API.Definitions.Destiny_Inventory_Bucket.Item_Location_Type;

use type API.Definitions.Quantity_Type;

with API.Error_Codes;
use all type API.Error_Codes.Error_Code_Type;

with API.Constants; use API.Constants;

with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared;

package body API.Transfers is
   --  Server Check
   procedure Server_Check (Data : AWS.Response.Data) is

      JSON_Data : constant JSON_Value :=
        Read
          (Strm     => String'(Response.Message_Body (Data)),
           Filename => "<inventory_transfer_data>");
      Error_Code : Error_Codes.Error_Code_Type;

   begin
      Error_Code :=
        Error_Codes.Error_Code_Type'Enum_Val
          (Integer'(JSON_Data.Get ("ErrorCode")));

      if Error_Code = Success then
         return;
      end if;

      Put_Line
        (Standard_Error,
         "[Error] API.Transfers got " & Error_Code'Image &
         " after passing all local checks");

      case Error_Code is
         when DestinyNoRoomInDestination    |
           DestinyItemActionForbidden       |
           DestinyItemNotFound              |
           DestinyItemUniqueEquipRestricted |
           DestinyItemNotTransferrable      =>
            raise Desynchronised with Error_Code'Image;
         when SystemDisabled =>
            raise System_Disabled;
         when DestinyCannotPerformActionAtThisLocation =>
            raise Cannot_Perform_Action_At_This_Location;

         when others =>
            raise Unknown_Error with Error_Code'Image;
      end case;
   end Server_Check;

   --  Local Checks
   --  These return no value and raise an exception if the check fails
   procedure Check_Character_Has_Room
     (Inventory : Inventories.Character.Character_Inventory_Type;
      M         : Manifest.Manifest_Type;
      D         : Manifest.Tools.Item_Description)
   is

      Bucket_Item_Count : constant API.Definitions.Quantity_Type :=
        Inventory.Item_Count (D.Default_Bucket_Location);
      Max_Item_Count : constant API.Definitions.Quantity_Type :=
        M.Destiny_Inventory_Buckets (D.Default_Bucket_Hash).Item_Count;

   begin
      --  +2 because space is needed for the equipped item and the new item
      if (Bucket_Item_Count + 2) > Max_Item_Count then
         raise No_Room_In_Destination;
      end if;
   end Check_Character_Has_Room;

   procedure Check_Vault_Has_Room
     (Inventory : Inventories.Global.Global_Inventory_Type;
      M         : Manifest.Manifest_Type;
      D         : Manifest.Tools.Item_Description)
   is

      Bucket_Item_Count : constant API.Definitions.Quantity_Type :=
        Inventories.Global.Item_Count (Inventory, D.Bucket_Location);
      Max_Item_Count : constant API.Definitions.Quantity_Type :=
        M.Destiny_Inventory_Buckets (General'Enum_Rep).Item_Count;
      Item_Stack_Quantity : API.Definitions.Quantity_Type := 0;

   begin
      --  Attempt to find the item stack quantity. There is not necessarily an item in the vault
      --  with the same hash, so this could fail.
      begin
         Item_Stack_Quantity := Inventory.Get (D.Item_Hash).Quantity;
      exception
         when Inventories.Item_Not_Found =>
            null;
      end;

      --  +1 because space is needed for the new item
      if (Bucket_Item_Count + 1) > Max_Item_Count then
         raise No_Room_In_Destination;
      end if;

      --  Check if adding this item would overflow a stack in the vault. This
      --  only occurs for non-transferrable items
      if D.Transfer_Status /= Can_Transfer then
         if D.Quantity + Item_Stack_Quantity > D.Max_Stack_Size then
            raise No_Room_In_Destination;
         end if;
      end if;
   end Check_Vault_Has_Room;

   procedure Check_Actions_Permitted (D : Manifest.Tools.Item_Description) is
   begin
      if not D.Allow_Actions then
         raise Item_Action_Forbidden;
      end if;
   end Check_Actions_Permitted;

   procedure Check_One_Exotic
     (Inventory : Inventories.Character.Character_Inventory_Type;
      D         : Manifest.Tools.Item_Description)
   is
      procedure Raise_Exception (Predicate : Boolean) is
      begin
         if Predicate then
            raise Item_Unique_Equip_Restricted;
         end if;
      end Raise_Exception;
      pragma Inline (Raise_Exception);

      Equipped :
        Inventories.Item_Description_Bucket_Location_Type_Array renames
        Inventory.Get_Equipped;
   begin
      case D.Tier_Type is
         when Exotic =>
            case D.Item_Type is
               when Weapon =>
                  Raise_Exception
                    (Equipped (Kinetic).Tier_Type = Exotic and
                     D.Bucket_Location /= Kinetic);
                  Raise_Exception
                    (Equipped (Energy).Tier_Type = Exotic and
                     D.Bucket_Location /= Energy);
                  Raise_Exception
                    (Equipped (Power).Tier_Type = Exotic and
                     D.Bucket_Location /= Power);

               when Armour =>
                  Raise_Exception
                    (Equipped (Helmet).Tier_Type = Exotic and
                     D.Bucket_Location /= Helmet);
                  Raise_Exception
                    (Equipped (Gauntlets).Tier_Type = Exotic and
                     D.Bucket_Location /= Gauntlets);
                  Raise_Exception
                    (Equipped (Chest).Tier_Type = Exotic and
                     D.Bucket_Location /= Chest);
                  Raise_Exception
                    (Equipped (Leg).Tier_Type = Exotic and
                     D.Bucket_Location /= Leg);
                  --  Au cas où
                  Raise_Exception
                    (Equipped (Class).Tier_Type = Exotic and
                     D.Bucket_Location /= Class);
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
   end Check_One_Exotic;

   --  The below subprograms perform both local and remote checks See the
   --  specification for more information
   procedure Vault
     (Auth            : API.Identification.Auth_Type;
      Vault_Inventory : Inventories.Global.Global_Inventory_Type;
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
            raise Item_Already_Here;

         when others =>
            null;
      end case;

      Check_Vault_Has_Room (Vault_Inventory, M, D);

      --  Check_Item_Not_Vaulted
      case D.Location is
         when Vault =>
            raise Item_Already_Here;

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
                  raise Item_Not_Transferrable;
            end case;
      end case;

      Data :=
        Client.Post
          (URL  => API_Root & "/Destiny2/Actions/Items/TransferItem/",
           Data =>
             "{" & '"' & "itemReferenceHash" & '"' & ':' & D.Item_Hash'Image &
             ',' & '"' & "stackSize" & '"' & ':' & D.Quantity'Image & ',' &
             '"' & "transferToVault" & '"' & ": true," & '"' & "itemId" & '"' &
             ':' & D.Item_Instance_ID'Image & ',' & '"' & "characterId" & '"' &
             ':' & ' ' & (+Source.Character_ID) & ',' & '"' &
             "membershipType" & '"' & ':' &
             Memberships.Find_Default_Platform_ID (Auth.Membership) & "}",
           Headers => Auth.Headers);
      Server_Check (Data);
   end Vault;

   procedure Unvault
     (Auth                : API.Identification.Auth_Type;
      Character_Inventory : Inventories.Character.Character_Inventory_Type;
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
      case D.Default_Bucket_Location is
         when Consumable | Modification =>
            raise Item_Already_Here;

         when others =>
            null;
      end case;

      Check_Character_Has_Room (Character_Inventory, M, D);

      Data :=
        Client.Post
          (URL  => API_Root & "/Destiny2/Actions/Items/TransferItem/",
           Data =>
             "{" & '"' & "itemReferenceHash" & '"' & ':' & D.Item_Hash'Image &
             ',' & '"' & "stackSize" & '"' & ':' & D.Quantity'Image & ',' &
             '"' & "transferToVault" & '"' & ": false," & '"' & "itemId" &
             '"' & ':' & D.Item_Instance_ID'Image & ',' & '"' & "characterId" &
             '"' & ':' & ' ' & (+Target.Character_ID) & ',' & '"' &
             "membershipType" & '"' & ':' &
             Memberships.Find_Default_Platform_ID (Auth.Membership) & "}",
           Headers => Auth.Headers);
      Server_Check (Data);
   end Unvault;

   procedure Transfer
     (Auth             : API.Identification.Auth_Type;
      Vault_Inventory  : Inventories.Global.Global_Inventory_Type;
      Target_Inventory : Inventories.Character.Character_Inventory_Type;
      M                : Manifest.Manifest_Type;
      D                : Manifest.Tools.Item_Description;
      Source, Target   : Profiles.Character_Type)
   is
   begin
      --  Local Check
      --  (No Additonal Checks)

      Vault (Auth, Vault_Inventory, M, D, Source);
      delay 0.1; -- Throttle timer
      Unvault (Auth, Target_Inventory, M, D, Target);
   end Transfer;

   procedure Postmaster_Pull
     (Auth                : API.Identification.Auth_Type;
      Vault_Inventory     : Inventories.Global.Global_Inventory_Type;
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
         Check_Character_Has_Room (Character_Inventory, M, D);
      end if;

      Data :=
        Client.Post
          (URL  => API_Root & "/Destiny2/Actions/Items/PullFromPostmaster/",
           Data =>
             "{" & '"' & "itemReferenceHash" & '"' & ':' & D.Item_Hash'Image &
             ',' & '"' & "stackSize" & '"' & ':' & D.Quantity'Image & ',' &
             '"' & "itemId" & '"' & ':' & D.Item_Instance_ID'Image & ',' &
             '"' & "characterId" & '"' & ':' & ' ' & (+Source.Character_ID) &
             ',' & '"' & "membershipType" & '"' & ':' &
             Memberships.Find_Default_Platform_ID (Auth.Membership) & "}",
           Headers => Auth.Headers);
      Server_Check (Data);
   end Postmaster_Pull;

   procedure Equip
     (Auth      : API.Identification.Auth_Type;
      Inventory : Inventories.Character.Character_Inventory_Type;
      D         : Manifest.Tools.Item_Description;
      Source    : Profiles.Character_Type)
   is

      Data : Response.Data;

   begin
      Debug.Put_Line ("Equip item");

      --  Local Check
      --  An exception will be raised if any of these fail
      Check_Actions_Permitted (D);

      Check_One_Exotic (Inventory, D);

      Data :=
        Client.Post
          (URL  => API_Root & "/Destiny2/Actions/Items/EquipItem/",
           Data =>
             "{" & '"' & "itemId" & '"' & ':' & D.Item_Instance_ID'Image &
             ',' & '"' & "characterId" & '"' & ':' & ' ' &
             (+Source.Character_ID) & ',' & '"' & "membershipType" & '"' &
             ':' & Memberships.Find_Default_Platform_ID (Auth.Membership) &
             "}",
           Headers => Auth.Headers);
      Server_Check (Data);
   end Equip;

end API.Transfers;
