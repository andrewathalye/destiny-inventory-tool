pragma Ada_2022;

--  VSS
with VSS.Text_Streams.Memory_UTF8_Input;
use VSS.Text_Streams.Memory_UTF8_Input;
use VSS.Text_Streams;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
use VSS.JSON.Pull_Readers;
use VSS.JSON;
with VSS.Stream_Element_Vectors.Conversions;
use VSS.Stream_Element_Vectors.Conversions;

--  Local Packages
with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared;
with Tasks.Download;

with API.Profiles.Read_Item_Components;

package body API.Profiles is
   --  Note: Position Reader before "data"
   procedure Read_Characters
     (Reader : in out JSON_Simple_Pull_Reader;
      List   :    out Character_List) is separate;

   --  Note: Position Reader at START_OBJECT
   function Read_Item
     (Reader : in out JSON_Simple_Pull_Reader) return Item_Type is separate;

   --  Note: Position Reader before "items"
   procedure Read_Inventory
     (Reader : in out JSON_Simple_Pull_Reader; Inventory : out Item_List)
   is
   begin
      Wait_Until_Key (Reader, "items");
      Read_Next (Reader);

      case Event_Kind (Reader) is
         when Start_Array =>
            null;

         when others =>
            raise Program_Error;
      end case;
      Read_Items :
         loop
            Read_Next (Reader);

            case Event_Kind (Reader) is
               when Start_Object =>
                  Inventory.Append (Read_Item (Reader));
               when End_Array =>
                  exit Read_Items;

               when others =>
                  raise Program_Error;
            end case;
         end loop Read_Items;

   end Read_Inventory;

   --  Note: Position Reader before START_OBJECT
   procedure Read_Silver
     (Reader : in out JSON_Simple_Pull_Reader;
      Result :    out Profile_Type) is separate;

   --  Note: Position Reader at START_OBJECT
   procedure Read_Loadout
     (Reader  : in out JSON_Simple_Pull_Reader;
      Loadout :    out Loadout_Type) is separate;

   function Get_Profile (M : Membership_Type) return Profile_Type is

      Result : Profile_Type;
      Stream : Memory_UTF8_Input_Stream_Access := new Memory_UTF8_Input_Stream;
      Reader : JSON_Simple_Pull_Reader;

   begin
      Debug.Put_Line ("Get profiles");

      --  Get live profile
      Set_Data
        (Stream.all,
         To_Stream_Element_Vector
           (Tasks.Download.Download
              (+
               (API_Root & "/Destiny2/" &
                Memberships.Find_Default_Platform_ID (M) & "/Profile/" &
                (+M.Primary_Membership_ID) & "/" &
                "?components=ProfileInventories,ProfileCurrencies,PlatformSilver,Characters,CharacterInventories,CharacterProgressions,CharacterEquipment,CharacterLoadouts,ItemInstances,ItemStats,ItemSockets,ItemPlugObjectives,ItemPerks"),
               Needs_Auth => True,
               Caching    => Debug_Caching)));
      Set_Stream (Reader, Input_Text_Stream_Access (Stream));

      --  Profile Inventory
      Wait_Until_Key (Reader, "profileInventory");
      Read_Inventory (Reader, Result.Profile_Inventory);
      Debug.Put_Line ("Done reading profile inventory");

      --  Profile Currencies
      Wait_Until_Key (Reader, "profileCurrencies");
      Read_Inventory (Reader, Result.Profile_Currencies);
      Debug.Put_Line ("Done reading currencies");

      --  Silver
      Wait_Until_Key (Reader, "platformSilver");
      Read_Silver (Reader, Result);

      Debug.Put_Line ("Done reading platform silver");

      --  Characters
      Wait_Until_Key (Reader, "characters");
      Read_Characters (Reader, Result.Characters);

      ---------------------------
      -- CHARACTER INVENTORIES --
      ---------------------------

      Wait_Until_Key (Reader, "characterInventories");
      Wait_Until_Key (Reader, "data");
      Read_Next (Reader); -- START_OBJECT

      while Event_Kind (Reader) /= End_Object loop
         declare

            Character_ID : Unbounded_String;
            Inventory    : Item_List;

         begin
            Read_Next (Reader); -- KEY_NAME
            Character_ID := VS2UB (Key_Name (Reader));
            Read_Inventory (Reader, Inventory);
            Result.Character_Inventories.Insert (Character_ID, Inventory);
            Read_Next (Reader); -- END_OBJECT
            Read_Next (Reader); -- START_OBJECT / END_OBJECT
         end;
      end loop;

      Debug.Put_Line ("Done reading character inventories");

      ------------------------
      -- CHARACTER LOADOUTS --
      ------------------------

      Wait_Until_Key (Reader, "characterLoadouts");
      Wait_Until_Key (Reader, "data");
      Read_Next (Reader); -- START_OBJECT
      Read_Next (Reader); -- Character ID / END_OBJECT

      --  TODO May need to be realigned as per above (might skip objects)

      while Event_Kind (Reader) /= End_Object loop
         declare

            Character_ID : Unbounded_String;
            Loadouts     : Loadout_List;
            Loadout      : Loadout_Type;

         begin
            Character_ID := VS2UB (String_Value (Reader));
            Read_Next (Reader); -- START_OBJECT
            Read_Next (Reader); -- "loadouts"
            Read_Next (Reader); -- START_ARRAY
            Read_Next (Reader); -- START_OBJECT

            while Event_Kind (Reader) /= End_Array loop
               Read_Loadout (Reader, Loadout);
               Loadouts.Append (Loadout);
               Read_Next (Reader); -- START_OBJECT / END_ARRAY
            end loop;
            Result.Character_Loadouts.Insert (Character_ID, Loadouts);
            Read_Next (Reader); -- Character ID / END_OBJECT
         end;
      end loop;

      Debug.Put_Line ("Done reading character loadouts");

      -------------------------
      -- CHARACTER EQUIPMENT --
      -------------------------

      Wait_Until_Key (Reader, "characterEquipment");
      Wait_Until_Key (Reader, "data");
      Read_Next (Reader); -- START_OBJECT

      while Event_Kind (Reader) /= End_Object loop
         declare

            Character_ID : Unbounded_String;
            Inventory    : Item_List;

         begin
            Read_Next (Reader); -- KEY_NAME
            Character_ID := VS2UB (Key_Name (Reader));
            Read_Inventory (Reader, Inventory);
            Result.Character_Equipment.Insert (Character_ID, Inventory);
            Read_Next (Reader); -- END_OBJECT
            Read_Next (Reader); -- START_OBJECT / END_OBJECT
         end;
      end loop;

      Debug.Put_Line ("Done reading character equipment");

      ---------------------
      -- ITEM COMPONENTS --
      ---------------------

      Wait_Until_Key (Reader, "itemComponents");
      Read_Item_Components (Reader, Result.Item_Components);

      Debug.Put_Line ("All item components loaded");

      Free (Stream);
      return Result;
   end Get_Profile;

end API.Profiles;
