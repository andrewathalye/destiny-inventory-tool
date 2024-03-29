--  VSS
with VSS.JSON.Pull_Readers.Simple;
   use VSS.JSON.Pull_Readers;
   use VSS.JSON;

--  Local Packages
with Shared.Strings; use Shared.Strings;

with API.Definitions.Hashes; use API.Definitions.Hashes;

procedure API.Tasks.Profiles.Read_Loadout
  (Reader : in out JSON_Simple_Pull_Reader; Loadout : out Loadout_Type)
is
   --  Note: Position Reader at START_OBJECT
   function Read_Loadout_Item
     (Reader : in out JSON_Simple_Pull_Reader) return Loadout_Item_Type
   is

      Result : Loadout_Item_Type;

   begin
      Read_Next (Reader); -- "itemInstanceId"
      Read_Next (Reader);
      Result.Item_Instance_ID := VS2UB (String_Value (Reader));
      Read_Next (Reader); -- "plugItemHashes";
      Read_Next (Reader); -- START_ARRAY
      Read_Next (Reader); -- Number_Value / End_Array

      while Event_Kind (Reader) /= End_Array loop
         Result.Plug_Item_Hashes.Append
           (Destiny_Inventory_Item_Definition_Manifest_Hash
              (As_Integer (Number_Value (Reader))));
         Read_Next (Reader); -- End_Array / Number_Value
      end loop;
      Read_Next (Reader); -- End_Object

      return Result;
   end Read_Loadout_Item;
begin
   loop
      Read_Next (Reader);

      case Event_Kind (Reader) is
         when Key_Name =>
            if VS2S (Key_Name (Reader)) = "colorHash" then
               Read_Next (Reader);
               Loadout.Colour_Hash :=
                 Destiny_Loadout_Color_Definition_Manifest_Hash
                   (As_Integer (Number_Value (Reader)));

            elsif VS2S (Key_Name (Reader)) = "iconHash" then
               Read_Next (Reader);
               Loadout.Icon_Hash :=
                 Destiny_Loadout_Icon_Definition_Manifest_Hash
                   (As_Integer (Number_Value (Reader)));

            elsif VS2S (Key_Name (Reader)) = "nameHash" then
               Read_Next (Reader);
               Loadout.Name_Hash :=
                 Destiny_Loadout_Name_Definition_Manifest_Hash
                   (As_Integer (Number_Value (Reader)));

            elsif VS2S (Key_Name (Reader)) = "items" then
               Read_Next (Reader); -- START_ARRAY
               Read_Next (Reader); -- START_OBJECT

               while Event_Kind (Reader) /= End_Array loop
                  Loadout.Items.Append (Read_Loadout_Item (Reader));
                  Read_Next (Reader); -- START_OBJECT / END_ARRAY
               end loop;

            else
               raise Program_Error;
            end if;

         when End_Object =>
            exit;

         when others =>
            raise Program_Error;
      end case;
   end loop;
end API.Tasks.Profiles.Read_Loadout;
