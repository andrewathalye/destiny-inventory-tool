pragma Ada_2022;
with Ada.Streams.Stream_IO;
with Ada.Directories; use Ada.Directories;

--  VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with VSS.Text_Streams;             use VSS.Text_Streams;
with VSS.Text_Streams.Memory_UTF8_Input;
use VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Stream_Element_Vectors.Conversions;
use VSS.Stream_Element_Vectors.Conversions;
use VSS.JSON.Pull_Readers;
use VSS.JSON;

--  Local Packages
with Shared.JSON;    use Shared.JSON;
with Shared.Debug;
with Shared.Strings; use Shared.Strings;
use Shared;
with Tasks.Download;

package body API.Manifest is
   --  Note Seek to "DestinyGenderDefinition"
   procedure Read_Genders
     (Reader  : in out JSON_Simple_Pull_Reader;
      Genders :    out Destiny_Gender_Map)
   is

      Gender : Destiny_Gender_Definition;

   begin
      Read_Next (Reader); -- START_OBJECT

      while Event_Kind (Reader) /= End_Object loop
         Wait_Until_Key (Reader, "genderType");
         Read_Next (Reader);
         Gender.Gender_Type :=
           Destiny_Gender_Type'Enum_Val (As_Integer (Number_Value (Reader)));
         Wait_Until_Key (Reader, "name");
         Read_Next (Reader);
         Gender.Gender_Name := VS2UB (String_Value (Reader));
         Wait_Until_Key (Reader, "hash");
         Read_Next (Reader);
         Genders.Insert
           (Manifest_Hash (As_Integer (Number_Value (Reader))), Gender);
         Wait_Until_Event (Reader, End_Object);
         Read_Next (Reader);
      end loop;
   end Read_Genders;

   --  Note Seek to "DestinyRaceDefinition"
   procedure Read_Races
     (Reader : in out JSON_Simple_Pull_Reader; Races : out Destiny_Race_Map)
   is

      Race : Destiny_Race_Name;

   begin
      Read_Next (Reader); -- START_OBJECT

      while Event_Kind (Reader) /= End_Object loop
         Wait_Until_Key (Reader, "genderedRaceNames");
         Read_Next (Reader); -- START_OBJECT

         Read_Next (Reader); -- "Male"
         Read_Next (Reader);
         Race (Male) := VS2UB (String_Value (Reader));
         Read_Next (Reader); -- "Female"
         Read_Next (Reader);
         Race (Female) := VS2UB (String_Value (Reader));
         Wait_Until_Key (Reader, "hash");
         Read_Next (Reader);
         Races.Insert
           (Manifest_Hash (As_Integer (Number_Value (Reader))), Race);
         Wait_Until_Event (Reader, End_Object);
         Read_Next (Reader);
      end loop;
   end Read_Races;

   --  Note Seek to "DestinyClassDefinition"
   procedure Read_Classes
     (Reader : in out JSON_Simple_Pull_Reader; Classes : out Destiny_Class_Map)
   is

      Class : Destiny_Class_Name;

   begin
      Read_Next (Reader); -- START_OBJECT

      while Event_Kind (Reader) /= End_Object loop
         Wait_Until_Key (Reader, "genderedClassNames");
         Read_Next (Reader); -- START_OBJECT

         Read_Next (Reader); -- "Male"
         Read_Next (Reader);
         Class (Male) := VS2UB (String_Value (Reader));
         Read_Next (Reader); -- "Female"
         Read_Next (Reader);
         Class (Female) := VS2UB (String_Value (Reader));
         Wait_Until_Key (Reader, "hash");
         Read_Next (Reader);
         Classes.Insert
           (Manifest_Hash (As_Integer (Number_Value (Reader))), Class);
         Wait_Until_Event (Reader, End_Object);
         Read_Next (Reader);
      end loop;
   end Read_Classes;

   --  Note Seek to "DestinyRecordDefinition"
   procedure Read_Titles
     (Reader : in out JSON_Simple_Pull_Reader; Titles : out Destiny_Title_Map)
   is

      Title : Destiny_Title_Name;

   begin
      Read_Next (Reader); -- START_OBJECT

      while Event_Kind (Reader) /= End_Object loop
         Wait_Until_Key (Reader, "titleInfo");
         Read_Next (Reader); -- START_OBJECT

         Read_Next (Reader); -- "hasTitle"
         Read_Next (Reader);

         if Boolean_Value (Reader) then
            Read_Next (Reader); -- "titlesByGender"
            Read_Next (Reader); -- START_OBJECT

            Read_Next (Reader); -- "Male"
            Read_Next (Reader);
            Title (Male) := VS2UB (String_Value (Reader));
            Read_Next (Reader); -- "Female"
            Read_Next (Reader);
            Title (Female) := VS2UB (String_Value (Reader));
            Wait_Until_Key (Reader, "hash");
            Read_Next (Reader);
            Titles.Insert
              (Manifest_Hash (As_Integer (Number_Value (Reader))), Title);
         end if;
         Wait_Until_Key (Reader, "blacklisted");
         Wait_Until_Event (Reader, End_Object);
         Read_Next (Reader);
      end loop;
   end Read_Titles;

   --  Note Seek to "DestinyInventoryBucketDefinition"
   procedure Read_Inventory_Buckets
     (Reader  : in out JSON_Simple_Pull_Reader;
      Buckets :    out Destiny_Inventory_Bucket_Map)
   is

      Bucket : Destiny_Inventory_Bucket_Definition;

   begin
      Read_Next (Reader); -- START_OBJECT

      while Event_Kind (Reader) /= End_Object loop
         Bucket.Description := Null_Unbounded_String;
         Bucket.Name        := Null_Unbounded_String;
         Wait_Until_Key (Reader, "displayProperties");
         Read_Next (Reader); -- START_OBJECT
         Read_Next (Reader); -- "description" or "hasIcon"

         --  If the bucket has a name and description, add them

         if VS2S (Key_Name (Reader)) = "description" then
            Read_Next (Reader);
            Bucket.Description := VS2UB (String_Value (Reader));
            Read_Next (Reader); -- "name"
            Read_Next (Reader);
            Bucket.Name := VS2UB (String_Value (Reader));
         end if;
         Wait_Until_Key (Reader, "category");
         Read_Next (Reader);
         Bucket.Category :=
           Destiny_Inventory_Bucket_Category'Enum_Val
             (As_Integer (Number_Value (Reader)));
         Read_Next (Reader); -- "bucketOrder"
         Read_Next (Reader);
         Bucket.Bucket_Order :=
           Integer_32 (As_Integer (Number_Value (Reader)));
         Read_Next (Reader); -- "itemCount"
         Read_Next (Reader);
         Bucket.Item_Count := Integer_32 (As_Integer (Number_Value (Reader)));
         Read_Next (Reader); -- "location"
         Read_Next (Reader);
         Bucket.Location :=
           Item_Location_Type'Enum_Val (As_Integer (Number_Value (Reader)));
         Wait_Until_Key (Reader, "fifo");
         Read_Next (Reader);
         Bucket.FIFO := Boolean_Value (Reader);
         Wait_Until_Key (Reader, "hash");
         Read_Next (Reader);
         Buckets.Insert
           (Manifest_Hash (As_Integer (Number_Value (Reader))), Bucket);
         Wait_Until_Event (Reader, End_Object);
         Read_Next (Reader);
      end loop;
   end Read_Inventory_Buckets;

   --  Note Seek to "DestinyDamageTypeDefinition"
   procedure Read_Damage_Types
     (Reader       : in out JSON_Simple_Pull_Reader;
      Damage_Types :    out Destiny_Damage_Type_Map)
   is

      Damage_Type : Destiny_Damage_Type_Definition;

   begin
      Read_Next (Reader); -- START_OBJECT

      while Event_Kind (Reader) /= End_Object loop
         Damage_Type.Icon_Path := Null_Unbounded_String;
         Wait_Until_Key (Reader, "displayProperties");
         Read_Next (Reader); -- START_OBJECT

         Read_Next (Reader); -- "description"
         Read_Next (Reader);
         Damage_Type.Description := VS2UB (String_Value (Reader));
         Read_Next (Reader); -- "name"
         Read_Next (Reader);
         Damage_Type.Name := VS2UB (String_Value (Reader));
         Read_Next (Reader); -- "icon" or "hasIcon"

         if VS2S (Key_Name (Reader)) = "icon" then
            Read_Next (Reader);
            Damage_Type.Icon_Path := VS2UB (String_Value (Reader));
         end if;
         Wait_Until_Key (Reader, "showIcon");
         Read_Next (Reader);
         Damage_Type.Show_Icon := Boolean_Value (Reader);
         Wait_Until_Key (Reader, "hash");
         Read_Next (Reader);
         Damage_Types.Insert
           (Manifest_Hash (As_Integer (Number_Value (Reader))), Damage_Type);
         Wait_Until_Event (Reader, End_Object);
         Read_Next (Reader);
      end loop;
   end Read_Damage_Types;

   --  Note Seek to "DestinyInventoryItemDefinition"
   procedure Read_Inventory_Items
     (Reader : in out JSON_Simple_Pull_Reader;
      Items  :    out Destiny_Inventory_Item_Map)
   is

      Item : Destiny_Inventory_Item_Definition;

   begin
      Item.Icon_Path              := Null_Unbounded_String;
      Item.Watermark_Path         := Null_Unbounded_String;
      Item.Shelved_Watermark_Path := Null_Unbounded_String;
      Item.Bucket_Type_Hash       := 0;

      Read_Next (Reader); -- START_OBJECT

      while Event_Kind (Reader) /= End_Object loop
         --  Clear Lists in Surrogate Item
         Item.Display_Version_Watermark_Icons.Clear;
         Wait_Until_Key (Reader, "displayProperties");
         Read_Next (Reader); -- START_OBJECT

         Read_Next (Reader); -- "description"
         Read_Next (Reader);
         Item.Description := VS2UB (String_Value (Reader));
         Read_Next (Reader); -- "name"
         Read_Next (Reader);
         Item.Name := VS2UB (String_Value (Reader));
         Read_Next (Reader); -- "icon" or "hasIcon"

         if VS2S (Key_Name (Reader)) = "icon" then
            Read_Next (Reader);
            Item.Icon_Path := VS2UB (String_Value (Reader));
         end if;
         --  Read optional fields with variable locations
         Read_Variable_Fields :
            loop
               case Event_Kind (Reader) is
                  when Key_Name =>
                     if VS2S (Key_Name (Reader)) = "itemTypeAndTierDisplayName"
                     then
                        Read_Next (Reader);
                        Item.Item_Type_And_Tier_Display_Name :=
                          VS2UB (String_Value (Reader));

                     elsif VS2S (Key_Name (Reader)) = "maxStackSize" then
                        Read_Next (Reader);
                        Item.Max_Stack_Size :=
                          Integer_32 (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) = "bucketTypeHash" then
                        Read_Next (Reader);
                        Item.Bucket_Type_Hash :=
                          Manifest_Hash (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) = "tierType" then
                        Read_Next (Reader);
                        Item.Tier_Type :=
                          Destiny_Tier_Type'Enum_Val
                            (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) =
                       "doesPostmasterPullHaveSideEffects"
                     then
                        Read_Next (Reader);
                        Item.Postmaster_Pull_Has_Side_Effects :=
                          Boolean_Value (Reader);

                     elsif VS2S (Key_Name (Reader)) = "itemType" then
                        Read_Next (Reader);
                        Item.Item_Type :=
                          Destiny_Item_Type'Enum_Val
                            (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) = "allowActions" then
                        Read_Next (Reader);
                        Item.Allow_Actions := Boolean_Value (Reader);

                     elsif VS2S (Key_Name (Reader)) = "defaultDamageTypeHash"
                     then
                        Read_Next (Reader);
                        Item.Default_Damage_Type_Hash :=
                          Manifest_Hash (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) = "collectibleHash" then
                        Read_Next (Reader); -- value ignored TODO
                        Read_Next
                          (Reader); -- "iconWatermark", "iconWatermarkShelved", etc.

                     elsif VS2S (Key_Name (Reader)) = "iconWatermark" then
                        Read_Next (Reader);
                        Item.Watermark_Path := VS2UB (String_Value (Reader));
                        Read_Next (Reader); -- "iconWatermarkShelved", etc.

                     elsif VS2S (Key_Name (Reader)) = "iconWatermarkShelved"
                     then
                        Read_Next (Reader);
                        Item.Shelved_Watermark_Path :=
                          VS2UB (String_Value (Reader));
                        Read_Next (Reader); -- etc.

                     elsif VS2S (Key_Name (Reader)) =
                       "displayVersionWatermarkIcons"
                     then
                        Read_Next (Reader); -- START_ARRAY
                        Read_Next (Reader);

                        while Event_Kind (Reader) /= End_Array loop
                           Item.Display_Version_Watermark_Icons.Append
                             (VS2UB (String_Value (Reader)));
                           Read_Next (Reader); -- STRING_VALUE or END_ARRAY
                        end loop;

                     elsif VS2S (Key_Name (Reader)) = "hash" then
                        exit Read_Variable_Fields;

                     else
                        Read_Next (Reader);
                     end if;

                  when others =>
                     null;
                     Read_Next (Reader);
               end case;
            end loop Read_Variable_Fields;

            --  At "hash"
         Read_Next (Reader);
         Items.Insert
           (Manifest_Hash (As_Integer (Number_Value (Reader))), Item);
         Wait_Until_Event (Reader, End_Object);
         Read_Next (Reader);
      end loop;
   end Read_Inventory_Items;

   --  Note: Position at "DestinyObjectiveDefinition"
   procedure Read_Objectives
     (Reader     : in out JSON_Simple_Pull_Reader;
      Objectives :    out Destiny_Objective_Map)
   is
      Objective : Destiny_Objective_Definition;
   begin
      Read_Next (Reader); --  START_OBJECT
      Read_Next (Reader); --  HASH as key or END_OBJECT

      Objective_Loop :
         while Event_Kind (Reader) /= End_Object loop
            Read_Next (Reader); --  START_OBJECT

            --  "icon" is a nullable field. Parse it manually.
            Read_Next (Reader); --  "displayProperties"
            Read_Next (Reader); --  START_OBJECT
            Read_Next (Reader); --  "description"
            Read_Next (Reader);
            Read_Next (Reader); --  "name"
            Read_Next (Reader);
            Read_Next (Reader); --  "icon" or "hasIcon"

            if VS2S (Key_Name (Reader)) = "icon" then
               Read_Next (Reader);
               Objective.Icon_Path := VS2UB (String_Value (Reader));
            else
               Objective.Icon_Path := Null_Unbounded_String;
            end if;

            Wait_Until_Key (Reader, "progressDescription");
            Read_Next (Reader);
            Objective.Progress_Description := VS2UB (String_Value (Reader));

            Wait_Until_Key (Reader, "hash");
            Read_Next (Reader);
            Objectives.Insert
              (Manifest_Hash (As_Integer (Number_Value (Reader))), Objective);

            Read_Next (Reader); -- "index"
            Read_Next (Reader);

            Read_Next (Reader); -- "redacted"
            Read_Next (Reader);

            Read_Next (Reader); -- "blacklisted"
            Read_Next (Reader);

            Read_Next (Reader); -- END_OBJECT
            Read_Next (Reader); -- HASH as key or END_OBJECT
         end loop Objective_Loop;
   end Read_Objectives;

   function Fetch_Manifest
     (Localised_Manifest_Path : Unbounded_String) return Manifest_Type
   is

      Result : Manifest_Type;
      Stream : Memory_UTF8_Input_Stream_Access := new Memory_UTF8_Input_Stream;
      Reader : JSON_Simple_Pull_Reader;

   begin
      Debug.Put_Line ("Fetch and parse manifest");
      Set_Data
        (Stream.all,
         To_Stream_Element_Vector
           (Tasks.Download.Download
              (+(Bungie_Root & (+Localised_Manifest_Path)))));

      --  Read Fields from Manifest
      Set_Stream (Reader, Input_Text_Stream_Access (Stream));

      Wait_Until_Key (Reader, "DestinyClassDefinition");
      Read_Classes (Reader, Result.Destiny_Classes);
      Debug.Put_Line ("classes read");

      Wait_Until_Key (Reader, "DestinyGenderDefinition");
      Read_Genders (Reader, Result.Destiny_Genders);
      Debug.Put_Line ("genders read");

      Wait_Until_Key (Reader, "DestinyInventoryBucketDefinition");
      Read_Inventory_Buckets (Reader, Result.Destiny_Inventory_Buckets);
      Debug.Put_Line ("buckets read");

      Wait_Until_Key (Reader, "DestinyRaceDefinition");
      Read_Races (Reader, Result.Destiny_Races);
      Debug.Put_Line ("races read");

      Wait_Until_Key (Reader, "DestinyDamageTypeDefinition");
      Read_Damage_Types (Reader, Result.Destiny_Damage_Types);
      Debug.Put_Line ("damage types read");

      Wait_Until_Key (Reader, "DestinyInventoryItemDefinition");
      Read_Inventory_Items (Reader, Result.Destiny_Inventory_Items);
      Debug.Put_Line ("items read");

      Wait_Until_Key (Reader, "DestinyObjectiveDefinition");
      Read_Objectives (Reader, Result.Destiny_Objectives);
      Debug.Put_Line ("objectives read");

      Wait_Until_Key (Reader, "DestinyRecordDefinition");
      Read_Titles (Reader, Result.Destiny_Titles);
      Debug.Put_Line ("records read");

      Free (Stream);

      --  Cache manifest for later use
      declare

         use Ada.Streams.Stream_IO;
         SF : File_Type;
         S  : Stream_Access;

      begin
         Create (SF, Out_File, "dat/manifest.dat");
         S := Ada.Streams.Stream_IO.Stream (SF);

         --   Compatibility metadata
         Natural'Write (S, Current_Manifest_Format_Version);
         Unbounded_String'Write (S, Localised_Manifest_Path);

         --  Raw manifest record
         Manifest_Type'Write (S, Result);
         Close (SF);
      end;
      return Result;
   end Fetch_Manifest;

   function Get_Manifest return Manifest_Type is
      --  Storage

      Localised_Manifest_Path : Unbounded_String;
      --  Requests
      Stream : Memory_UTF8_Input_Stream_Access := new Memory_UTF8_Input_Stream;
      --  Parsing
      Reader : JSON_Simple_Pull_Reader;
      Result : Manifest_Type;

   begin
      Debug.Put_Line ("Get manifest");
      Set_Data
        (Stream.all,
         To_Stream_Element_Vector
           (Tasks.Download.Download
              (+(API_Root & "/Destiny2/Manifest/"),
               Caching => Debug_Caching)));
      Set_Stream (Reader, Input_Text_Stream_Access (Stream));
      Wait_Until_Key (Reader, "jsonWorldContentPaths");
      Wait_Until_Key
        (Reader, "en"); -- +M.Bungie_Net_User.Locale); -- TODO Needs to change
      Read_Next (Reader); -- STRING_VALUE

      Localised_Manifest_Path := VS2UB (String_Value (Reader));
      Free (Stream);

      if Exists ("dat/manifest.dat") then
         Debug.Put_Line ("Load preparsed manifest");

         declare

            use Ada.Streams.Stream_IO;
            SF                      : File_Type;
            S                       : Stream_Access;
            Manifest_Format_Version : Natural;
            Manifest_Version        : Unbounded_String;

         begin
            Open (SF, In_File, "dat/manifest.dat");
            S := Ada.Streams.Stream_IO.Stream (SF);

            --  Read compatibility metadata
            Natural'Read (S, Manifest_Format_Version);
            Unbounded_String'Read (S, Manifest_Version);

            --  Read raw manifest data
            Manifest_Type'Read (S, Result);
            Close (SF);

            if Manifest_Format_Version /= Current_Manifest_Format_Version or
              Manifest_Version /= Localised_Manifest_Path
            then
               Debug.Put_Line ("Update prepared manifest");
               Delete_File ("dat/manifest.dat");
               return Fetch_Manifest (Localised_Manifest_Path);
            end if;
            return Result;
         end;

      else
         return Fetch_Manifest (Localised_Manifest_Path);
      end if;
   end Get_Manifest;

end API.Manifest;
