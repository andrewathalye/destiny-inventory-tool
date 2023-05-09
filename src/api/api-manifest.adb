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
   --  Note Seek to "DestinyClassDefinition"
   procedure Read_Classes
     (Reader  : in out JSON_Simple_Pull_Reader;
      Classes :    out Destiny_Class_Map) is separate;

   --  Note Seek to "DestinyGenderDefinition"
   procedure Read_Genders
     (Reader  : in out JSON_Simple_Pull_Reader;
      Genders :    out Destiny_Gender_Map) is separate;

   --  Note Seek to "DestinyInventoryBucketDefinition"
   procedure Read_Inventory_Buckets
     (Reader  : in out JSON_Simple_Pull_Reader;
      Buckets :    out Destiny_Inventory_Bucket_Map) is separate;

   --  Note Seek to "DestinyRaceDefinition"
   procedure Read_Races
     (Reader : in out JSON_Simple_Pull_Reader;
      Races  :    out Destiny_Race_Map) is separate;

   --  Note Seek to "DestinyDamageTypeDefinition"
   procedure Read_Damage_Types
     (Reader       : in out JSON_Simple_Pull_Reader;
      Damage_Types :    out Destiny_Damage_Type_Map) is separate;

   --  Note Seek to "DestinyStatDefinition"
   procedure Read_Stats
     (Reader : in out JSON_Simple_Pull_Reader;
      Stats  :    out Destiny_Stat_Map) is separate;

   --  Note Seek to "DestinyInventoryItemDefinition"
   procedure Read_Inventory_Items
     (Reader : in out JSON_Simple_Pull_Reader;
      Items  :    out Destiny_Inventory_Item_Map) is separate;

   --  Note: Position at "DestinyObjectiveDefinition"
   procedure Read_Objectives
     (Reader     : in out JSON_Simple_Pull_Reader;
      Objectives :    out Destiny_Objective_Map) is separate;

   --  Note Seek to "DestinyRecordDefinition"
   procedure Read_Titles
     (Reader : in out JSON_Simple_Pull_Reader;
      Titles :    out Destiny_Title_Map) is separate;

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

      Wait_Until_Key (Reader, "DestinyStatDefinition");
      Read_Stats (Reader, Result.Destiny_Stats);
      Debug.Put_Line ("stats read");

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
        (Reader, "it"); -- +M.Bungie_Net_User.Locale); -- TODO Needs to change
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

            --  Read raw manifest data after format check
            if Manifest_Format_Version = Current_Manifest_Format_Version and
              Manifest_Version = Localised_Manifest_Path
            then
               Manifest_Type'Read (S, Result);
               Close (SF);

               return Result;
            else
               Close (SF);
               Debug.Put_Line ("Update manifest");
               Delete_File ("dat/manifest.dat");
               return Fetch_Manifest (Localised_Manifest_Path);
            end if;
         end;

      else
         return Fetch_Manifest (Localised_Manifest_Path);
      end if;
   end Get_Manifest;

end API.Manifest;
