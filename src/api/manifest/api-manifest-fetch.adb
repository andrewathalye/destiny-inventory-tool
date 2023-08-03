pragma Ada_2022;

with Ada.Directories; use Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Ada.Exceptions;  use Ada.Exceptions;

--  VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with VSS.Text_Streams;             use VSS.Text_Streams;
with VSS.Text_Streams.Memory_UTF8_Input;
use VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Stream_Element_Vectors.Conversions;
use VSS.Stream_Element_Vectors.Conversions;
use VSS.JSON.Pull_Readers;
use VSS.JSON;

--  libarchive binding
with libarchive; use libarchive;

--  GNATCOLL-db
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.SQL.Exec;

--  Local Packages
with Tasks.Download;

with Shared.Debug;
with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;
with Shared.Streams; use Shared.Streams;

--  Standalone Procedures: Processing callbacks
with API.Manifest.Class_Callback;
with API.Manifest.Gender_Callback;
with API.Manifest.Inventory_Bucket_Callback;
with API.Manifest.Race_Callback;
with API.Manifest.Damage_Type_Callback;
with API.Manifest.Stat_Callback;
with API.Manifest.Inventory_Item_Callback;
with API.Manifest.Objective_Callback;
with API.Manifest.Record_Callback;
with API.Manifest.Vendor_Callback;
with API.Manifest.Faction_Callback;
with API.Manifest.Destination_Callback;
with API.Manifest.Place_Callback;
with API.Manifest.Activity_Callback;

function API.Manifest.Fetch
  (Localised_Manifest_Path : Unbounded_String) return Manifest_Type
is
   --  Constants
   Database_Path : constant String := "dat/manifest.sqlite";

   --  Variables
   Result : Manifest_Type;

   --  Note: These need to be freed after we are done
   Manifest_Data : Stream_Element_Array_Access;
   Archive_Data  : Stream_Element_Array_Access;

begin
   Shared.Debug.Put_Line ("Fetch manifest");
   Manifest_Data :=
     new Ada.Streams.Stream_Element_Array'
       (Tasks.Download.Download (+(Bungie_Root & (+Localised_Manifest_Path))));

   Shared.Debug.Put_Line ("Decompress manifest");

   --  Manifest is in zipped Sqlite format
   --  Use libarchive to decompress
   declare
      Archive       : Archive_Access := Archive_Read_New;
      Archive_Entry : Archive_Entry_Access;
   begin
      Archive_Read_Support_Format_Zip (Archive);

      Archive_Read_Open_Memory
        (Archive, Manifest_Data.all'Address, Manifest_Data.all'Length);

      Archive_Entry := Archive_Read_Next_Header (Archive);

      --  Ensure that the entry actually has a size set
      if Archive_Entry_Size_Is_Set (Archive_Entry) then
         Archive_Data :=
           new Ada.Streams.Stream_Element_Array
             (1 ..
                  Ada.Streams.Stream_Element_Offset
                    (Archive_Entry_Size (Archive_Entry)));
      else
         raise Program_Error with "Manifest zip entry had unset size";
      end if;

      Archive_Read_Data
        (Archive, Archive_Data.all'Address, Archive_Data.all'Length);

      Archive_Read_Free (Archive);
   end;

   Free (Manifest_Data); --  Manifest unzipped

   Shared.Debug.Put_Line ("Load database and start parsing fields");

   Write_Temp_Database :
      declare
         SF : Ada.Streams.Stream_IO.File_Type;
         SE : Ada.Streams.Stream_IO.Stream_Access;
      begin
         --  If we crashed last time, remove the old database
         if Exists (Database_Path) then
            goto TODO_Skip;
            --  Delete_File (Database_Path);
         end if;

         Ada.Streams.Stream_IO.Create (File => SF, Name => Database_Path);
         SE := Ada.Streams.Stream_IO.Stream (SF);

         Ada.Streams.Stream_Element_Array'Write (SE, Archive_Data.all);

         Ada.Streams.Stream_IO.Close (SF);
      end Write_Temp_Database;

   <<TODO_Skip>>

   Free (Archive_Data); --  Archive data written

   Read_Database :
      declare
         Description : GNATCOLL.SQL.Exec.Database_Description;
         Connection  : GNATCOLL.SQL.Exec.Database_Connection;

         --  Nested subprogram to execute callbacks and fetch table data
         procedure Add_Data
           (Table    : String;
            Callback : access procedure
              (Hash         :        Manifest_Hash;
               Reader       : in out JSON_Simple_Pull_Reader;
               The_Manifest :    out Manifest_Type))
         is
            use GNATCOLL.SQL.Exec;
            --  Instantiations
            function Integer_To_Manifest_Hash is new Ada.Unchecked_Conversion
              (Integer, Manifest_Hash);

            --  Variables
            Cursor : GNATCOLL.SQL.Exec.Direct_Cursor;

            Stream : Memory_UTF8_Input_Stream_Access :=
              new Memory_UTF8_Input_Stream;
         begin
            --  Fetch full table
            GNATCOLL.SQL.Exec.Fetch
              (Cursor, Connection, "SELECT * FROM " & Table);

            --  Iterate over table
            while Cursor.Has_Row loop
               --  Create JSON stream
               declare
                  Reader : JSON_Simple_Pull_Reader;
               begin
                  Set_Data
                    (Stream.all,
                     Unchecked_From_Unbounded_String (+Cursor.Value (1)));
                  Set_Stream (Reader, Input_Text_Stream_Access (Stream));

                  Callback
                    (Hash =>
                       Integer_To_Manifest_Hash (Cursor.Integer_Value (0)),
                     Reader       => Reader,
                     The_Manifest => Result);
               exception
                  when X : others =>
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error, Cursor.Value (1));
                     Ada.Exceptions.Reraise_Occurrence (X);
               end;

               Cursor.Next;
            end loop;

            --  Cleanup
            Free (Stream);

            Shared.Debug.Put_Line ("Processed " & Table);
         end Add_Data;

      begin
         --  Connect to Database
         Description := GNATCOLL.SQL.Sqlite.Setup (Database => Database_Path);
         Connection  := GNATCOLL.SQL.Exec.Build_Connection (Description);

         --  Add data from tables using callbacks
         Add_Data
           ("DestinyClassDefinition", API.Manifest.Class_Callback'Access);
         Add_Data
           ("DestinyGenderDefinition", API.Manifest.Gender_Callback'Access);
         Add_Data
           ("DestinyInventoryBucketDefinition",
            API.Manifest.Inventory_Bucket_Callback'Access);
         Add_Data ("DestinyRaceDefinition", API.Manifest.Race_Callback'Access);
         Add_Data
           ("DestinyDamageTypeDefinition",
            API.Manifest.Damage_Type_Callback'Access);
         Add_Data ("DestinyStatDefinition", API.Manifest.Stat_Callback'Access);
         Add_Data
           ("DestinyInventoryItemDefinition",
            API.Manifest.Inventory_Item_Callback'Access);
         Add_Data
           ("DestinyObjectiveDefinition",
            API.Manifest.Objective_Callback'Access);
         Add_Data
           ("DestinyRecordDefinition", API.Manifest.Record_Callback'Access);
         Add_Data
           ("DestinyVendorDefinition", API.Manifest.Vendor_Callback'Access);
         Add_Data
           ("DestinyFactionDefinition", API.Manifest.Faction_Callback'Access);
         Add_Data
           ("DestinyDestinationDefinition",
            API.Manifest.Destination_Callback'Access);
         Add_Data
           ("DestinyPlaceDefinition", API.Manifest.Place_Callback'Access);
         Add_Data
           ("DestinyActivityDefinition",
            API.Manifest.Activity_Callback'Access);

         --  Done reading, free connection and description
         GNATCOLL.SQL.Exec.Free (Connection);
         GNATCOLL.SQL.Exec.Free (Description);

      end Read_Database;

   return Result;
   pragma Warnings (Off);

   Delete_File (Database_Path);

end API.Manifest.Fetch;
