pragma Ada_2022;

with Ada.Streams.Stream_IO;
with Ada.Directories; use Ada.Directories;
--  with Ada.Text_IO;
--  with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

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
with API.Debug;

with Shared.JSON;    use Shared.JSON;
with Shared.Debug;
with Shared.Strings; use Shared.Strings;

with Tasks.Download;

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

package body API.Manifest is
   function Fetch_Manifest
     (Current_Manifest_Version : Unbounded_String;
      Localised_Manifest_Path  : Unbounded_String)
      return Manifest_Type
   is
      --  Types
      type Stream_Element_Array_Access is
        access Ada.Streams.Stream_Element_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Ada.Streams.Stream_Element_Array,
         Name   => Stream_Element_Array_Access);

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
          (Tasks.Download.Download
             (+(Bungie_Root & (+Localised_Manifest_Path))));

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
               Cursor : GNATCOLL.SQL.Exec.Direct_Cursor;

               Stream : Memory_UTF8_Input_Stream_Access :=
                 new Memory_UTF8_Input_Stream;

               use GNATCOLL.SQL.Exec;

               function As_Manifest_Hash (I : Integer) return Manifest_Hash is
                  M : Manifest_Hash with
                    Address => I'Address;
               begin
                  return M;
               end As_Manifest_Hash;

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
                       (As_Manifest_Hash (Cursor.Integer_Value (0)),
                        Reader,
                        Result);
--                  exception
--                     when X : others =>
--                        Ada.Text_IO.Put_Line
--                          (Ada.Text_IO.Standard_Error, Cursor.Value (1));
--                        Ada.Exceptions.Reraise_Occurrence (X);
                  end;

                  Cursor.Next;
               end loop;

               --  Cleanup
               Free (Stream);

               Shared.Debug.Put_Line ("Processed " & Table);
            end Add_Data;

         begin
            --  Connect to Database
            Description :=
              GNATCOLL.SQL.Sqlite.Setup (Database => Database_Path);
            Connection := GNATCOLL.SQL.Exec.Build_Connection (Description);

            --  Add data from tables using callbacks
            pragma Warnings (Off, "unreachable code");
            goto Read_Vendor_Definition; -- TODO TODO
            Add_Data
              ("DestinyClassDefinition", API.Manifest.Class_Callback'Access);
            Add_Data
              ("DestinyGenderDefinition", API.Manifest.Gender_Callback'Access);
            Add_Data
              ("DestinyInventoryBucketDefinition",
               API.Manifest.Inventory_Bucket_Callback'Access);
            Add_Data
              ("DestinyRaceDefinition", API.Manifest.Race_Callback'Access);
            Add_Data
              ("DestinyDamageTypeDefinition",
               API.Manifest.Damage_Type_Callback'Access);
            Add_Data
              ("DestinyStatDefinition", API.Manifest.Stat_Callback'Access);
            Add_Data
              ("DestinyInventoryItemDefinition",
               API.Manifest.Inventory_Item_Callback'Access);
            Add_Data
              ("DestinyObjectiveDefinition",
               API.Manifest.Objective_Callback'Access);
            Add_Data
              ("DestinyRecordDefinition", API.Manifest.Record_Callback'Access);
            <<Read_Vendor_Definition>> -- TODO TODO
            Add_Data
              ("DestinyVendorDefinition", API.Manifest.Vendor_Callback'Access);

            --  Done reading, free connection and description
            GNATCOLL.SQL.Exec.Free (Connection);
            GNATCOLL.SQL.Exec.Free (Description);

         end Read_Database;

      return Result;
      pragma Warnings (Off);

      Delete_File (Database_Path);

      --  Cache manifest for later use
      --  TODO TODO TODO TODO use Sqlite directly and don’t cache?
      declare

         use Ada.Streams.Stream_IO;
         SF : File_Type;
         S  : Stream_Access;

      begin
         Create (SF, Out_File, "dat/manifest.dat");
         S := Ada.Streams.Stream_IO.Stream (SF);

         --   Compatibility metadata
         Natural'Write (S, Current_Manifest_Format_Version);
         Unbounded_String'Write (S, Current_Manifest_Version);

         --  Raw manifest record
         Manifest_Type'Write (S, Result);
         Close (SF);
      end;
      return Result;
   end Fetch_Manifest;

   function Get_Manifest return Manifest_Type is
      --  Storage
      Current_Manifest_Version : Unbounded_String;
      Localised_Manifest_Path  : Unbounded_String;

      --  Requests
      Stream : Memory_UTF8_Input_Stream_Access := new Memory_UTF8_Input_Stream;

      --  Parsing
      Reader : JSON_Simple_Pull_Reader;
      Result : Manifest_Type;

   begin
      Shared.Debug.Put_Line ("Get manifest");

      Set_Data
        (Stream.all,
         To_Stream_Element_Vector
           (Tasks.Download.Download
              (+(API_Root & "/Destiny2/Manifest/"),
               Needs_Auth => True,
               Caching    => API.Debug.Caching)));
      Set_Stream (Reader, Input_Text_Stream_Access (Stream));

      Wait_Until_Key (Reader, "version");
      Read_Next (Reader);
      Current_Manifest_Version := VS2UB (String_Value (Reader));

      Wait_Until_Key (Reader, "mobileWorldContentPaths");
      Wait_Until_Key
        (Reader, "en"); -- +M.Bungie_Net_User.Locale); -- TODO Needs to change
      Read_Next (Reader); -- STRING_VALUE

      Localised_Manifest_Path := VS2UB (String_Value (Reader));

      Free (Stream);

      --  Try to load cached manifest
      if Exists ("dat/manifest.dat") then
         Shared.Debug.Put_Line ("Load preparsed manifest");

         Load_Cached_Manifest :
            declare

               use Ada.Streams.Stream_IO;
               SF                      : File_Type;
               S                       : Stream_Access;
               Manifest_Format_Version : Natural;
               Cached_Manifest_Version : Unbounded_String;

            begin
               Open (SF, In_File, "dat/manifest.dat");
               S := Ada.Streams.Stream_IO.Stream (SF);

               --  Read compatibility metadata
               Natural'Read
                 (S,
                  Manifest_Format_Version); --  Manifest serialisation format
               Unbounded_String'Read
                 (S, Cached_Manifest_Version); --  Bungie Manifest version

               --  Read raw manifest data after format check
               if Manifest_Format_Version = Current_Manifest_Format_Version and
                 Cached_Manifest_Version = Current_Manifest_Version
               then
                  Manifest_Type'Read (S, Result);
                  Close (SF);

                  return Result;
               else
                  Close (SF);
                  Shared.Debug.Put_Line ("Update manifest");
                  Delete_File ("dat/manifest.dat");
               end if;
            end Load_Cached_Manifest;

      end if;
      return
        Fetch_Manifest (Current_Manifest_Version, Localised_Manifest_Path);
   end Get_Manifest;

end API.Manifest;
