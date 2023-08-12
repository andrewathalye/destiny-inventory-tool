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
with Shared.Config;
with Shared.Strings; use Shared.Strings;

with Tasks.Download;

--  Standalone Fetch Function
with API.Manifest.Fetch;

package body API.Manifest is
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
               Caching    => Shared.Config.Debug_API)));
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

      Result := API.Manifest.Fetch (Localised_Manifest_Path);

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
         Unbounded_String'Write (S, Current_Manifest_Version);

         --  Raw manifest record
         Manifest_Type'Write (S, Result);
         Close (SF);
      end;
      return Result;
   end Get_Manifest;

end API.Manifest;
