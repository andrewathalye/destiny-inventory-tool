pragma Ada_2022;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Hash;

--  Local Packages
with Shared.Streams.Unsafe; use Shared.Streams.Unsafe;

package body Shared.Files is
   --  Cache Utilities
   function Get_Cache_Path (Name : String) return String is
     ("cache/" & Ada.Strings.Hash (Name)'Image);

   function Has_Cached (Name : String) return Boolean is
     (Exists (Get_Cache_Path (Name)));

   --  Internal-only implementation of Get_Data
   --  Original_Name should be "" unless the original file name was encoded
   --  in the file being read
   function Get_Data
     (Name : String; Original_Name : String) return Shared_Stream_Element_Array
   is

      SF : File_Type;
      S  : Stream_Access;

      SF_Size      : constant Natural := Natural (Size (Name));
      Discard_Name : String (Original_Name'Range);

      SEA_A : Stream_Element_Array_Access :=
        new Stream_Element_Array
          (1 .. Stream_Element_Offset (SF_Size - Original_Name'Length));
      SSEA : Shared_Stream_Element_Array;

   begin
      Open (SF, In_File, Name, "shared=yes");
      S := Stream (SF);

      --  Skip unnecessary metadata
      if Original_Name'Length > 0 then
         String'Read (S, Discard_Name); --  Original Name
      end if;

      Stream_Element_Array'Read (S, SEA_A.all);
      Close (SF);

      --  Load data into Shared_Stream_Element_Array and clean up
      SSEA.Set (SEA_A.all);
      Free (SEA_A);

      return SSEA;
   end Get_Data;

   function Get_Data (Name : String) return Shared_Stream_Element_Array is
     (Get_Data (Name => Name, Original_Name => ""));

   function Get_Cached (Name : String) return Shared_Stream_Element_Array is
     (Get_Data
        (Name => Get_Cache_Path (Name), Original_Name => Name & ASCII.NUL));

   procedure Cache (Name : String; Content : Stream_Element_Array) is

      SF : File_Type;
      S  : Stream_Access;

   begin
      if Has_Cached (Name) then
         raise Program_Error
           with "File " & Name & " already cached or hash collision.";
      end if;

      if not Exists ("cache/") then
         Create_Directory ("cache");
      end if;

      --  Write to disk cache
      Create (SF, Out_File, Get_Cache_Path (Name));
      S := Stream (SF);

      --  Write metadata
      String'Write
        (S, Name & ASCII.NUL); --  Write null-terminated original filename

      --  Write raw data
      Stream_Element_Array'Write (S, Content);
      Close (SF);
   end Cache;

end Shared.Files;
