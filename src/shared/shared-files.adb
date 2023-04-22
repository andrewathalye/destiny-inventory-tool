pragma Ada_2022;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
use Ada.Streams;

with Ada.Directories; use Ada.Directories;

-- AWS
with AWS.URL; use AWS;

package body Shared.Files is
   -- Cache Utilities
   function Has_Cached (Name : String) return Boolean is
     (Exists ("cache/" & URL.Encode (Name)));

   function Get_Data (Name : String) return Stream_Element_Array is
      SF  : File_Type;
      S   : Stream_Access;
      SEA : Stream_Element_Array (1 .. Stream_Element_Offset (Size (Name)));
   begin
      Open (SF, In_File, Name, "shared=yes");
      S := Stream (SF);

      Stream_Element_Array'Read (S, SEA);
      Close (SF);

      return SEA;
   end Get_Data;

   function Get_Cache_Path (Name : String) return String is
     ("cache/" & URL.Encode (Name));

   function Get_Cached (Name : String) return Stream_Element_Array is
     (Get_Data (Get_Cache_Path (Name)));

   procedure Cache (Name : String; Content : Stream_Element_Array) is
      SF : File_Type;
      S  : Stream_Access;
   begin
      if Has_Cached (Name) then
         raise Program_Error;
      end if;

      if not Exists ("cache/") then
         Create_Directory ("cache");
      end if;

      -- Write to disk cache
      Create (SF, Out_File, "cache/" & URL.Encode (Name));
      S := Stream (SF);
      Stream_Element_Array'Write (S, Content);
      Close (SF);
   end Cache;
end Shared.Files;
