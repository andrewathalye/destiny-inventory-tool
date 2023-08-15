with Ada.Streams; use Ada.Streams;

with Shared.Streams; use Shared.Streams;

package Shared.Files is
   --  Cache Utilities
   --  Get a file from the cache
   function Get_Cached (Name : String) return Shared_Stream_Element_Array;

   --  Query whether a given file is cached
   function Has_Cached (Name : String) return Boolean with
     Inline;

   --  Save a file to the cache
   procedure Cache (Name : String; Content : Stream_Element_Array);

   --  General I/O
   --  Get a File as a Stream_Element_Array via its actual path
   function Get_Data (Name : String) return Shared_Stream_Element_Array;
end Shared.Files;
