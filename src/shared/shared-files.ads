with Ada.Streams;

package Shared.Files is
   --  Note: These subprograms REQUIRE the program to be compiled with "-O2"
   --  Otherwise they try to allocate too much memory on the stack and the
   --  program hangs. The alternative here isn't great because it requires
   --  allocating a variable on the heap which isn't possible with this
   --  specification

   --  Cache Utilities
   --  Get a file from the cache
   function Get_Cached
     (Name : String) return Ada.Streams.Stream_Element_Array with
     Inline;

   --  Query whether a given file is cached
   function Has_Cached (Name : String) return Boolean with
     Inline;

   --  Save a file to the cache
   procedure Cache (Name : String; Content : Ada.Streams.Stream_Element_Array);

   --  General I/O
   --  Get a File as a Stream_Element_Array via its actual path
   function Get_Data (Name : String) return Ada.Streams.Stream_Element_Array;
end Shared.Files;
