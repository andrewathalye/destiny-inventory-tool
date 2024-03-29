with Ada.Streams; use Ada.Streams;

with Interfaces; use Interfaces;

--  Note: Thick binding over the parts of libarchive needed
--  to deal with Zip files in memory. No other parts of the API
--  are exposed here.

package libarchive is
   pragma Preelaborate (libarchive);

   --  Types
   type Archive_Type is limited private;
   type Archive_Access is access Archive_Type;

   type Archive_Entry_Type is limited private;
   type Archive_Entry_Access is access Archive_Entry_Type;

   --  Exceptions
   Archive_Error : exception;

   --  Subprograms
   --  Note: Any subprogram may raise an Archive_Error at any time.
   --  The exception will contain information about the error
   --  as provided by libarchive.

   --  I/O
   procedure Archive_Read_Open_Memory
     (Archive :         not null access Archive_Type;
      Data    : aliased Stream_Element_Array);
   procedure Archive_Read_Data
     (Archive : not null access Archive_Type;
      Data    : access Stream_Element_Array);

   --  Metadata
   procedure Archive_Read_Support_Format_Zip
     (Archive : not null access Archive_Type);
   function Archive_Read_Next_Header
     (Archive : not null access Archive_Type) return Archive_Entry_Access;
   function Archive_Entry_Size_Is_Set
     (Archive_Entry : not null access Archive_Entry_Type) return Boolean;
   function Archive_Entry_Size
     (Archive_Entry : not null access Archive_Entry_Type)
      return Integer_64 with
     Import => True, Convention => C;

   --  Allocation / Deallocation
   --  Note: Deallocating any access type invalidates it.
   function Archive_Read_New return Archive_Access with
     Import => True, Convention => C;
   procedure Archive_Read_Free (Archive : out Archive_Access);
private
   type Archive_Type is limited null record;
   type Archive_Entry_Type is limited null record;
end libarchive;
