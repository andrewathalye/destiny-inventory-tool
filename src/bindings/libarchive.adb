with Interfaces.C.Strings; use Interfaces.C.Strings;
use Interfaces.C;

with System; use System;

package body libarchive is
   --  Types
   --  POSIX but not in Interfaces.C
   --  This is an approximation and hopefully good enough for most systems
   type ssize_t is range -1 .. Integer_64'Last;

   --  From libarchive.h
   type Archive_Result_Type is (Fatal, Failed, Warn, Retry, Ok, EOF);
   for Archive_Result_Type use
     (Fatal  => -30,
      Failed => -25,
      Warn   => -20,
      Retry  => -10,
      Ok     => 0,
      EOF    => 1);

   for Archive_Result_Type'Size use int'Size;

   --  Internal Subprograms
   procedure Check_Result
     (Archive : not null access Archive_Type; Result : Archive_Result_Type)
   is
      function Archive_Error_String
        (a : not null access Archive_Type) return chars_ptr with
        Import => True, Convention => C;
   begin
      if Result /= Ok then
         raise Archive_Error with Value (Archive_Error_String (Archive));
      end if;
   end Check_Result;

   --  Public Subprograms

   --  I/O
   procedure Archive_Read_Open_Memory
     (Archive :         not null access Archive_Type;
      Data    : aliased Stream_Element_Array)
   is
      function Archive_Read_Open_Memory
        (a : not null access Archive_Type;
         b : System.Address;
         l : size_t)
         return Archive_Result_Type with
        Import => True, Convention => C;

      Result : constant Archive_Result_Type :=
        Archive_Read_Open_Memory (Archive, Data'Address, Data'Length);
   begin
      Check_Result (Archive, Result);
   end Archive_Read_Open_Memory;

   procedure Archive_Read_Data
     (Archive : not null access Archive_Type;
      Data    : access Stream_Element_Array)
   is
      function Archive_Read_Data
        (a : not null access Archive_Type;
         b : System.Address;
         l : size_t)
         return ssize_t with
        Import => True, Convention => C;

      Result : constant ssize_t :=
        Archive_Read_Data
          (a => Archive, b => Data.all'Address, l => Data.all'Length);
   begin
      if Result /= Data.all'Length then
         raise Archive_Error
           with "Failed reading archive data: got length " & Result'Image &
           " instead of" & Data.all'Length'Image;
      end if;
   end Archive_Read_Data;

   --  Metadata
   procedure Archive_Read_Support_Format_Zip
     (Archive : not null access Archive_Type)
   is
      function Archive_Read_Support_Format_Zip
        (a : not null access Archive_Type) return Archive_Result_Type with
        Import => True, Convention => C;

      Result : constant Archive_Result_Type :=
        Archive_Read_Support_Format_Zip (Archive);
   begin
      Check_Result (Archive, Result);
   end Archive_Read_Support_Format_Zip;

   function Archive_Read_Next_Header
     (Archive : not null access Archive_Type) return Archive_Entry_Access
   is
      function Archive_Read_Next_Header
        (a :     not null access Archive_Type;
         e : out Archive_Entry_Access)
         return Archive_Result_Type with
        Import => True, Convention => C;

      Archive_Entry : Archive_Entry_Access;
      Result        : constant Archive_Result_Type :=
        Archive_Read_Next_Header (Archive, Archive_Entry);
   begin
      Check_Result (Archive, Result);

      return Archive_Entry;
   end Archive_Read_Next_Header;

   function Archive_Entry_Size_Is_Set
     (Archive_Entry : not null access Archive_Entry_Type) return Boolean
   is
      function Archive_Entry_Size_Is_Set
        (a : not null access Archive_Entry_Type) return int with
        Import => True, Convention => C;
   begin
      return Archive_Entry_Size_Is_Set (Archive_Entry) > 0;
   end Archive_Entry_Size_Is_Set;
   --  Archive_Entry_Size is imported

   --  Allocation / Deallocation
   --  Archive_Read_New is imported
   procedure Archive_Read_Free (Archive : out Archive_Access) is
      function Archive_Read_Free
        (a : not null access Archive_Type) return Archive_Result_Type with
        Import => True, Convention => C;

      Result : constant Archive_Result_Type := Archive_Read_Free (Archive);
   begin
      Check_Result (Archive, Result);

      Archive := null;
   end Archive_Read_Free;

end libarchive;
