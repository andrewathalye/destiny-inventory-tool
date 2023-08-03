with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

package Shared.Streams is
   --  Types
   type Stream_Element_Array_Access is access Stream_Element_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Ada.Streams.Stream_Element_Array,
      Name   => Stream_Element_Array_Access);
end Shared.Streams;
