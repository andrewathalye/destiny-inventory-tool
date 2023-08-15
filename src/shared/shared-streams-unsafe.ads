with Ada.Unchecked_Deallocation;

package Shared.Streams.Unsafe is
   --  Stream Element Array Access
   type Stream_Element_Array_Access is access Stream_Element_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Stream_Element_Array, Stream_Element_Array_Access);
end Shared.Streams.Unsafe;
