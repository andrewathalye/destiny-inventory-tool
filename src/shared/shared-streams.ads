with Ada.Streams; use Ada.Streams;

--  GNATCOLL
with GNATCOLL.Refcount; use GNATCOLL.Refcount;

package Shared.Streams is
   --  Refcounted Stream_Element_Array
   package Shared_Stream_Element_Arrays is new Shared_Pointers
     (Element_Type => Stream_Element_Array);
   subtype Shared_Stream_Element_Array is Shared_Stream_Element_Arrays.Ref;
end Shared.Streams;
