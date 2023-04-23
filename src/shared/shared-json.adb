--  VSS
with VSS.Stream_Element_Vectors.Conversions;
use VSS.Stream_Element_Vectors.Conversions;
with VSS.Text_Streams; use VSS.Text_Streams;

--  Local Packages
with Shared.Strings; use Shared.Strings;

package body Shared.JSON is

   function Get_Stream
     (JSON_Data : Unbounded_String) return Memory_UTF8_Input_Stream_Access
   is

      Stream : constant Memory_UTF8_Input_Stream_Access :=
        new Memory_UTF8_Input_Stream;

   begin
      Set_Data (Stream.all, Unchecked_From_Unbounded_String (JSON_Data));
      return Stream;
   end Get_Stream;

   procedure Wait_Until_Key
     (Reader : in out JSON_Simple_Pull_Reader; Key : String)
   is
   begin
      while not At_End (Reader) loop
         Read_Next (Reader);

         if Event_Kind (Reader) = Key_Name
           and then VS2S (Key_Name (Reader)) = Key
         then
            return;
         end if;
      end loop;
      raise Program_Error with Key & " not found";
   end Wait_Until_Key;

   procedure Wait_Until_Event
     (Reader : in out JSON_Simple_Pull_Reader; Event : JSON_Event_Kind)
   is
   begin
      while not At_End (Reader) loop
         Read_Next (Reader);

         if Event_Kind (Reader) = Event then
            return;
         end if;
      end loop;
      raise Program_Error;
   end Wait_Until_Event;

end Shared.JSON;
