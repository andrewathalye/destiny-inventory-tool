-- VSS
with VSS.Stream_Element_Vectors.Conversions;
use VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Conversions; use VSS.Strings;
use VSS.Strings.Conversions;
with VSS.Text_Streams; use VSS.Text_Streams;

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

   function Get_Strings
     (JSON_Data : Unbounded_String;
      Keys      : Unbounded_String_Array)
      return Unbounded_String_List
   is
      List : Unbounded_String_List;

      Stream : Memory_UTF8_Input_Stream_Access := Get_Stream (JSON_Data);
      Reader : JSON_Simple_Pull_Reader;
   begin
      Set_Stream (Reader, Input_Text_Stream_Access (Stream));

      while not At_End (Reader) loop
         case Read_Next (Reader) is
            when Key_Name =>
               for K of Keys loop
                  if To_Unbounded_UTF_8_String (Key_Name (Reader)) = K then
                     Read_Next (Reader);
                     if List.Contains (K) then
                        raise Program_Error with "Duplicate key in JSON parse";
                     end if;
                     List.Insert
                       (K, To_Unbounded_UTF_8_String (String_Value (Reader)));
                  end if;
               end loop;
            when others =>
               null;
         end case;
      end loop;

      Free (Stream);
      return List;
   end Get_Strings;

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
      raise Program_Error;
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
