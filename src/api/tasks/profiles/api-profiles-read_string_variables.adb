separate (API.Profiles)
procedure Read_String_Variables
  (Reader : in out JSON_Simple_Pull_Reader; Map : out String_Variable_Map)
is
   Key : Unsigned_32;
begin
   Read_Next (Reader); --  START_OBJECT

   Read_Next (Reader); --  "data"
   Read_Next (Reader); --  START_OBJECT

   Read_Next (Reader); --  "integerValuesByHash"
   Read_Next (Reader); --  START_OBJECT
   Read_Next (Reader); --  KEY_NAME or END_OBJECT

   Read_Variable :
      while Event_Kind (Reader) /= End_Object loop
         Key := Unsigned_32'Value (VS2S (Key_Name (Reader)));
         Read_Next (Reader); -- NUMBER_VALUE
         Map.Insert
           (Key, String_Variable_Type (As_Integer (Number_Value (Reader))));

         Read_Next (Reader); -- KEY_NAME or END_OBJECT
      end loop Read_Variable;
end Read_String_Variables;
