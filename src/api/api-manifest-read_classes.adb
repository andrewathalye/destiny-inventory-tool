separate (API.Manifest)
procedure Read_Classes
  (Reader : in out JSON_Simple_Pull_Reader; Classes : out Destiny_Class_Map)
is

   Class : Destiny_Class_Name;

begin
   Read_Next (Reader); -- START_OBJECT

   while Event_Kind (Reader) /= End_Object loop
      Wait_Until_Key (Reader, "genderedClassNames");
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "Male"
      Read_Next (Reader);
      Class (Male) := VS2UB (String_Value (Reader));
      Read_Next (Reader); -- "Female"
      Read_Next (Reader);
      Class (Female) := VS2UB (String_Value (Reader));
      Wait_Until_Key (Reader, "hash");
      Read_Next (Reader);
      Classes.Insert
        (Manifest_Hash (As_Integer (Number_Value (Reader))), Class);
      Wait_Until_Event (Reader, End_Object);
      Read_Next (Reader);
   end loop;
end Read_Classes;
