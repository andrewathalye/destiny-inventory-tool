separate (API.Manifest)
procedure Read_Genders
  (Reader : in out JSON_Simple_Pull_Reader; Genders : out Destiny_Gender_Map)
is

   Gender : Destiny_Gender_Definition;

begin
   Read_Next (Reader); -- START_OBJECT

   while Event_Kind (Reader) /= End_Object loop
      Wait_Until_Key (Reader, "genderType");
      Read_Next (Reader);
      Gender.Gender_Type :=
        Destiny_Gender_Type'Enum_Val (As_Integer (Number_Value (Reader)));
      Wait_Until_Key (Reader, "name");
      Read_Next (Reader);
      Gender.Gender_Name := VS2UB (String_Value (Reader));
      Wait_Until_Key (Reader, "hash");
      Read_Next (Reader);
      Genders.Insert
        (Manifest_Hash (As_Integer (Number_Value (Reader))), Gender);
      Wait_Until_Event (Reader, End_Object);
      Read_Next (Reader);
   end loop;
end Read_Genders;
