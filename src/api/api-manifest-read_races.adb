separate (API.Manifest)
procedure Read_Races
  (Reader : in out JSON_Simple_Pull_Reader; Races : out Destiny_Race_Map)
is

   Race : Destiny_Race_Name;

begin
   Read_Next (Reader); -- START_OBJECT

   while Event_Kind (Reader) /= End_Object loop
      Wait_Until_Key (Reader, "genderedRaceNames");
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "Male"
      Read_Next (Reader);
      Race (Male) := VS2UB (String_Value (Reader));
      Read_Next (Reader); -- "Female"
      Read_Next (Reader);
      Race (Female) := VS2UB (String_Value (Reader));
      Wait_Until_Key (Reader, "hash");
      Read_Next (Reader);
      Races.Insert (Manifest_Hash (As_Integer (Number_Value (Reader))), Race);
      Wait_Until_Event (Reader, End_Object);
      Read_Next (Reader);
   end loop;
end Read_Races;
