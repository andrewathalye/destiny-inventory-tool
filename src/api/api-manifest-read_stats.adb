separate (API.Manifest)
procedure Read_Stats
  (Reader : in out JSON_Simple_Pull_Reader; Stats : out Destiny_Stat_Map)
is
   Name : Unbounded_String;
begin
   Read_Next (Reader); --  START_OBJECT
   Read_Next (Reader); --  hash as key_name or END_OBJECT
   while Event_Kind (Reader) /= End_Object loop
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "displayProperties"
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "description"
      Read_Next (Reader);

      Read_Next (Reader); -- "name"
      Read_Next (Reader);
      Name := VS2UB (String_Value (Reader));

      Wait_Until_Key (Reader, "hash");
      Read_Next (Reader);
      Stats.Insert (Manifest_Hash (As_Integer (Number_Value (Reader))), Name);

      Wait_Until_Event (Reader, End_Object); --  END_OBJECt for this stat
      Read_Next (Reader); -- hash as key_name or END_OBJECT
   end loop;
end Read_Stats;
