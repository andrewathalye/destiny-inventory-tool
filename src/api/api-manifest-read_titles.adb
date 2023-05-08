separate (API.Manifest)
procedure Read_Titles
  (Reader : in out JSON_Simple_Pull_Reader; Titles : out Destiny_Title_Map)
is

   Title : Destiny_Title_Name;

begin
   Read_Next (Reader); -- START_OBJECT

   while Event_Kind (Reader) /= End_Object loop
      Wait_Until_Key (Reader, "titleInfo");
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "hasTitle"
      Read_Next (Reader);

      if Boolean_Value (Reader) then
         Read_Next (Reader); -- "titlesByGender"
         Read_Next (Reader); -- START_OBJECT

         Read_Next (Reader); -- "Male"
         Read_Next (Reader);
         Title (Male) := VS2UB (String_Value (Reader));
         Read_Next (Reader); -- "Female"
         Read_Next (Reader);
         Title (Female) := VS2UB (String_Value (Reader));
         Wait_Until_Key (Reader, "hash");
         Read_Next (Reader);
         Titles.Insert
           (Manifest_Hash (As_Integer (Number_Value (Reader))), Title);
      end if;
      Wait_Until_Key (Reader, "blacklisted");
      Wait_Until_Event (Reader, End_Object);
      Read_Next (Reader);
   end loop;
end Read_Titles;
