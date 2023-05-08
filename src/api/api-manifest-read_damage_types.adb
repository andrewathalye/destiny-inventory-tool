separate (API.Manifest)
procedure Read_Damage_Types
  (Reader       : in out JSON_Simple_Pull_Reader;
   Damage_Types :    out Destiny_Damage_Type_Map)
is

   Damage_Type : Destiny_Damage_Type_Definition;

begin
   Read_Next (Reader); -- START_OBJECT

   while Event_Kind (Reader) /= End_Object loop
      Damage_Type.Icon_Path := Null_Unbounded_String;
      Wait_Until_Key (Reader, "displayProperties");
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "description"
      Read_Next (Reader);
      Damage_Type.Description := VS2UB (String_Value (Reader));
      Read_Next (Reader); -- "name"
      Read_Next (Reader);
      Damage_Type.Name := VS2UB (String_Value (Reader));
      Read_Next (Reader); -- "icon" or "hasIcon"

      if VS2S (Key_Name (Reader)) = "icon" then
         Read_Next (Reader);
         Damage_Type.Icon_Path := VS2UB (String_Value (Reader));
      end if;
      Wait_Until_Key (Reader, "showIcon");
      Read_Next (Reader);
      Damage_Type.Show_Icon := Boolean_Value (Reader);
      Wait_Until_Key (Reader, "hash");
      Read_Next (Reader);
      Damage_Types.Insert
        (Manifest_Hash (As_Integer (Number_Value (Reader))), Damage_Type);
      Wait_Until_Event (Reader, End_Object);
      Read_Next (Reader);
   end loop;
end Read_Damage_Types;
