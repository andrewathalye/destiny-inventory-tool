separate (API.Manifest)
procedure Read_Objectives
  (Reader     : in out JSON_Simple_Pull_Reader;
   Objectives :    out Destiny_Objective_Map)
is
   Objective : Destiny_Objective_Definition;
begin
   Read_Next (Reader); --  START_OBJECT
   Read_Next (Reader); --  HASH as key or END_OBJECT

   Objective_Loop :
      while Event_Kind (Reader) /= End_Object loop
         Read_Next (Reader); --  START_OBJECT

         --  "icon" is a nullable field. Parse it manually.
         Read_Next (Reader); --  "displayProperties"
         Read_Next (Reader); --  START_OBJECT
         Read_Next (Reader); --  "description"
         Read_Next (Reader);
         Read_Next (Reader); --  "name"
         Read_Next (Reader);
         Read_Next (Reader); --  "icon" or "hasIcon"

         if VS2S (Key_Name (Reader)) = "icon" then
            Read_Next (Reader);
            Objective.Icon_Path := VS2UB (String_Value (Reader));
         else
            Objective.Icon_Path := Null_Unbounded_String;
         end if;

         Wait_Until_Key (Reader, "progressDescription");
         Read_Next (Reader);
         Objective.Progress_Description := VS2UB (String_Value (Reader));

         Wait_Until_Key (Reader, "hash");
         Read_Next (Reader);
         Objectives.Insert
           (Manifest_Hash (As_Integer (Number_Value (Reader))), Objective);

         Read_Next (Reader); -- "index"
         Read_Next (Reader);

         Read_Next (Reader); -- "redacted"
         Read_Next (Reader);

         Read_Next (Reader); -- "blacklisted"
         Read_Next (Reader);

         Read_Next (Reader); -- END_OBJECT
         Read_Next (Reader); -- HASH as key or END_OBJECT
      end loop Objective_Loop;
end Read_Objectives;
