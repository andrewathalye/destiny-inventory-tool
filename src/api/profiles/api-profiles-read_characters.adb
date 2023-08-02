separate (API.Profiles)
procedure Read_Characters
  (Reader : in out JSON_Simple_Pull_Reader; List : out Character_List)
is
begin
   Wait_Until_Key (Reader, "data");
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader);

   if Event_Kind (Reader) = End_Object then
      return;
   end if;

   loop
      declare

         Character : Character_Type;
         Stat_Temp : Manifest_Hash;

      begin
         Wait_Until_Key (Reader, "characterId");
         Read_Next (Reader);
         Character.Character_ID := VS2UB (String_Value (Reader));

         Wait_Until_Key (Reader, "dateLastPlayed");
         Read_Next (Reader);
         Character.Date_Last_Played := VS2UB (String_Value (Reader));

         Wait_Until_Key (Reader, "light");
         Read_Next (Reader);
         Character.Light := Quantity_Type (As_Integer (Number_Value (Reader)));

         Wait_Until_Key (Reader, "stats");
         Read_Next (Reader); -- Start_Object
         Read_Next (Reader); -- Key_Name
         while Event_Kind (Reader) /= End_Object loop
            Stat_Temp :=
              Manifest_Hash'Value (VS2S (Key_Name (Reader))); -- Stat Name Hash
            Read_Next (Reader); -- NUMBER_VALUE
            Character.Stats.Insert
              (Stat_Temp, Quantity_Type (As_Integer (Number_Value (Reader))));
            Read_Next (Reader); -- KEY_NAME or END_OBJECT
         end loop;

         Wait_Until_Key (Reader, "raceHash");
         Read_Next (Reader);
         Character.Race_Hash :=
           Manifest_Hash (As_Integer (Number_Value (Reader)));

         Wait_Until_Key (Reader, "genderHash");
         Read_Next (Reader);
         Character.Gender_Hash :=
           Manifest_Hash (As_Integer (Number_Value (Reader)));

         Wait_Until_Key (Reader, "classHash");
         Read_Next (Reader);
         Character.Class_Hash :=
           Manifest_Hash (As_Integer (Number_Value (Reader)));

         Wait_Until_Key (Reader, "emblemPath");
         Read_Next (Reader);
         Character.Emblem_Path := VS2UB (String_Value (Reader));

         Wait_Until_Key (Reader, "emblemBackgroundPath");
         Read_Next (Reader);
         Character.Emblem_Background_Path := VS2UB (String_Value (Reader));

         --  Attempt to Read "titleRecordHash". It is not always present however
         Wait_Until_Key (Reader, "percentToNextLevel");
         Read_Next (Reader); -- Number_Value
         Read_Next (Reader); -- Either Key_Name or End_Object

         if Event_Kind (Reader) = Key_Name then
            Read_Next (Reader); -- "titleRecordHash"
            Character.Title_Record_Hash :=
              Manifest_Hash (As_Integer (Number_Value (Reader)));

            Read_Next (Reader); -- End_Object for this Character
         end if;

         --  Submit Character
         List.Append (Character);

      end;

      Read_Next
        (Reader); -- either End_Object or Key_Name for the next Character
      if Event_Kind (Reader) = End_Object then
         exit;
      end if;
   end loop;
end Read_Characters;
