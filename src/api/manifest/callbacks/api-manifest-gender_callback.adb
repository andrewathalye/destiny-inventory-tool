with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
with VSS.JSON;              use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

procedure API.Manifest.Gender_Callback
  (Hash         :        Base_Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Gender : Destiny_Gender_Definition;
begin
   Wait_Until_Key (Reader, "genderType");
   Read_Next (Reader);
   Gender.Gender_Type :=
     Destiny_Gender_Type'Enum_Val (As_Integer (Number_Value (Reader)));

   Wait_Until_Key (Reader, "name");
   Read_Next (Reader);
   Gender.Gender_Name := VS2UB (String_Value (Reader));

   The_Manifest.Destiny_Genders.Insert
     (Destiny_Gender_Definition_Manifest_Hash (Hash), Gender);
end API.Manifest.Gender_Callback;
