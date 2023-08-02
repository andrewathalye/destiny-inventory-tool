with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

procedure API.Manifest.Record_Callback
  (Hash         :        Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Title : Destiny_Title_Name;
begin
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

      The_Manifest.Destiny_Titles.Insert (Hash, Title);
   end if;
end API.Manifest.Record_Callback;
