with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

procedure API.Manifest.Race_Callback
  (Hash         :        Base_Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Race : Destiny_Race_Name;
begin
   Wait_Until_Key (Reader, "genderedRaceNames");
   Read_Next (Reader); -- START_OBJECT

   Read_Next (Reader); -- "Male"
   Read_Next (Reader);
   Race (Male) := VS2UB (String_Value (Reader));

   Read_Next (Reader); -- "Female"
   Read_Next (Reader);
   Race (Female) := VS2UB (String_Value (Reader));

   The_Manifest.Destiny_Races.Insert
     (Destiny_Race_Definition_Manifest_Hash (Hash), Race);
end API.Manifest.Race_Callback;
