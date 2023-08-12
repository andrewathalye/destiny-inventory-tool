with API.Manifest;

with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

package body API.Definitions.Destiny_Race is
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type)
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
   end Read;

end API.Definitions.Destiny_Race;
