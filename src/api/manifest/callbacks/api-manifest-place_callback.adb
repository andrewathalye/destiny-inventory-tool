with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

procedure API.Manifest.Place_Callback
  (Hash         :        Base_Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Place : Destiny_Place_Definition;
begin
   Wait_Until_Key (Reader, "description");
   Read_Next (Reader);
   Place.Description := VS2UB (String_Value (Reader));

   Read_Next (Reader); --  "name"
   Read_Next (Reader);
   Place.Name := VS2UB (String_Value (Reader));

   The_Manifest.Destiny_Places.Insert
     (Destiny_Place_Definition_Manifest_Hash (Hash), Place);
end API.Manifest.Place_Callback;
