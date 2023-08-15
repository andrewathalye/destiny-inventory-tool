with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

with API.Manifest;

package body API.Definitions.Destiny_Place is
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type)
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
   end Read;

end API.Definitions.Destiny_Place;
