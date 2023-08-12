with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.Strings; use Shared.Strings;

with API.Manifest;

package body API.Definitions.Destiny_Stat is
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type)
   is
      Name : Unbounded_String;
   begin
      --  TODO: Check why this extra Read_Next is needed
      Read_Next (Reader);
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "displayProperties"
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "description"
      Read_Next (Reader);

      Read_Next (Reader); -- "name"
      Read_Next (Reader);
      Name := VS2UB (String_Value (Reader));

      The_Manifest.Destiny_Stats.Insert
        (Destiny_Stat_Definition_Manifest_Hash (Hash), Name);
   end Read;

end API.Definitions.Destiny_Stat;
