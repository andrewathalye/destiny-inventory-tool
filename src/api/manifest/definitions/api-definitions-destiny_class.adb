with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
with VSS.JSON;              use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

with API.Manifest;

package body API.Definitions.Destiny_Class is
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type)
   is
      Class : Destiny_Class_Name;
   begin
      Wait_Until_Key (Reader, "genderedClassNames");
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "Male"
      Read_Next (Reader);
      Class (Male) := VS2UB (String_Value (Reader));

      Read_Next (Reader); -- "Female"
      Read_Next (Reader);
      Class (Female) := VS2UB (String_Value (Reader));

      The_Manifest.Destiny_Classes.Insert
        (Destiny_Class_Definition_Manifest_Hash (Hash), Class);
   end Read;

end API.Definitions.Destiny_Class;
