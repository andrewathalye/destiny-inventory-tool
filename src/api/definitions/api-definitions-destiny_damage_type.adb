with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

with API.Manifest;

package body API.Definitions.Destiny_Damage_Type is
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type)
   is
      Damage_Type : Destiny_Damage_Type_Definition;
   begin
      Wait_Until_Key (Reader, "displayProperties");
      Read_Next (Reader); -- START_OBJECT

      Read_Next (Reader); -- "description"
      Read_Next (Reader);
      Damage_Type.Description := VS2UB (String_Value (Reader));

      Read_Next (Reader); -- "name"
      Read_Next (Reader);
      Damage_Type.Name := VS2UB (String_Value (Reader));

      Read_Next (Reader); -- "icon" or "hasIcon"
      if VS2S (Key_Name (Reader)) = "icon" then
         Read_Next (Reader);
         Damage_Type.Icon_Path := VS2UB (String_Value (Reader));
      end if;

      Wait_Until_Key (Reader, "showIcon");
      Read_Next (Reader);
      Damage_Type.Show_Icon := Boolean_Value (Reader);

      The_Manifest.Destiny_Damage_Types.Insert
        (Destiny_Damage_Type_Definition_Manifest_Hash (Hash), Damage_Type);
   end Read;

end API.Definitions.Destiny_Damage_Type;
