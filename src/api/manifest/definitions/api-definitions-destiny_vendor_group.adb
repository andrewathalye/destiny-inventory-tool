--  VSS
with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

--  Local
with API.Manifest;
with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

package body API.Definitions.Destiny_Vendor_Group is
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type)
   is
      Vendor_Group : Destiny_Vendor_Group_Definition;
   begin
      Wait_Until_Key (Reader, "order");
      Read_Next (Reader);
      Vendor_Group.Order :=
        Destiny_Vendor_Group_Order_Type (As_Integer (Number_Value (Reader)));

      Read_Next (Reader); --  "categoryName"
      Read_Next (Reader);
      Vendor_Group.Category_Name := VS2UB (String_Value (Reader));

      The_Manifest.Destiny_Vendor_Groups.Insert
        (Destiny_Vendor_Group_Definition_Manifest_Hash (Hash), Vendor_Group);
   end Read;

end API.Definitions.Destiny_Vendor_Group;
