limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes; use API.Definitions.Hashes;

package API.Definitions.Destiny_Vendor_Group is
----------------------------------
   -- DestinyVendorGroupDefinition --
   ----------------------------------
   type Destiny_Vendor_Group_Order_Type is new Integer_32;
   type Destiny_Vendor_Group_Definition is record
      Order         : Destiny_Vendor_Group_Order_Type;
      Category_Name : Unbounded_String;
   end record;

   package DVGDM is new Ada.Containers.Ordered_Maps
     (Destiny_Vendor_Group_Definition_Manifest_Hash,
      Destiny_Vendor_Group_Definition);
   subtype Destiny_Vendor_Group_Map is DVGDM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Vendor_Group;
