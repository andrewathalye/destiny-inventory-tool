limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes;       use API.Definitions.Hashes;

package API.Definitions.Destiny_Inventory_Bucket is
--------------------------------------
   -- DestinyInventoryBucketDefinition --
   --------------------------------------
   type Destiny_Inventory_Bucket_Category is
     (Invisible, Item, Currency, Equippable, Ignored);

   type Item_Location_Type is (Unknown, Inventory, Vault, Vendor, Postmaster);
   --  Note: "Postmaster" seems to not be used anymore, so check the bucket
   --  location instead

   type Destiny_Inventory_Bucket_Definition is record
      Description  : Unbounded_String;
      Name         : Unbounded_String;
      Category     : Destiny_Inventory_Bucket_Category;
      Bucket_Order : Integer_32;
      Item_Count   : Quantity_Type;
      Location     : Item_Location_Type;
      FIFO         : Boolean;
   end record;
   package DIBDM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Inventory_Bucket_Definition_Manifest_Hash,
      Element_Type => Destiny_Inventory_Bucket_Definition);
   subtype Destiny_Inventory_Bucket_Map is DIBDM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Inventory_Bucket;
