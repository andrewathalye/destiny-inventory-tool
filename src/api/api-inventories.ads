with Ada.Containers.Vectors;

--  Local Packages
with API.Manifest.Tools;
use type API.Manifest.Tools.Item_Description;

with API.Definitions.Hashes;

package API.Inventories is
   --  Exceptions
   Inventory_Full : exception;
   Item_Not_Found : exception;

   --  Types
   package IDV is new Ada.Containers.Vectors
     (Natural, Manifest.Tools.Item_Description);
   subtype Item_Description_List is IDV.Vector;

   type Item_Description_List_Bucket_Location_Type_Array is
     array (Manifest.Tools.Bucket_Location_Type) of Item_Description_List;

   type Item_Description_Bucket_Location_Type_Array is
     array
       (Manifest.Tools
          .Bucket_Location_Type) of Manifest.Tools
       .Item_Description;

   --  Tagged Type
   --  Note: To interact with inventories, use the specialised packages (Vendor, Global, Character)
   type Destiny_Inventory_Type is interface;

   --  Associated Subprograms

   --------------------------
   -- Inventory Management --
   --------------------------
   --  Note: All associated subprograms _except_ Clear ignore supplemental inventories provided by specialised packages

   --  .raises Inventory_Full if Inventory cannot store additional items
   procedure Add
     (Inventory : in out Destiny_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description) is abstract;

   --  .raises Item_Not_Found if Item cannot be found
   procedure Remove
     (Inventory : in out Destiny_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description) is abstract;

   procedure Update
     (Inventory : out Destiny_Inventory_Type;
      Items     :     Item_Description_List) is abstract;

   procedure Clear (Inventory : out Destiny_Inventory_Type) is abstract;

   -------------
   -- Queries --
   -------------

   --  Returns the count of items actually located in _Location_
   function Item_Count
     (Inventory : Destiny_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return Definitions.Quantity_Type is abstract;

   --  Returns all items actually located in _Location_
   function Get
     (Inventory : Destiny_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return Item_Description_List is abstract;

   --  Returns all items located in _Location_ by default (may be identical to Get depending on the inventory)
   function Get_Default
     (Inventory : Destiny_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return Item_Description_List is abstract;

   --  Returns the item stack for the item with hash Hash in inventory Inventory
   --  .raises Item_Not_Found if Hash does not match any item
   function Get
     (Inventory : Destiny_Inventory_Type;
      Hash      : Definitions.Hashes
        .Destiny_Inventory_Item_Definition_Manifest_Hash)
      return Manifest.Tools.Item_Description is abstract;

   --  Return the full inventory contents, either sorted by bucket location type or unsorted (as desired)
   function Get_Sorted
     (Inventory : Destiny_Inventory_Type)
      return Item_Description_List_Bucket_Location_Type_Array is abstract;
   function Get_Unsorted
     (Inventory : Destiny_Inventory_Type)
      return Item_Description_List is abstract;
end API.Inventories;
