--  Local Packages
with API.Manifest.Tools;
with API.Profiles;

package API.Inventories.Global is
   --  The Vault ("Global") stores three kinds of items General Consumables
   --  Modifications
   --
   --  General items are stored in the inventory by their default bucket
   --  locations rather than by their actual location (Manifest.Tools.General)

   --  Types
   type Global_Inventory_Type is private;

   --  Subprograms
   --  Inventory Management
   procedure Add_Item
     (Inventory : in out Global_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description) with
     Inline;
   procedure Remove_Item
     (Inventory : in out Global_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description);

   --  This accepts either General / Consumables / Modifications or any other
   --  valid Location, which will be intrinsically converted to General to
   --  return an item count
   function Item_Count
     (Inventory : Global_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return API.Manifest.Quantity_Type;

   --  This accepts a Manifest Hash and searches the Vault for a matching item
   --  stack. The Hash should be of an item that is stackable, but this is not
   --  checked. Raises Item_Not_Found on failure.
   function Get_Vault_Item_Stack
     (Inventory : Global_Inventory_Type;
      Hash      : Manifest.Manifest_Hash)
      return Manifest.Tools.Item_Description;

   --  Same as above, but specifically searches for currency items instead of vault items.
   function Get_Currency_Item_Stack
     (Inventory : Global_Inventory_Type;
      Hash      : Manifest.Manifest_Hash)
      return Manifest.Tools.Item_Description;

   --  Access and Initialisation
   function Currency_Inventory
     (Inventory : Global_Inventory_Type) return Item_Description_List with
     Inline;

   function Vault_Inventory
     (Inventory : Global_Inventory_Type)
      return Item_Description_List_Bucket_Location_Type_Array with
     Inline;

   procedure Update_Inventory
     (Inventory : out Global_Inventory_Type;
      Profile   :     Profiles.Profile_Type;
      M         :     Manifest.Manifest_Type);
private
   type Global_Inventory_Type is record
      Currencies : Item_Description_List;
      Inventory  : Item_Description_List_Bucket_Location_Type_Array;
   end record;
end API.Inventories.Global;
