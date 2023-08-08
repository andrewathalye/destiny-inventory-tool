pragma Ada_2022;

--  Local Packages
with API.Manifest.Tools;
with API.Profiles;

package API.Inventories.Global is
   --  The Vault ("Global") stores three kinds of items: General Consumables Modifications
   --
   --  General items are stored in the inventory by their default bucket
   --  locations rather than by their actual location (Manifest.Tools.General)

   --  Types
   type Global_Inventory_Type is new Destiny_Inventory_Type with private;

   --  Subprograms
   --  Note: For overriding subprograms, see API.Inventories for exception information.

   --------------------------
   -- Inventory Management --
   --------------------------
   overriding procedure Add
     (Inventory : in out Global_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description) with
     Inline;

   overriding procedure Remove
     (Inventory : in out Global_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description);

   overriding procedure Update
     (Inventory : out Global_Inventory_Type; Items : Item_Description_List);

   overriding procedure Clear (Inventory : out Global_Inventory_Type);

   --  Local Additions
   --  Updates the Currency sub-inventory using a premade list
   procedure Update_Currency
     (Inventory : out Global_Inventory_Type; Items : Item_Description_List);

   --  Updates all inventories using raw data (preferred method for external interface)
   procedure Update
     (Inventory    : out Global_Inventory_Type;
      Profile      :     Profiles.Profile_Type;
      The_Manifest :     Manifest.Manifest_Type);

   -------------
   -- Queries --
   -------------
   --  General, Consumables, and Modifications are valid locations for Global Inventories

   overriding function Item_Count
     (Inventory : Global_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return API.Manifest.Quantity_Type with
     Inline;

   overriding function Get
     (Inventory : Global_Inventory_Type;
      Location  : API.Manifest.Tools.Bucket_Location_Type)
      return Item_Description_List;

   overriding function Get_Default
     (Inventory : Global_Inventory_Type;
      Location  : API.Manifest.Tools.Bucket_Location_Type)
      return Item_Description_List with
     Inline;

   overriding function Get
     (Inventory : Global_Inventory_Type;
      Hash      : Manifest.Destiny_Inventory_Item_Definition_Manifest_Hash)
      return Manifest.Tools.Item_Description;

   overriding function Get_Sorted
     (Inventory : Global_Inventory_Type)
      return Item_Description_List_Bucket_Location_Type_Array with
     Inline;

   overriding function Get_Unsorted
     (Inventory : Global_Inventory_Type) return Item_Description_List;

   --  Returns the contents of the global currency inventory
   function Get_Currency
     (Inventory : Global_Inventory_Type) return Item_Description_List with
     Inline;

private
   type Global_Inventory_Type is new Destiny_Inventory_Type with record
      Currencies : Item_Description_List;
      Inventory  : Item_Description_List_Bucket_Location_Type_Array;
   end record;
end API.Inventories.Global;
