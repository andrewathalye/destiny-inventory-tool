pragma Ada_2022;

--  Local Packages
with API.Profiles;
with API.Manifest.Tools;
use type API.Manifest.Quantity_Type;

package API.Inventories.Character is
   --  Types
   type Character_Inventory_Type is new Destiny_Inventory_Type with private;
   type Character_Inventory_Array is
     array (Profiles.Character_Range) of Character_Inventory_Type;

   --  Subprograms
   --  Note: Descriptions are given only for non-overriding subprograms. See API.Inventories for more info.

   --------------------------
   -- Inventory Management --
   --------------------------
   overriding procedure Add
     (Inventory : in out Character_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description) with
     Inline;

   overriding procedure Remove
     (Inventory : in out Character_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description);

   overriding procedure Update
     (Inventory : out Character_Inventory_Type; Items : Item_Description_List);

   procedure Clear (Inventory : out Character_Inventory_Type);

   --  Add to local equipped inventory
   procedure Equip
     (Inventory : out Character_Inventory_Type;
      Item      :     Manifest.Tools.Item_Description);

   --  The "proper" way to update a character inventory, including both equipped items and inventory items
   procedure Update
     (Inventory    : out Character_Inventory_Type;
      Profile      :     Profiles.Profile_Type;
      The_Manifest :     Manifest.Manifest_Type;
      Character    :     Profiles.Character_Type);

   -------------
   -- Queries --
   -------------

   overriding function Item_Count
     (Inventory : Character_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return API.Manifest.Quantity_Type with
     Inline;

   overriding function Get
     (Inventory : Character_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return Item_Description_List with
     Inline;

   overriding function Get_Default
     (Inventory : Character_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return Item_Description_List renames
     Get;

   overriding function Get
     (Inventory : Character_Inventory_Type;
      Hash      : Manifest.Destiny_Inventory_Item_Definition_Manifest_Hash)
      return Manifest.Tools.Item_Description;

   overriding function Get_Sorted
     (Inventory : Character_Inventory_Type)
      return Item_Description_List_Bucket_Location_Type_Array with
     Inline;

   overriding function Get_Unsorted
     (Inventory : Character_Inventory_Type) return Item_Description_List;

   --  Returns a list of equipped items by slot
   function Get_Equipped
     (Inventory : Character_Inventory_Type)
      return Item_Description_Bucket_Location_Type_Array with
     Inline;

private
   type Character_Inventory_Type is new Destiny_Inventory_Type with record
      Character_Items : Item_Description_List_Bucket_Location_Type_Array;
      Equipped_Items  : Item_Description_Bucket_Location_Type_Array;
   end record;
end API.Inventories.Character;
