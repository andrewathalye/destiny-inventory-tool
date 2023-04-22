with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;

-- Local Packages
with API.Profiles;
with API.Manifest.Tools;

with Shared.Strings;

package API.Inventories.Character is
   -- Types
   type Character_Inventory_Type is private;

   -- Subprograms
   -- Inventory Management
   procedure Add_Item
     (Inventory : in out Character_Inventory_Type;
      Character :        Profiles.Character_Type;
      Item      :        Manifest.Tools.Item_Description) with
     Inline;

   procedure Remove_Item
     (Inventory : in out Character_Inventory_Type;
      Character :        Profiles.Character_Type;
      Item      :        Manifest.Tools.Item_Description);

   procedure Equip_Item
     (Inventory : in out Character_Inventory_Type;
      Character :        Profiles.Character_Type;
      Item      :        Manifest.Tools.Item_Description);

   function Item_Count
     (Inventory : Character_Inventory_Type;
      Character : Profiles.Character_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return Natural;

   -- Initialisation and Access
   function Character_Items
     (Inventory : Character_Inventory_Type;
      Character : Profiles.Character_Type)
      return Item_Description_List_Bucket_Location_Type_Array with
     Inline;

   function Equipped_Items
     (Inventory : Character_Inventory_Type;
      Character : Profiles.Character_Type)
      return Item_Description_Bucket_Location_Type_Array with
     Inline;

   procedure Update_Inventory
     (Inventory : out Character_Inventory_Type;
      Profile   :     Profiles.Profile_Type;
      M         :     Manifest.Manifest_Type);
private
   package Item_Description_List_Bucket_Location_Type_Array_Maps is new Ada
     .Containers
     .Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Item_Description_List_Bucket_Location_Type_Array,
      Hash            => Shared.Strings.Hash,
      Equivalent_Keys => Shared.Strings.Equivalent_Keys);

   package Item_Description_Bucket_Location_Type_Array_Maps is new Ada
     .Containers
     .Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Item_Description_Bucket_Location_Type_Array,
      Hash            => Shared.Strings.Hash,
      Equivalent_Keys => Shared.Strings.Equivalent_Keys);

   subtype Item_Description_List_Bucket_Location_Type_Array_Map is
     Item_Description_List_Bucket_Location_Type_Array_Maps.Map;
   subtype Item_Description_Bucket_Location_Type_Array_Map is
     Item_Description_Bucket_Location_Type_Array_Maps.Map;

   type Character_Inventory_Type is record
      All_Character_Items : Item_Description_List_Bucket_Location_Type_Array_Map;
      All_Equipped_Items  : Item_Description_Bucket_Location_Type_Array_Map;
   end record;
end API.Inventories.Character;
