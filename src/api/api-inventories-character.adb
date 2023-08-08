pragma Ada_2022;

with Shared.Debug; use Shared.Debug;
with API.Manifest;
use type API.Manifest.Destiny_Inventory_Item_Definition_Manifest_Hash;

package body API.Inventories.Character is
   ---------
   -- Add --
   ---------

   --  Note: Linter currently crashes on delta aggregates, so they are separated out for now
   overriding procedure Add
     (Inventory : in out Character_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description)
   is separate;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Inventory : in out Character_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description)
   is
   begin
      for List of Inventory.Character_Items loop
         for I in List.First_Index .. List.Last_Index loop
            if List (I) = Item then
               List.Delete (I);
               return;
            end if;
         end loop;
      end loop;

      raise Item_Not_Found;
   end Remove;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Inventory : out Character_Inventory_Type; Items : Item_Description_List)
   is
   begin
      for Item of Items loop
         Add (Inventory, Item);
      end loop;
   end Update;

   -----------
   -- Clear --
   -----------

   procedure Clear (Inventory : out Character_Inventory_Type) is
   begin
      for List of Inventory.Character_Items loop
         List.Clear;
      end loop;
   end Clear;

   -----------
   -- Equip --
   -----------

   --  Note: See above with "Add"
   procedure Equip
     (Inventory : out Character_Inventory_Type;
      Item      :     Manifest.Tools.Item_Description)
   is separate;

   ------------
   -- Update --
   ------------

   procedure Update
     (Inventory    : out Character_Inventory_Type;
      Profile      :     Profiles.Profile_Type;
      The_Manifest :     Manifest.Manifest_Type;
      Character    :     Profiles.Character_Type)
   is
   begin
      Put_Line ("Update character inventory");

      --  Clear existing data
      for List of Inventory.Character_Items loop
         List.Clear;
      end loop;

      --  Inventory Items (not equipped)
      for I of Profile.Character_Inventories (Character.Character_ID) loop
         declare

            D : constant Manifest.Tools.Item_Description :=
              Manifest.Tools.Get_Description (The_Manifest, Profile, I);

         begin
            Inventory.Character_Items (D.Bucket_Location).Append (D);
         end;
      end loop;

      for I of Profile.Character_Equipment (Character.Character_ID) loop
         declare

            D : constant Manifest.Tools.Item_Description :=
              Manifest.Tools.Get_Description (The_Manifest, Profile, I);

         begin
            Inventory.Equipped_Items (D.Bucket_Location) := D;
         end;
      end loop;
   end Update;

   ----------------
   -- Item_Count --
   ----------------

   overriding function Item_Count
     (Inventory : Character_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return API.Manifest.Quantity_Type
   is
   begin
      return
        API.Manifest.Quantity_Type
          (Inventory.Character_Items (Location).Length);
   end Item_Count;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Inventory : Character_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return Item_Description_List
   is
   begin
      return Inventory.Character_Items (Location);
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Inventory : Character_Inventory_Type;
      Hash      : Manifest.Destiny_Inventory_Item_Definition_Manifest_Hash)
      return Manifest.Tools.Item_Description
   is
   begin
           for List of Inventory.Character_Items loop
              for Item of List loop
                      if Item.Item_Hash = Hash then
                              return Item;
                      end if;
              end loop;
           end loop;

           raise Item_Not_Found;
   end Get;

   ----------------
   -- Get_Sorted --
   ----------------

   overriding function Get_Sorted
     (Inventory : Character_Inventory_Type)
      return Item_Description_List_Bucket_Location_Type_Array
   is
   begin
      return Inventory.Character_Items;
   end Get_Sorted;

   ------------------
   -- Get_Unsorted --
   ------------------

   overriding function Get_Unsorted
     (Inventory : Character_Inventory_Type) return Item_Description_List
   is
      List : Item_Description_List;
   begin
           for Orig_List of Inventory.Character_Items loop
                   List.Append (Orig_List);
           end loop;

           return List;
   end Get_Unsorted;

   ------------------
   -- Get_Equipped --
   ------------------

   function Get_Equipped
     (Inventory : Character_Inventory_Type)
      return Item_Description_Bucket_Location_Type_Array
   is
   begin
      return Inventory.Equipped_Items;
   end Get_Equipped;

end API.Inventories.Character;
