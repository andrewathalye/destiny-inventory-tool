pragma Ada_2022;

with API.Manifest.Tools; use API.Manifest.Tools;
use type API.Manifest.Item_Location_Type;
use type API.Manifest.Quantity_Type;
use type API.Manifest.Destiny_Inventory_Item_Definition_Manifest_Hash;

package body API.Inventories.Global is
   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Inventory : in out Global_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description)
   is
      Modified_Item : Manifest.Tools.Item_Description;
   begin
      Modified_Item.Location        := Manifest.Vault;
      Modified_Item.Bucket_Location :=
        Manifest.Tools.General; -- The item is now in the Vault
      Modified_Item.Bucket_Hash := Manifest.Tools.General'Enum_Rep; -- ^^

      Inventory.Inventory (Item.Default_Bucket_Location).Append
        (Modified_Item);
   end Add;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Inventory : in out Global_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description)
   is
   begin
      for I in
        Inventory.Inventory (Item.Default_Bucket_Location).First_Index ..
          Inventory.Inventory (Item.Default_Bucket_Location).Last_Index
      loop
         if Inventory.Inventory (Item.Default_Bucket_Location) (I) = Item then
            Inventory.Inventory (Item.Default_Bucket_Location).Delete (I);
            return;
         end if;
      end loop;

      raise Item_Not_Found;
   end Remove;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Inventory : out Global_Inventory_Type; Items : Item_Description_List)
   is
   begin
      for Item of Items loop
         Add (Inventory, Item);
      end loop;
   end Update;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Inventory : out Global_Inventory_Type) is
   begin
      for List of Inventory.Inventory loop
         List.Clear;
      end loop;

      Inventory.Currencies.Clear;
   end Clear;

   ---------------------
   -- Update_Currency --
   ---------------------

   procedure Update_Currency
     (Inventory : out Global_Inventory_Type; Items : Item_Description_List)
   is
   begin
      Inventory.Currencies := Items;
   end Update_Currency;

   ------------
   -- Update --
   ------------

   procedure Update
     (Inventory    : out Global_Inventory_Type;
      Profile      :     Profiles.Profile_Type;
      The_Manifest :     Manifest.Manifest_Type)
   is
   begin
      --  Clear old currency
      Inventory.Currencies.Clear;

      --  Clear any old vault items
      for IDL of Inventory.Inventory loop
         IDL.Clear;
      end loop;

      --  Load currency
      for I of Profile.Profile_Currencies loop
         Inventory.Currencies.Append
           (Manifest.Tools.Get_Description (The_Manifest, Profile, I));
      end loop;

      --  Load vault inventory
      for I of Profile.Profile_Inventory loop
         if I.Location = Manifest.Vault then
            declare
               D : constant Manifest.Tools.Item_Description :=
                 Manifest.Tools.Get_Description (The_Manifest, Profile, I);
            begin
               Inventory.Inventory (D.Default_Bucket_Location).Append (D);
            end;
         end if;
      end loop;
   end Update;

   ----------------
   -- Item_Count --
   ----------------

   overriding function Item_Count
     (Inventory : Global_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return API.Manifest.Quantity_Type
   is

      Count : API.Manifest.Quantity_Type := 0;

   begin
      case Location is
         when Consumable =>
            return
              API.Manifest.Quantity_Type
                (Inventory.Inventory (Consumable).Length);

         when Modification =>
            return
              API.Manifest.Quantity_Type
                (Inventory.Inventory (Modification).Length);

         when others =>
            for BLT in Bucket_Location_Type'Range loop
               case BLT is
                  when Consumable | Modification =>
                     null;
                  when others =>
                     Count :=
                       @ +
                       API.Manifest.Quantity_Type
                         (Inventory.Inventory (BLT).Length);
               end case;
            end loop;
            return Count;
      end case;
   end Item_Count;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Inventory : Global_Inventory_Type;
      Location  : API.Manifest.Tools.Bucket_Location_Type)
      return Item_Description_List
   is
      List : Item_Description_List;
   begin
      case Location is
         when Consumable | Modification =>
            return Inventory.Inventory (Location);
         when General =>
            Search_Lists :
               for BLT in Bucket_Location_Type'Range loop
                  case BLT is
                     when Consumable | Modification =>
                        null;
                     when others =>
                        List.Append (Inventory.Inventory (BLT));
                  end case;
               end loop Search_Lists;
         when others =>
            raise Item_Not_Found
              with "Global storage only stores Consumable, Modification, and General";
      end case;

      return List;
   end Get;

   -----------------
   -- Get_Default --
   -----------------

   overriding function Get_Default
     (Inventory : Global_Inventory_Type;
      Location  : API.Manifest.Tools.Bucket_Location_Type)
      return Item_Description_List is
     (Inventory.Inventory (Location));

   ---------
   -- Get --
   ---------

   overriding function Get
     (Inventory : Global_Inventory_Type;
      Hash      : Manifest.Destiny_Inventory_Item_Definition_Manifest_Hash)
      return Manifest.Tools.Item_Description
   is
   begin
      Search_Lists :
         for List of Inventory.Inventory loop
            Search_Items :
               for Item of List loop
                  if Item.Item_Hash = Hash then
                     return Item;
                  end if;
               end loop Search_Items;
         end loop Search_Lists;
      raise Item_Not_Found;
   end Get;

   ----------------
   -- Get_Sorted --
   ----------------

   overriding function Get_Sorted
     (Inventory : Global_Inventory_Type)
      return Item_Description_List_Bucket_Location_Type_Array is
     (Inventory.Inventory);

   ------------------
   -- Get_Unsorted --
   ------------------

   overriding function Get_Unsorted
     (Inventory : Global_Inventory_Type) return Item_Description_List
   is
      List : Item_Description_List;
   begin
      for Orig_List of Inventory.Inventory loop
         List.Append (Orig_List);
      end loop;

      return List;
   end Get_Unsorted;

   ------------------
   -- Get_Currency --
   ------------------

   function Get_Currency
     (Inventory : Global_Inventory_Type) return Item_Description_List is
     (Inventory.Currencies);
end API.Inventories.Global;
