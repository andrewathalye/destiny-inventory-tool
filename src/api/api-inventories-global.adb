pragma Ada_2022;
with Interfaces; use Interfaces;

--  Local Packages
with API.Manifest.Tools;
use type API.Manifest.Item_Location_Type;
use all type API.Manifest.Tools.Bucket_Location_Type;

package body API.Inventories.Global is
   --  (Virtual) Inventory Management

   procedure Add_Item
     (Inventory : in out Global_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description)
   is

      Modified_Item : Manifest.Tools.Item_Description := Item;

   begin
      Modified_Item.Location        := Manifest.Vault;
      Modified_Item.Bucket_Location :=
        Manifest.Tools.General; -- The item is now in the Vault
      Modified_Item.Bucket_Hash     := Manifest.Tools.General'Enum_Rep; -- ^^
      Modified_Item.Transfer_Status := Profiles.Can_Transfer;
      Inventory.Inventory (Item.Default_Bucket_Location).Append
        (Modified_Item);
   end Add_Item;

   procedure Remove_Item
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
      raise Program_Error with "API.Inventories.Global: Virtual remove failed";
   end Remove_Item;
   --  The vault is divided into different types as well, with most items being
   --  put in the General category

   function Item_Count
     (Inventory : Global_Inventory_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return Natural
   is

      Count : Natural := 0;

   begin
      case Location is
         when Consumable =>
            return Natural (Inventory.Inventory (Consumable).Length);

         when Modification =>
            return Natural (Inventory.Inventory (Modification).Length);

         when others =>
            --  Can't just check (General).Length because the items are
            --  organised by their default Bucket_Locations for convenience
            --  elsewhere
            for IDL of Inventory.Inventory loop
               for D of IDL loop
                  if D.Bucket_Location = General then
                     Count := @ + 1;
                  end if;
               end loop;
            end loop;
            return Count;
      end case;
   end Item_Count;

   function Get_Item_Stack
     (Inventory : Global_Inventory_Type;
      Hash      : Manifest.Manifest_Hash)
      return Manifest.Tools.Item_Description
   is
   begin
      for IDL of Inventory.Inventory loop
         for ID of IDL loop
            if ID.Item_Hash = Hash then
               return ID;
            end if;
         end loop;
      end loop;
      raise Item_Not_Found;
   end Get_Item_Stack;
   --  Inventory Updates and Access
   function Vault_Inventory
     (Inventory : Global_Inventory_Type)
      return Item_Description_List_Bucket_Location_Type_Array is
     (Inventory.Inventory);

   procedure Update_Inventory
     (Inventory : out Global_Inventory_Type;
      Profile   :     Profiles.Profile_Type;
      M         :     Manifest.Manifest_Type)
   is
   begin
      --  Clear any old vault items
      for IDL of Inventory.Inventory loop
         IDL.Clear;
      end loop;
      --  Load vault inventory

      for I of Profile.Profile_Inventory loop
         if I.Location = Manifest.Vault then
            declare

               D : constant Manifest.Tools.Item_Description :=
                 Manifest.Tools.Get_Description (M, I);

            begin
               Inventory.Inventory (D.Default_Bucket_Location).Append (D);
            end;
         end if;
      end loop;
   end Update_Inventory;

end API.Inventories.Global;
