--  Local Packages
with Shared.Debug; use Shared;

package body API.Inventories.Character is
   --  Public Subprograms
   --  (Virtual) Inventory Management

   procedure Add_Item
     (Inventory : in out Character_Inventory_Type;
      Character :        Profiles.Character_Type;
      Item      :        Manifest.Tools.Item_Description)
   is

      Character_Items :
        Item_Description_List_Bucket_Location_Type_Array renames
        Inventory.All_Character_Items (Character.Character_ID);
      Relevant_Items :
        Item_Description_List renames
        Character_Items (Item.Default_Bucket_Location);
      Modified_Item : Manifest.Tools.Item_Description := Item;

   begin
      Modified_Item.Location        := Manifest.Inventory;
      Modified_Item.Bucket_Location := Item.Default_Bucket_Location;
      --  Item is not vaulted (any longer), so ensure Bucket_Location is
      --  updated
      Modified_Item.Bucket_Hash     := Item.Default_Bucket_Hash;
      Modified_Item.Transfer_Status := Profiles.Can_Transfer;
      Relevant_Items.Append (Modified_Item);
   end Add_Item;

   procedure Remove_Item
     (Inventory : in out Character_Inventory_Type;
      Character :        Profiles.Character_Type;
      Item      :        Manifest.Tools.Item_Description)
   is

      Character_Items :
        Item_Description_List_Bucket_Location_Type_Array renames
        Inventory.All_Character_Items (Character.Character_ID);
      Relevant_Items :
        Item_Description_List renames Character_Items (Item.Bucket_Location);

   begin
      for I in Relevant_Items.First_Index .. Relevant_Items.Last_Index loop
         if Relevant_Items (I) = Item then
            Relevant_Items.Delete (I);
            return;
         end if;
      end loop;
      raise Program_Error
        with "API.Inventories.Character: Virtual remove failed";
   end Remove_Item;

   procedure Equip_Item
     (Inventory : in out Character_Inventory_Type;
      Character :        Profiles.Character_Type;
      Item      :        Manifest.Tools.Item_Description)
   is

      Equipped_Items :
        Item_Description_Bucket_Location_Type_Array renames
        Inventory.All_Equipped_Items (Character.Character_ID);
      Relevant_Item :
        Manifest.Tools.Item_Description renames
        Equipped_Items (Item.Bucket_Location);

   begin
      --  Virtually transfer currently-equipped item to bucket
      Relevant_Item.Transfer_Status := Profiles.Can_Transfer;
      Add_Item (Inventory, Character, Relevant_Item);
      Relevant_Item                 := Item;
      Relevant_Item.Transfer_Status := Profiles.Item_Is_Equipped;
   end Equip_Item;

   function Item_Count
     (Inventory : Character_Inventory_Type;
      Character : Profiles.Character_Type;
      Location  : Manifest.Tools.Bucket_Location_Type)
      return API.Manifest.Quantity_Type is
     (API.Manifest.Quantity_Type
        (Inventory.All_Character_Items (Character.Character_ID) (Location)
           .Length));

   --  Access to Item Data
   function Character_Items
     (Inventory : Character_Inventory_Type;
      Character : Profiles.Character_Type)
      return Item_Description_List_Bucket_Location_Type_Array is
     (Inventory.All_Character_Items (Character.Character_ID));
   function Equipped_Items
     (Inventory : Character_Inventory_Type;
      Character : Profiles.Character_Type)
      return Item_Description_Bucket_Location_Type_Array is
     (Inventory.All_Equipped_Items (Character.Character_ID));

   --  Update inventory data for characters

   procedure Update_Inventory
     (Inventory : out Character_Inventory_Type;
      Profile   :     Profiles.Profile_Type;
      M         :     Manifest.Manifest_Type)
   is

      C1 : Item_Description_List_Bucket_Location_Type_Array_Maps.Cursor;
      C2 : Item_Description_Bucket_Location_Type_Array_Maps.Cursor;
      B  : Boolean;

   begin
      Debug.Put_Line ("Update character inventories");
      --  Clear existing data
      Inventory.All_Character_Items.Clear;
      Inventory.All_Equipped_Items.Clear;
      --  Add character data
      Add_Characters :
         for C of Profile.Characters loop
            Inventory.All_Character_Items.Insert (C.Character_ID, C1, B);
            Inventory.All_Equipped_Items.Insert (C.Character_ID, C2, B);
            --  Inventory Items (not equipped)

            for I of Profile.Character_Inventories (C.Character_ID) loop
               declare

                  D : constant Manifest.Tools.Item_Description :=
                    Manifest.Tools.Get_Description (M, Profile, I);

               begin
                  Inventory.All_Character_Items (C.Character_ID)
                    (D.Bucket_Location)
                    .Append
                    (D);
               end;
            end loop;

            for I of Profile.Character_Equipment (C.Character_ID) loop
               declare

                  D : constant Manifest.Tools.Item_Description :=
                    Manifest.Tools.Get_Description (M, Profile, I);

               begin
                  Inventory.All_Equipped_Items (C.Character_ID)
                    (D.Bucket_Location) :=
                    D;
               end;
            end loop;
         end loop Add_Characters;
   end Update_Inventory;

end API.Inventories.Character;
