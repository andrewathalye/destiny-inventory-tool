separate (API.Inventories.Character)
procedure Equip
   (Inventory : out Character_Inventory_Type;
    Item      :     Manifest.Tools.Item_Description)
is
begin
   Add (Inventory, (Inventory.Equipped_Items (Item.Bucket_Location) with delta
      Transfer_Status => Profiles.Can_Transfer));

   Inventory.Equipped_Items (Item.Bucket_Location) := (Item with delta
      Transfer_Status => Profiles.Item_Is_Equipped);
end Equip;

