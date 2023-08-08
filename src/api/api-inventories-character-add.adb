separate (API.Inventories.Character)
overriding procedure Add
     (Inventory : in out Character_Inventory_Type;
      Item      :        Manifest.Tools.Item_Description)
is
begin
   --  Item is not vaulted (any longer), so ensure Bucket_Location is
   --  updated. The item is also no longer equipped, so it can be transferred
   --  once again.
   Inventory.Character_Items (Item.Default_Bucket_Location).Append (
    Manifest.Tools.Item_Description'(
       Item with delta Location => Manifest.Inventory,
       Bucket_Location => Item.Default_Bucket_Location,
       Bucket_Hash => Item.Default_Bucket_Hash,
       Transfer_Status => Profiles.Can_Transfer));
end Add;

