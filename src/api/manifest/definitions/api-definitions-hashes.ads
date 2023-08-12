package API.Definitions.Hashes is
   ------------
   -- Hashes --
   ------------
   --  Note: Use this base type only when a uniform type must be used for multiple
   --  Manifest Hash varieties. Bungie.Net does not guarantee that Manifest Hashes
   --  are unique across different definition categories, and as such this is unsafe
   --  to assume.

   subtype Base_Manifest_Hash is Unsigned_32;

   type Destiny_Activity_Definition_Manifest_Hash is new Base_Manifest_Hash;
   type Destiny_Class_Definition_Manifest_Hash is new Base_Manifest_Hash;

   type Destiny_Damage_Type_Definition_Manifest_Hash is new Base_Manifest_Hash;
   type Destiny_Destination_Definition_Manifest_Hash is new Base_Manifest_Hash;

   type Destiny_Faction_Definition_Manifest_Hash is new Base_Manifest_Hash;
   type Destiny_Gender_Definition_Manifest_Hash is new Base_Manifest_Hash;

   type Destiny_Inventory_Bucket_Definition_Manifest_Hash is
     new Base_Manifest_Hash;
   type Destiny_Inventory_Item_Definition_Manifest_Hash is
     new Base_Manifest_Hash;

   type Destiny_Loadout_Color_Definition_Manifest_Hash is
     new Base_Manifest_Hash; --  TODO unimplemented
   type Destiny_Loadout_Icon_Definition_Manifest_Hash is
     new Base_Manifest_Hash; --  TODO unimplemented
   type Destiny_Loadout_Name_Definition_Manifest_Hash is
     new Base_Manifest_Hash; --  TODO unimplemented

   type Destiny_Objective_Definition_Manifest_Hash is new Base_Manifest_Hash;
   type Destiny_Stat_Definition_Manifest_Hash is new Base_Manifest_Hash;

   type Destiny_Place_Definition_Manifest_Hash is new Base_Manifest_Hash;
   type Destiny_Progression_Definition_Manifest_Hash is
     new Base_Manifest_Hash; --  TODO unimplemented

   type Destiny_Race_Definition_Manifest_Hash is new Base_Manifest_Hash;
   type Destiny_Record_Definition_Manifest_Hash is new Base_Manifest_Hash;

   type Destiny_Sandbox_Perk_Definition_Manifest_Hash is
     new Base_Manifest_Hash; -- TODO unimplemented
   type Destiny_Socket_Type_Definition_Manifest_Hash is
     new Base_Manifest_Hash; --  TODO unimplemented
   type Destiny_Vendor_Definition_Manifest_Hash is new Base_Manifest_Hash;

   type Destiny_Vendor_Group_Definition_Manifest_Hash is
     new Base_Manifest_Hash;
end API.Definitions.Hashes;
