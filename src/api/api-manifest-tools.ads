with API.Profiles; use API.Profiles;

package API.Manifest.Tools is
   type Bucket_Location_Type is
     (Unknown,
      Chest,
      Leg,
      General,
      Postmaster,
      Ship,
      Engram,
      Clan_Bounty, -- Unofficial Name
      Clan_Activity, -- Unofficial Name
      Power,
      Emote_Collection,
      Quest,
      Consumable,
      Kinetic,
      Artefact,
      Class,
      Quest_Step, -- Unofficial Name
      Sparrow,
      Emote,
      Dummy_A, -- TODO specify
      Energy,
      Subclass,
      Modification,
      Helmet,
      Gauntlets,
      Dummy_B, -- TODO specify
      Finisher,
      Shell,
      Emblem,
      Clan_Banner);

   for Bucket_Location_Type use
     (Unknown          => 0,
      Chest            => 14_239_492,
      Leg              => 20_886_954,
      General          => 138_197_802,
      Postmaster       => 215_593_132,
      Ship             => 284_967_655,
      Engram           => 375_726_501,
      Clan_Bounty      => 444_348_033,
      Clan_Activity    => 497_170_007,
      Power            => 953_998_645,
      Emote_Collection => 1_107_761_855,
      Quest            => 1_345_459_588,
      Consumable       => 1_469_714_392,
      Kinetic          => 1_498_876_634,
      Artefact         => 1_506_418_338,
      Class            => 1_585_787_867,
      Quest_Step       => 1_801_258_597,
      Sparrow          => 2_025_709_351,
      Emote            => 2_401_704_334,
      Dummy_A          => 2_422_292_810,
      Energy           => 2_465_295_065,
      Subclass         => 3_284_755_031,
      Modification     => 3_313_201_758,
      Helmet           => 3_448_274_439,
      Gauntlets        => 3_551_918_588,
      Dummy_B          => 3_621_873_013,
      Finisher         => 3_683_254_069,
      Shell            => 4_023_194_814,
      Emblem           => 4_274_335_291,
      Clan_Banner      => 4_292_445_962);

   --  Intended to store sufficient information about an item to display it
   --  without further Manifest lookups
   --
   --  Location, Bucket_Location, Bucket_Hash, and Transfer_Status should be
   --  modified if the item is to be virtually moved

   type Item_Description is record
      Name        : Unbounded_String;
      Description : Unbounded_String;
      Item_Hash   : Manifest_Hash;
      --  DestinyInventoryItemDefinition
      Item_Instance_ID : Unbounded_String;

      Quantity       : Integer_32;
      Max_Stack_Size : Integer_32;
      Location       : Item_Location_Type;

      Bucket_Hash, Default_Bucket_Hash : Manifest_Hash;
      --  DestinyInventoryBucketDefinition
      Bucket_Location, Default_Bucket_Location : Bucket_Location_Type;

      Category        : Destiny_Inventory_Bucket_Category;
      State           : Item_State_Type;
      Allow_Actions   : Boolean;
      Transfer_Status : Transfer_Status_Type;

      Icon_Path      : Unbounded_String;
      Watermark_Path : Unbounded_String;

      Style_Overridden : Boolean;

      Postmaster_Pull_Has_Side_Effects : Boolean;
      Item_Type                        : Destiny_Item_Type;
      Tier_Type                        : Destiny_Tier_Type;
      Item_Type_And_Tier_Display_Name  : Unbounded_String;
   end record;

   function Get_Description
     (M : Manifest_Type; C : Character_Type) return String;

   function Get_Description
     (M : Manifest_Type; I : Item_Type) return Item_Description;

   function Get_Title
     (M : Manifest_Type; C : Character_Type) return Unbounded_String;
end API.Manifest.Tools;