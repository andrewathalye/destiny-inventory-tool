with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Interfaces;            use Interfaces;

package API.Manifest is
   --  Types
   -----------
   -- BASIC --
   -----------
   subtype Manifest_Hash is Unsigned_32;
   package Manifest_Hash_Lists is new Ada.Containers.Vectors
     (Natural, Manifest_Hash);
   subtype Manifest_Hash_List is Manifest_Hash_Lists.Vector;

   type Quantity_Type is range -1 .. Integer_32'Last;

   package USL is new Ada.Containers.Vectors (Natural, Unbounded_String);
   subtype Unbounded_String_List is USL.Vector;

   -----------------------------
   -- DestinyGenderDefinition --
   -----------------------------
   type Destiny_Gender_Type is (Male, Female);

   type Destiny_Gender_Definition is record
      Gender_Type : Destiny_Gender_Type;
      Gender_Name : Unbounded_String;
   end record;
   package DGDM is new Ada.Containers.Ordered_Maps
     (Key_Type => Manifest_Hash, Element_Type => Destiny_Gender_Definition);
   subtype Destiny_Gender_Map is DGDM.Map;

   ---------------------------
   -- DestinyRaceDefinition --
   ---------------------------
   type Destiny_Race_Name is array (Destiny_Gender_Type) of Unbounded_String;
   package DRNM is new Ada.Containers.Ordered_Maps
     (Key_Type => Manifest_Hash, Element_Type => Destiny_Race_Name);
   subtype Destiny_Race_Map is DRNM.Map;

   ----------------------------
   -- DestinyClassDefinition --
   ----------------------------
   type Destiny_Class_Name is array (Destiny_Gender_Type) of Unbounded_String;
   package DCNM is new Ada.Containers.Ordered_Maps
     (Key_Type => Manifest_Hash, Element_Type => Destiny_Class_Name);
   subtype Destiny_Class_Map is DCNM.Map;

   -----------------------------
   -- DestinyRecordDefinition --
   -----------------------------
   type Destiny_Title_Name is array (Destiny_Gender_Type) of Unbounded_String;
   package DTNM is new Ada.Containers.Ordered_Maps
     (Key_Type => Manifest_Hash, Element_Type => Destiny_Title_Name);
   subtype Destiny_Title_Map is DTNM.Map;

   ------------------------------------
   -- DestinyInventoryItemDefinition --
   ------------------------------------
   type Destiny_Tier_Type is
     (Unknown, Currency, Basic, Common, Rare, Superior, Exotic);

   type Destiny_Item_Type is
     (None,
      Currency,
      Armour,
      Weapon,
      Message,
      Engram,
      Consumable,
      Exchange_Material,
      Mission_Reward,
      Quest_Step,
      Quest_Step_Complete,
      Emblem,
      Quest,
      Subclass,
      Clan_Banner,
      Aura,
      DIT_Mod,
      Dummy,
      Ship,
      Vehicle,
      Emote,
      Ghost,
      DIT_Package,
      Bounty,
      Wrapper,
      Seasonal_Artefact,
      Finisher,
      Pattern);
   for Destiny_Item_Type use
     (None                => 0,
      Currency            => 1,
      Armour              => 2,
      Weapon              => 3,
      Message             => 7,
      Engram              => 8,
      Consumable          => 9,
      Exchange_Material   => 10,
      Mission_Reward      => 11,
      Quest_Step          => 12,
      Quest_Step_Complete => 13,
      Emblem              => 14,
      Quest               => 15,
      Subclass            => 16,
      Clan_Banner         => 17,
      Aura                => 18,
      DIT_Mod             => 19,
      Dummy               => 20,
      Ship                => 21,
      Vehicle             => 22,
      Emote               => 23,
      Ghost               => 24,
      DIT_Package         => 25,
      Bounty              => 26,
      Wrapper             => 27,
      Seasonal_Artefact   => 28,
      Finisher            => 29,
      Pattern             => 30);

   type Destiny_Inventory_Item_Definition is record
      Description                      : Unbounded_String;
      Name                             : Unbounded_String;
      Icon_Path                        : Unbounded_String;
      Watermark_Path                   : Unbounded_String;
      Shelved_Watermark_Path           : Unbounded_String;
      Item_Type_And_Tier_Display_Name  : Unbounded_String;
      Max_Stack_Size                   : Quantity_Type;
      Bucket_Type_Hash                 : Manifest_Hash;
      Tier_Type                        : Destiny_Tier_Type;
      Display_Version_Watermark_Icons  : Unbounded_String_List;
      Allow_Actions                    : Boolean;
      Postmaster_Pull_Has_Side_Effects : Boolean;
      Item_Type                        : Destiny_Item_Type;
      Default_Damage_Type_Hash         : Manifest_Hash := 0; -- Nullable
      --  DestinyDamageTypeDefinition
   end record;

   package DIIDM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Manifest_Hash,
      Element_Type => Destiny_Inventory_Item_Definition);
   subtype Destiny_Inventory_Item_Map is DIIDM.Map;

   ---------------------------------
   -- DestinyDamageTypeDefinition --
   ---------------------------------
   type Destiny_Damage_Type_Definition is record
      Description : Unbounded_String;
      Name        : Unbounded_String;
      Icon_Path   : Unbounded_String;
      Show_Icon   : Boolean;
   end record;

   package DDTDM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Manifest_Hash,
      Element_Type => Destiny_Damage_Type_Definition);
   subtype Destiny_Damage_Type_Map is DDTDM.Map;

   --------------------------------------
   -- DestinyInventoryBucketDefinition --
   --------------------------------------
   type Destiny_Inventory_Bucket_Category is
     (Invisible, Item, Currency, Equippable, Ignored);

   type Item_Location_Type is (Unknown, Inventory, Vault, Vendor, Postmaster);
   --  Note: "Postmaster" seems to not be used anymore, so check the bucket
   --  location instead

   type Destiny_Inventory_Bucket_Definition is record
      Description  : Unbounded_String;
      Name         : Unbounded_String;
      Category     : Destiny_Inventory_Bucket_Category;
      Bucket_Order : Integer_32;
      Item_Count   : Quantity_Type;
      Location     : Item_Location_Type;
      FIFO         : Boolean;
   end record;
   package DIBDM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Manifest_Hash,
      Element_Type => Destiny_Inventory_Bucket_Definition);
   subtype Destiny_Inventory_Bucket_Map is DIBDM.Map;

   --------------------------------
   -- DestinyObjectiveDefinition --
   --------------------------------
   type Destiny_Objective_Definition is record
      Icon_Path            : Unbounded_String; --  Nullable
      Progress_Description : Unbounded_String; --  Nullable
   end record;

   package DODM is new Ada.Containers.Ordered_Maps
     (Key_Type => Manifest_Hash, Element_Type => Destiny_Objective_Definition);
   subtype Destiny_Objective_Map is DODM.Map;

   ---------------------------
   -- DestinyStatDefinition --
   ---------------------------
   --  At the moment, only the name of the stat is computed / stored
   package Destiny_Stat_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Manifest_Hash, Element_Type => Unbounded_String);
   subtype Destiny_Stat_Map is Destiny_Stat_Maps.Map;

   -----------------------------
   -- DestinyVendorDefinition --
   -----------------------------
   type Vendor_Item_Index_Type is new Natural;
   type Failure_Index_Type is new Natural;
   type Display_Category_Index_Type is new Integer range -1 .. Integer'Last;

   package Failure_Index_Lists is new Ada.Containers.Vectors
     (Natural, Failure_Index_Type);
   subtype Failure_Index_List is Failure_Index_Lists.Vector;

   package Failure_String_Lists is new Ada.Containers.Vectors
     (Failure_Index_Type, Unbounded_String);
   subtype Failure_String_List is Failure_String_Lists.Vector;

   --  At this time, no other information is needed
   --  but that might change.
   type Destiny_Display_Category_Definition is record
      Name : Unbounded_String;
   end record;

   package DDCDM is new Ada.Containers.Ordered_Maps
     (Display_Category_Index_Type, Destiny_Display_Category_Definition);
   subtype Destiny_Display_Category_Map is DDCDM.Map;

   type Destiny_Vendor_Item_Socket_Override_Type is record
      Single_Item_Hash : Manifest_Hash := 0; --  Nullable
      --  Mapped to DestinyItemDefinition
      Randomized_Options_Count : Quantity_Type;
      Socket_Type_Hash         : Manifest_Hash;
      --  Mapped to TODO unimplemented DestinySocketTypeDefinition
   end record;

   package DVISOL is new Ada.Containers.Vectors
     (Natural, Destiny_Vendor_Item_Socket_Override_Type);
   subtype Destiny_Vendor_Item_Socket_Override_List is DVISOL.Vector;

   type Destiny_Vendor_Item_Definition is record
      Item_Hash : Manifest_Hash;
      --  DestinyInventoryItemDefinition
      Quantity        : Quantity_Type;
      Failure_Indexes : Failure_Index_List;
      --  Currencies? IMO best to avoid because live data is better here
      Display_Category_Index : Display_Category_Index_Type;
      --  Points to a Destiny_Display_Category_Definition within Display_Categories
      --  Category_Index?
      Socket_Overrides : Destiny_Vendor_Item_Socket_Override_List;
   end record;

   package DVIDM is new Ada.Containers.Ordered_Maps
     (Vendor_Item_Index_Type, Destiny_Vendor_Item_Definition);
   subtype Destiny_Vendor_Item_Map is DVIDM.Map;

   type Destiny_Vendor_Definition is record
      Large_Icon_Path : Unbounded_String; --  Nullable
      Subtitle        : Unbounded_String;
      --  largeTransparentIcon?
      Description       : Unbounded_String;
      Name              : Unbounded_String;
      Icon_Path         : Unbounded_String; --  Nullable
      Display_Item_Hash : Manifest_Hash; --  Nullable
      --  Linked to DestinyInventoryItemDefinition
      Inhibit_Buying  : Boolean;
      Inhibit_Selling : Boolean;
      Faction_Hash    : Manifest_Hash := 0; --  Nullable
      --  Linked to DestinyFactionDefinition TODO unimplemented parsing
      Failure_Strings : Failure_String_List;
      --  Vendor_Portrait? Vendor_Banner?
      Enabled : Boolean;
      Visible : Boolean;
      --  Categories?
      Display_Categories : Destiny_Display_Category_Map;
      --  Indexed by Hash
      Items : Destiny_Vendor_Item_Map;
      --  Indexed by Vendor Item Index (may be ignored if not needed)
      Group : Manifest_Hash := 0; --  Nullable
      --  Linked to DestinyVendorGroupDefinition TODO unimplemented parsing
      --  Note: Theoretically there can be multiple, but according to the API spec
      --  only one group may be attached to a vendor at a time.
      Ignore_Sale_Hashes : Manifest_Hash_List;
   end record;

   package DVDM is new Ada.Containers.Ordered_Maps
     (Manifest_Hash, Destiny_Vendor_Definition);
   subtype Destiny_Vendor_Map is DVDM.Map;

   -------------
   -- CENTRAL --
   -------------
   --  Available Manifest fields (no longer sorted)
   --  Most data types do not include all available information
   --  See individual defintions for more information

   type Manifest_Type is record
      Destiny_Classes : Destiny_Class_Map;
      --  DestinyClassDefinition
      Destiny_Genders : Destiny_Gender_Map;
      --  DestinyGenderDefinition
      Destiny_Inventory_Buckets : Destiny_Inventory_Bucket_Map;
      --  DestinyInventoryBucketDefinition
      Destiny_Races : Destiny_Race_Map;
      --  DestinyRaceDefinition
      Destiny_Stats : Destiny_Stat_Map;
      --  DestinyStatDefinition
      Destiny_Damage_Types : Destiny_Damage_Type_Map;
      --  DestinyDamageTypeDefinition
      Destiny_Inventory_Items : Destiny_Inventory_Item_Map;
      --  DestinyInventoryItemDefinition
      Destiny_Objectives : Destiny_Objective_Map;
      --  DestinyObjectiveDefinition
      Destiny_Titles : Destiny_Title_Map;
      --  DestinyRecordDefinition
      Destiny_Vendors : Destiny_Vendor_Map;
      --  DestinyVendorDefinition

      --  TODO:
      --  DestinyFactionDefinition
      --  DestinyVendorGroupDefinition
   end record;

   --  Subprograms
   function Get_Manifest return Manifest_Type;

private
   Current_Manifest_Format_Version : constant := 4;
end API.Manifest;
