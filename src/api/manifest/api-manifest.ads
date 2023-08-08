with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Interfaces;            use Interfaces;

package API.Manifest is
   --  Types
   -----------
   -- BASIC --
   -----------
   type Quantity_Type is range -1 .. Integer_32'Last;

   package USL is new Ada.Containers.Vectors (Natural, Unbounded_String);
   subtype Unbounded_String_List is USL.Vector;

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
     new Base_Manifest_Hash; -- TODO unimplemented

   -----------------------------
   -- DestinyGenderDefinition --
   -----------------------------
   type Destiny_Gender_Type is (Male, Female);

   type Destiny_Gender_Definition is record
      Gender_Type : Destiny_Gender_Type;
      Gender_Name : Unbounded_String;
   end record;
   package DGDM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Gender_Definition_Manifest_Hash,
      Element_Type => Destiny_Gender_Definition);
   subtype Destiny_Gender_Map is DGDM.Map;

   ---------------------------
   -- DestinyRaceDefinition --
   ---------------------------
   type Destiny_Race_Name is array (Destiny_Gender_Type) of Unbounded_String;
   package DRNM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Race_Definition_Manifest_Hash,
      Element_Type => Destiny_Race_Name);
   subtype Destiny_Race_Map is DRNM.Map;

   ----------------------------
   -- DestinyClassDefinition --
   ----------------------------
   type Destiny_Class_Name is array (Destiny_Gender_Type) of Unbounded_String;
   package DCNM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Class_Definition_Manifest_Hash,
      Element_Type => Destiny_Class_Name);
   subtype Destiny_Class_Map is DCNM.Map;

   -----------------------------
   -- DestinyRecordDefinition --
   -----------------------------
   type Destiny_Title_Name is array (Destiny_Gender_Type) of Unbounded_String;
   package DTNM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Record_Definition_Manifest_Hash,
      Element_Type => Destiny_Title_Name);
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
      Description                     : Unbounded_String;
      Name                            : Unbounded_String;
      Icon_Path                       : Unbounded_String; --  Nullable
      Watermark_Path                  : Unbounded_String; --  Nullable
      Shelved_Watermark_Path          : Unbounded_String; --  Nullable
      Secondary_Icon_Path             : Unbounded_String; --  Nullable
      Secondary_Overlay_Path          : Unbounded_String; --  Nullable
      Secondary_Special_Path          : Unbounded_String; --  Nullable
      Item_Type_And_Tier_Display_Name : Unbounded_String;
      --  stats, emblemObjectiveHash?
      Max_Stack_Size                  : Quantity_Type;
      Bucket_Type_Hash : Destiny_Inventory_Bucket_Definition_Manifest_Hash;
      Tier_Type                       : Destiny_Tier_Type;
      Display_Version_Watermark_Icons : Unbounded_String_List;
      --  plugs, sockets, perks?
      Allow_Actions                    : Boolean;
      Postmaster_Pull_Has_Side_Effects : Boolean;
      Item_Type                        : Destiny_Item_Type;
      --  breakerTypeHash?
      Default_Damage_Type_Hash : Destiny_Damage_Type_Definition_Manifest_Hash :=
        0; -- Nullable
   end record;

   package DIIDM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Inventory_Item_Definition_Manifest_Hash,
      Element_Type => Destiny_Inventory_Item_Definition);
   subtype Destiny_Inventory_Item_Map is DIIDM.Map;

   ---------------------------------
   -- DestinyDamageTypeDefinition --
   ---------------------------------
   type Destiny_Damage_Type_Definition is record
      Description : Unbounded_String;
      Name        : Unbounded_String;
      Icon_Path   : Unbounded_String; --  Nullable
      Show_Icon   : Boolean;
   end record;

   package DDTDM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Damage_Type_Definition_Manifest_Hash,
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
     (Key_Type     => Destiny_Inventory_Bucket_Definition_Manifest_Hash,
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
     (Key_Type     => Destiny_Objective_Definition_Manifest_Hash,
      Element_Type => Destiny_Objective_Definition);
   subtype Destiny_Objective_Map is DODM.Map;

   ---------------------------
   -- DestinyStatDefinition --
   ---------------------------
   --  At the moment, only the name of the stat is computed / stored
   package Destiny_Stat_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Stat_Definition_Manifest_Hash,
      Element_Type => Unbounded_String);
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
      Single_Item_Hash : Destiny_Inventory_Item_Definition_Manifest_Hash :=
        0; --  Nullable
      Randomized_Options_Count : Quantity_Type;
      Socket_Type_Hash         : Destiny_Socket_Type_Definition_Manifest_Hash;
   end record;

   package DVISOL is new Ada.Containers.Vectors
     (Natural, Destiny_Vendor_Item_Socket_Override_Type);
   subtype Destiny_Vendor_Item_Socket_Override_List is DVISOL.Vector;

   type Destiny_Vendor_Item_Definition is record
      Item_Hash       : Destiny_Inventory_Item_Definition_Manifest_Hash;
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

   type Destiny_Vendor_Location_Definition is record
      Destination_Hash      : Destiny_Destination_Definition_Manifest_Hash;
      Background_Image_Path : Unbounded_String;
   end record;

   package DVLL is new Ada.Containers.Vectors
     (Natural, Destiny_Vendor_Location_Definition);
   subtype Destiny_Vendor_Location_List is DVLL.Vector;

   package DIIDMHL is new Ada.Containers.Vectors
     (Natural, Destiny_Inventory_Item_Definition_Manifest_Hash);
   subtype Destiny_Inventory_Item_Definition_Manifest_Hash_List is
     DIIDMHL.Vector;

   type Destiny_Vendor_Definition is record
      Large_Icon_Path : Unbounded_String; --  Nullable
      Subtitle        : Unbounded_String;
      --  largeTransparentIcon?
      Description       : Unbounded_String;
      Name              : Unbounded_String;
      Icon_Path         : Unbounded_String; --  Nullable
      Display_Item_Hash : Destiny_Inventory_Item_Definition_Manifest_Hash; --  Nullable
      Inhibit_Buying    : Boolean;
      Inhibit_Selling   : Boolean;
      Faction_Hash      : Destiny_Faction_Definition_Manifest_Hash :=
        0; --  Nullable
      Failure_Strings : Failure_String_List;
      --  Vendor_Portrait? Vendor_Banner?
      Enabled : Boolean;
      Visible : Boolean;
      --  Categories?
      Display_Categories : Destiny_Display_Category_Map;
      --  Indexed by Hash
      Items : Destiny_Vendor_Item_Map;
      --  Indexed by Vendor Item Index (may be ignored if not needed)
      Locations : Destiny_Vendor_Location_List; --  Nullable
      Group : Destiny_Vendor_Group_Definition_Manifest_Hash := 0; --  Nullable
      --  Note: Theoretically there can be multiple, but according to the API spec
      --  only one group may be attached to a vendor at a time.
      Ignore_Sale_Hashes : Destiny_Inventory_Item_Definition_Manifest_Hash_List;
   end record;

   package DVDM is new Ada.Containers.Ordered_Maps
     (Destiny_Vendor_Definition_Manifest_Hash, Destiny_Vendor_Definition);
   subtype Destiny_Vendor_Map is DVDM.Map;

   ------------------------------
   -- DestinyFactionDefinition --
   ------------------------------
   type Destiny_Faction_Definition is record
      Description      : Unbounded_String;
      Name             : Unbounded_String;
      Icon_Path        : Unbounded_String; --  Nullable
      Progression_Hash : Destiny_Progression_Definition_Manifest_Hash;
   end record;

   package DFDM is new Ada.Containers.Ordered_Maps
     (Destiny_Faction_Definition_Manifest_Hash, Destiny_Faction_Definition);
   subtype Destiny_Faction_Map is DFDM.Map;

   ----------------------------------
   -- DestinyDestinationDefinition --
   ----------------------------------
   type Destiny_Bubble_Hash_Type is new Unsigned_32;

   type Destiny_Bubble_Definition is record
      Hash        : Destiny_Bubble_Hash_Type;
      Description : Unbounded_String;
      Name        : Unbounded_String;
   end record;

   package DBDL is new Ada.Containers.Vectors
     (Natural, Destiny_Bubble_Definition);
   subtype Destiny_Bubble_List is DBDL.Vector;

   type Destiny_Destination_Definition is record
      Description                    : Unbounded_String;
      Name                           : Unbounded_String;
      Place_Hash                     : Destiny_Place_Definition_Manifest_Hash;
      Default_Freeroam_Activity_Hash : Destiny_Activity_Definition_Manifest_Hash;
      --  activityGraphEntries?
      Bubbles : Destiny_Bubble_List;
   end record;

   package DDDM is new Ada.Containers.Ordered_Maps
     (Destiny_Destination_Definition_Manifest_Hash,
      Destiny_Destination_Definition);
   subtype Destiny_Destination_Map is DDDM.Map;

   ----------------------------
   -- DestinyPlaceDefinition --
   ----------------------------
   type Destiny_Place_Definition is record
      Description : Unbounded_String;
      Name        : Unbounded_String;
   end record;

   package DPDM is new Ada.Containers.Ordered_Maps
     (Destiny_Place_Definition_Manifest_Hash, Destiny_Place_Definition);
   subtype Destiny_Place_Map is DPDM.Map;

   -------------------------------
   -- DestinyActivityDefinition --
   -------------------------------
   type Destiny_Activity_Definition is record
      Description : Unbounded_String;
      Name        : Unbounded_String;
      --  Many Fields Omitted
      Destination_Hash : Destiny_Destination_Definition_Manifest_Hash;
      Place_Hash       : Destiny_Place_Definition_Manifest_Hash;
   end record;

   package DADM is new Ada.Containers.Ordered_Maps
     (Destiny_Activity_Definition_Manifest_Hash, Destiny_Activity_Definition);
   subtype Destiny_Activity_Map is DADM.Map;

   -------------
   -- CENTRAL --
   -------------
   --  Available Manifest fields (now sorted alphabetically)
   --  Most data types do not include all available information
   --  See individual definitions for more information

   type Manifest_Type is record
      Destiny_Activities        : Destiny_Activity_Map;
      Destiny_Classes           : Destiny_Class_Map;
      Destiny_Damage_Types      : Destiny_Damage_Type_Map;
      Destiny_Destinations      : Destiny_Destination_Map;
      Destiny_Factions          : Destiny_Faction_Map;
      Destiny_Genders           : Destiny_Gender_Map;
      Destiny_Inventory_Buckets : Destiny_Inventory_Bucket_Map;
      Destiny_Inventory_Items   : Destiny_Inventory_Item_Map;
      Destiny_Objectives        : Destiny_Objective_Map;
      Destiny_Places            : Destiny_Place_Map;
      Destiny_Races             : Destiny_Race_Map;
      Destiny_Stats             : Destiny_Stat_Map;
      Destiny_Titles            : Destiny_Title_Map;
      Destiny_Vendors           : Destiny_Vendor_Map;

      --  TODO:
      --  DestinyVendorGroupDefinition
      --  DestinySocketTypeDefinition
      --  DestinyProgressionDefinition
   end record;

   --  Subprograms
   function Get_Manifest return Manifest_Type;

private
   Current_Manifest_Format_Version : constant := 5;
end API.Manifest;
