limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes;       use API.Definitions.Hashes;

package API.Definitions.Destiny_Inventory_Item is
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
      Description                            : Unbounded_String;
      Name                                   : Unbounded_String;
      Icon_Path                              : Unbounded_String; --  Nullable
      Watermark_Path, Shelved_Watermark_Path : Unbounded_String; --  Nullable
      Secondary_Icon_Path                    : Unbounded_String; --  Nullable
      Secondary_Overlay_Path                 : Unbounded_String; --  Nullable
      Secondary_Special_Path                 : Unbounded_String; --  Nullable
      Item_Type_And_Tier_Display_Name        : Unbounded_String;
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

   package DIIDMHL is new Ada.Containers.Vectors
     (Natural, Destiny_Inventory_Item_Definition_Manifest_Hash);
   subtype Destiny_Inventory_Item_Definition_Manifest_Hash_List is
     DIIDMHL.Vector;

   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Inventory_Item;
