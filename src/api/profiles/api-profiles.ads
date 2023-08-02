with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Calendar;          use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;            use Interfaces;

--  Local Packages
with Shared.Strings;
with API.Memberships; use API.Memberships;
with API.Manifest;    use API.Manifest;

package API.Profiles is
   --  Note: All types in this package correspond roughly to
   --  datatypes returned from a call to GetProfile

   --  Basic Types
   subtype Item_Instance_ID_Type is Integer_64;

   --  Characters
   package Stats_Maps is new Ada.Containers.Ordered_Maps
     (Manifest_Hash, Quantity_Type);
   subtype Stats_Map is Stats_Maps.Map;

   type Character_Type is record
      Character_ID     : Unbounded_String;
      Date_Last_Played : Unbounded_String;
      Light            : Quantity_Type;
      Stats            : Stats_Map;
      Race_Hash        : Manifest_Hash;
      --  DestinyRaceDefinition
      Gender_Hash : Manifest_Hash;
      --  DestinyGenderDefinition
      Class_Hash : Manifest_Hash;
      --  DestinyClassDefinition
      Emblem_Path            : Unbounded_String;
      Emblem_Background_Path : Unbounded_String;
      Title_Record_Hash      : Manifest_Hash := 0; -- Nullable
      --  Records.DestinyRecordDefinition Items Omitted
   end record;
   package Character_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Character_Type);
   subtype Character_List is Character_Vectors.Vector;
   --  Inventories

   type Bind_Status_Type is
     (Not_Bound, Bound_To_Character, Bound_To_Account, Bound_To_Guild);

   type Transfer_Status_Type is
     (Can_Transfer,
      Item_Is_Equipped,
      Not_Transferable,
      No_Room_In_Destination);

   type Item_State_Type is record
      Locked                : Boolean;
      Tracked               : Boolean;
      Masterwork            : Boolean;
      Crafted               : Boolean;
      Highlighted_Objective : Boolean;
   end record;

   type Item_Type is record
      Item_Hash : Manifest_Hash;
      --  DestinyInventoryItemDefinition
      Item_Instance_ID : Item_Instance_ID_Type := -1; -- Nullable
      --  Note: For compatibility reasons, the API returns
      --  item instance IDs as strings rather than Integer_64 values.
      Quantity    : Quantity_Type;
      Bind_Status : Bind_Status_Type;
      Location    : Item_Location_Type;
      Bucket_Hash : Manifest_Hash;
      --  DestinyInventoryBucketDefinition
      Transfer_Status          : Transfer_Status_Type;
      Lockable                 : Boolean;
      State                    : Item_State_Type;
      Override_Style_Item_Hash : Manifest_Hash := 0; -- Nullable
      --  DestinyInventoryItemDefinition
      Expiration_Date : Unbounded_String; -- Nullable
      Version_Number  : Integer_32 := -1; -- Nullable
      --  Items Omitted
   end record;

   package Item_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Item_Type);
   use Item_Vectors;
   subtype Item_List is Item_Vectors.Vector;

   package Inventory_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Item_List,
      Hash            => Shared.Strings.Hash,
      Equivalent_Keys => Shared.Strings.Equivalent_Keys);
   subtype Inventory_Map is Inventory_Maps.Map;

   --  Loadouts
   package Manifest_Hash_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Manifest_Hash);
   subtype Manifest_Hash_List is Manifest_Hash_Vectors.Vector;

   type Loadout_Item_Type is record
      Item_Instance_ID : Unbounded_String;
      Plug_Item_Hashes : Manifest_Hash_List;
      --  DestinyInventoryItemDefinition
   end record;
   package Loadout_Item_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Loadout_Item_Type);
   subtype Loadout_Item_List is Loadout_Item_Vectors.Vector;

   type Loadout_Type is record
      Colour_Hash : Manifest_Hash;
      --  Loadouts.DestinyLoadoutColorDefinition
      Icon_Hash : Manifest_Hash;
      --  Loadouts.DestinyLoadoutIconDefinition
      Name_Hash : Manifest_Hash;
      --  Loadouts.DestinyLoadoutNameDefinition
      Items : Loadout_Item_List;
   end record;
   package Loadout_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Loadout_Type);
   subtype Loadout_List is Loadout_Vectors.Vector;
   use all type Loadout_Vectors.Vector;

   package Loadout_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Loadout_List,
      Hash            => Shared.Strings.Hash,
      Equivalent_Keys => Shared.Strings.Equivalent_Keys);
   subtype Loadout_Map is Loadout_Maps.Map;

   --  Silver (skips BMT.None because that can't have any Silver)
   type Platform_Silver_Array is
     array
       (Bungie_Platform_Type range
          Bungie_Platform_Type'Succ (Bungie_Platform_Type'First) ..
            Bungie_Platform_Type'Last) of Item_Type;

   --  Instanced Data
   type Perk_Type is record
      Perk_Hash : Manifest_Hash;
      --  DestinySandboxPerkDefinition
      Icon_Path : Unbounded_String;
      Is_Active : Boolean;
      Visible   : Boolean;
   end record;

   package Perk_Lists is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Perk_Type);
   use type Perk_Lists.Vector;
   subtype Perk_List is Perk_Lists.Vector;

   package Perks_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Item_Instance_ID_Type, Element_Type => Perk_List);
   subtype Perks_Map is Perks_Maps.Map;

   type Socket_Type is record
      Plug_Hash : Manifest_Hash := 0;
      --  DestinyInventoryItemDefinition
      Is_Enabled : Boolean;
      Is_Visible : Boolean;
   end record;

   package Socket_Lists is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Socket_Type);
   use type Socket_Lists.Vector;
   subtype Socket_List is Socket_Lists.Vector;

   package Sockets_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Item_Instance_ID_Type, Element_Type => Socket_List);
   subtype Sockets_Map is Sockets_Maps.Map;

   type Instance_Type is record
      Light_Level     : Quantity_Type;
      Energy_Capacity : Quantity_Type;
      Energy_Used     : Quantity_Type;
   end record;

   package Instance_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Item_Instance_ID_Type, Element_Type => Instance_Type);
   subtype Instance_Map is Instance_Maps.Map;

   use type Stats_Map;
   package Stats_Maps_By_IID is new Ada.Containers.Ordered_Maps
     (Key_Type => Item_Instance_ID_Type, Element_Type => Stats_Map);
   subtype Stats_Map_By_IID is Stats_Maps_By_IID.Map;

   type Plug_Objective_Type is record
      Objective_Hash : Manifest_Hash;
      --  DestinyObjectiveDefinition
      Destination_Hash : Manifest_Hash := 0; --  Nullable
      Activity_Hash    : Manifest_Hash := 0; --  Nullable
      Progress         : Quantity_Type := -1; --  Nullable
      Completion_Value : Quantity_Type;
      Complete         : Boolean;
      Visible          : Boolean;
   end record;

   package Plug_Objective_Lists is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Plug_Objective_Type);
   use type Plug_Objective_Lists.Vector;
   subtype Plug_Objective_List is Plug_Objective_Lists.Vector;

   package Plug_Objective_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Manifest_Hash, Element_Type => Plug_Objective_List);
   use type Plug_Objective_Maps.Map;
   subtype Plug_Objective_Map is Plug_Objective_Maps.Map;

   package Plug_Objectives_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Item_Instance_ID_Type, Element_Type => Plug_Objective_Map);
   subtype Plug_Objectives_Map is Plug_Objectives_Maps.Map;

   type Item_Components_Type is record
      Instances : Instance_Map;
      Stats     : Stats_Map_By_IID;
      --  Note: Map (Instance_ID) of Map (Stat Manifest_Hash) of Stat
      Sockets : Sockets_Map;
      --  Note: Map (Instance_ID) of List of Sockets
      Plug_Objectives : Plug_Objectives_Map;
      --  Note: Map (Instance_ID) of Map (Socket Manifest_Hash) of List of Objectives
      Perks : Perks_Map; --  TODO: May not be necessary? Consider removing.
   end record;

   package String_Variable_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Unsigned_32, Element_Type => Quantity_Type);

   subtype String_Variable_Map is String_Variable_Maps.Map;

   --  Profile
   type Profile_Type is record
      Response_Minted_Timestamp : Time;
      Profile_Inventory         : Item_List;
      Profile_Currencies        : Item_List;
      Platform_Silver           : Platform_Silver_Array;
      --  Plug Sets?
      Profile_String_Variables : String_Variable_Map;
      Characters               : Character_List;
      Character_Inventories    : Inventory_Map;
      Character_Loadouts       : Loadout_Map;
      Character_Equipment      : Inventory_Map;
      --  Character Plug Sets?
      --  Character Currency Lookups? Items Omitted
      Item_Components : Item_Components_Type;
   end record;

   --  Subprograms
   function Get_Profile (M : Membership_Type) return Profile_Type;
end API.Profiles;
