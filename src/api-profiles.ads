with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Local Packages
with Shared; use Shared;
with API.Memberships; use API.Memberships;
with API.Manifest; use API.Manifest;

package API.Profiles is
	-- Characters
	package SM is new Ada.Containers.Ordered_Maps (Manifest_Hash, Integer_32);
	subtype Stats_Map is SM.Map;

	type Character_Type is record
		Character_ID : Unbounded_String;
		Date_Last_Played : Unbounded_String;
		Light : Integer_32;
		Stats : Stats_Map;
		Race_Hash : Manifest_Hash;
			-- DestinyRaceDefinition
		Gender_Hash : Manifest_Hash;
			-- DestinyGenderDefinition
		Class_Hash : Manifest_Hash;
			-- DestinyClassDefinition
		Emblem_Path : Unbounded_String;
		Emblem_Background_Path : Unbounded_String;
		Title_Record_Hash : Manifest_Hash := 0; -- Nullable
			-- Records.DestinyRecordDefinition
		-- Items Omitted
	end record;

	package CV is new Ada.Containers.Vectors (
		Index_Type => Natural,
		Element_Type => Character_Type);
	subtype Character_List is CV.Vector;

	-- Inventories
	type Bind_Status_Type is (Not_Bound, Bound_To_Character, Bound_To_Account, Bound_To_Guild);
	type Item_Location_type is (Unknown, Inventory, Vault, Vendor, Postmaster);
	type Transfer_Status_Type is (Can_Transfer, Item_Is_Equipped, Not_Transferable, No_Room_In_Destination);
	type Item_State_Type is record
		Locked : Boolean;
		Tracked : Boolean;
		Masterwork : Boolean;
		Crafted : Boolean;
		Highlighted_Objective : Boolean;
	end record;

	type Item_Type is record
		Item_Hash : Manifest_Hash;
			-- DestinyInventoryItemDefinition
		Item_Instance_ID : Unbounded_String;
		Quantity : Integer_32;
		Bind_Status : Bind_Status_Type;
		Location : Item_Location_Type;
		Bucket_Hash : Manifest_Hash;
			-- DestinyInventoryBucketDefinition
		Transfer_Status : Transfer_Status_Type;
		Lockable : Boolean;
		State : Item_State_Type;
		Override_Style_Item_Hash : Manifest_Hash := 0; -- Nullable
			-- DestinyInventoryItemDefinition
		Expiration_Date : Unbounded_String;
		Version_Number : Integer_32 := -1; -- Nullable
		-- Items Omitted
	end record;

	package IV is new Ada.Containers.Vectors (Natural, Item_Type);
	subtype Item_List is IV.Vector;

	pragma Warnings (Off, "is not referenced");
	function "=" (L,R : Item_List) return Boolean is (False);
	pragma Warnings (On, "is not referenced");

	package IM is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => Item_List,
		Hash => Hash,
		Equivalent_Keys => Equivalent_Key);
	subtype Inventory_Map is IM.Map;

	-- Loadouts
	package MHV is new Ada.Containers.Vectors (Natural, Manifest_Hash);
	subtype Manifest_Hash_List is MHV.Vector;
		
	type Loadout_Item_Type is record
		Instance_ID : Unbounded_String;
		Plug_Item_Hashes : Manifest_Hash_List;
			-- DestinyInventoryItemDefinition
	end record;

	package LIV is new Ada.Containers.Vectors (Natural, Loadout_Item_Type);
	subtype Loadout_Item_List is LIV.Vector;

	type Loadout_Type is record
		Colour_Hash : Manifest_Hash;
			-- Loadouts.DestinyLoadoutColorDefinition
		Icon_Hash : Manifest_Hash;
			-- Loadouts.DestinyLoadoutIconDefinition
		Name_Hash : Manifest_Hash;
			-- Loadouts.DestinyLoadoutNameDefinition
		Items : Loadout_Item_List;
	end record;

	package LV is new Ada.Containers.Vectors (Natural, Loadout_Type);
	subtype Loadout_List is LV.Vector;

	pragma Warnings (Off, "is not referenced");
	function "=" (L, R : Loadout_List) return Boolean is (False);
	pragma Warnings (On, "is not referenced");

	package LM is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => Loadout_List,
		Hash => Hash,
		Equivalent_Keys => Equivalent_Key);
	subtype Loadout_Map is LM.Map;

	-- Silver
	type Platform_Silver_Type is record
		PSN,
		XBOX,
		Blizzard,
		Stadia,
		Steam,
		Bungie_Next,
		EGS : Item_Type;
	end record;

	-- Profile
	type Profile_Type is record
		Profile_Inventory : Item_List;
		Profile_Currencies : Item_List;
		Platform_Silver : Platform_Silver_Type;
		-- Plug Sets?
		Characters : Character_List;
		Character_Inventories : Inventory_Map;
		Character_Loadouts : Loadout_Map;
		Character_Equipment : Inventory_Map;
		-- Character Plug Sets?
		-- Character Currency Lookups?
		-- Items Omitted
	end record;

	-- Subprograms
	function Get_Profile (
		Headers : Auth_Header_Type;
		M : Membership_Type) return Profile_Type;
end API.Profiles;
