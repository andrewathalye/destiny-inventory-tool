with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;

-- Local Packages
with API.Memberships; use API.Memberships;

package API.Profiles is
	-- Hashed Map Functions
	function Hash (Key : Unbounded_String) return Ada.Containers.Hash_Type is
		(Ada.Strings.Hash (To_String (Key)));
	
	function Equivalent_Key (L, R : Unbounded_String) return Boolean is
		(L = R);

	subtype Manifest_Hash is Unsigned_32; -- TODO!

	package SM is new Ada.Containers.Ordered_Maps (Manifest_Hash, Integer_32);
	subtype Stats_Map is SM.Map;

	type Character_Type is record
		Character_ID : Unbounded_String;
		Date_Last_Played : Unbounded_String;
		Light : Integer_32;
		Stats : Stats_Map;
		Race_Hash : Manifest_Hash;
		Gender_Hash : Manifest_Hash;
		Class_Hash : Manifest_Hash;
		Emblem_Path : Unbounded_String;
		Emblem_Background_Path : Unbounded_String;
		Title_Record_Hash : Manifest_Hash := 0; -- Nullable
		-- Items Omitted
	end record;

	package CM is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => Character_Type,
		Hash => Hash,
		Equivalent_Keys => Equivalent_Key);
	subtype Character_Map is CM.Map;

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
		Item_Instance_ID : Unbounded_String;
		Quantity : Integer_32;
		Bind_Status : Bind_Status_Type;
		Location : Item_Location_Type;
		Bucket_Hash : Manifest_Hash;
		Transfer_Status : Transfer_Status_Type;
		Lockable : Boolean;
		State : Item_State_Type;
		Override_Style_Item_Hash : Manifest_Hash := 0; -- Nullable
		Expiration_Date : Unbounded_String;
		-- Items Omitted
	end record;

	package IV is new Ada.Containers.Vectors (Natural, Item_Type);
	subtype Item_List is IV.Vector;

	type Inventory_Type is record
		Items : Item_List;
	end record;

	package IM is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => Inventory_Type,
		Hash => Hash,
		Equivalent_Keys => Equivalent_Key);
	subtype Inventory_Map is IM.Map;
		
	type Loadout_Type is record
		Colour_Hash : Manifest_Hash;
		Icon_Hash : Manifest_Hash;
		Name_Hash : Manifest_Hash;
		Instance_ID : Unsigned_64;
		Items : Item_List;
	end record;

	package LM is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => Loadout_Type,
		Hash => Hash,
		Equivalent_Keys => Equivalent_Key);
	subtype Loadout_Map is LM.Map;

	type Platform_Silver_Type is record
		PSN,
		XBOX,
		Blizzard,
		Stadia,
		Steam,
		Bungie_Next,
		EGS : Item_Type;
	end record;

	type Profile_Type is record
		Profile_Inventory : Inventory_Type;
		Profile_Currencies : Inventory_Type;
		Platform_Silver : Platform_Silver_Type;
		-- Plug Sets?
		Characters : Character_Map;
		Character_Inventories : Inventory_Map;
		Character_Loadouts : Loadout_Map;
		Character_Equipment : Inventory_Map;
		-- Character Plug Sets?
		-- Character Currency Lookups?
		-- Items Omitted
	end record;

	function Get_Profiles (
		Headers : Auth_Header_Type;
		M : Membership_Type) return Profile_Type;
end API.Profiles;
