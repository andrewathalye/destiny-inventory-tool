with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

-- Local Packages
with API.Memberships;

package API.Manifest is
	-- Types
	subtype Manifest_Hash is Unsigned_32;

	type Destiny_Gender_Type is (Male, Female);
	type Destiny_Gender_Definition is record
		Gender_Type : Destiny_Gender_Type;
		Gender_Name : Unbounded_String;
	end record;

	package DGDM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Gender_Definition);

	subtype Destiny_Gender_Map is DGDM.Map;

	type Destiny_Race_Name is array (Destiny_Gender_Type) of Unbounded_String;
	package DRNM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Race_Name);
	subtype Destiny_Race_Map is DRNM.Map;

	type Destiny_Class_Name is array (Destiny_Gender_Type) of Unbounded_String;
	package DCNM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Class_Name);
	subtype Destiny_Class_Map is DCNM.Map;

	type Destiny_Title_Name is array (Destiny_Gender_Type) of Unbounded_String;
	package DTNM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Title_Name);
	subtype Destiny_Title_Map is DTNM.Map;

	type Destiny_Inventory_Item_Definition is record
		Name : Unbounded_String;
		Description : Unbounded_String;
		Icon_Path : Unbounded_String;
		Watermark_Path : Unbounded_String;
		Shelved_Watermark_Path : Unbounded_String;
		Item_Type : Unbounded_String;
		-- Stats?
		Default_Damage_Type_Hash : Manifest_Hash;
			-- DestinyDamageTypeDefinition
	end record;
	package DIIDM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Inventory_Item_Definition);
	subtype Destiny_Inventory_Item_Map is DIIDM.Map;

	type Destiny_Damage_Type_Definition is record
		Name : Unbounded_String;
		Description : Unbounded_String;
		Icon_Path : Unbounded_String;
	end record;
	package DDTDM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Damage_Type_Definition);
	subtype Destiny_Damage_Type_Map is DDTDM.Map;

	type Destiny_Inventory_Bucket_Definition is record
		Name : Unbounded_String;
		Description : Unbounded_String;
		Icon_Path : Unbounded_String;
		Item_Count : Integer_32;
		FIFO : Boolean;
	end record;
	package DIBDM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Inventory_Bucket_Definition);
	subtype Destiny_Inventory_Bucket_Map is DIBDM.Map;

	type Manifest_Type is record
		Destiny_Genders : Destiny_Gender_Map;
			-- DestinyGenderDefinition
		Destiny_Races : Destiny_Race_Map;
			-- DestinyRaceDefinition
		Destiny_Classes : Destiny_Class_Map;
			-- DestinyClassDefinition
		Destiny_Titles : Destiny_Title_Map;
			-- DestinyRecordDefinition (partial)
		Destiny_Inventory_Items : Destiny_Inventory_Item_Map;
			-- DestinyInventoryItemDefinition
		Destiny_Damage_Types : Destiny_Damage_Type_Map;
			-- DestinyDamageTypeDefinition
		Destiny_Inventory_Buckets : Destiny_Inventory_Bucket_Map;
			-- DestinyInventoryBucketDefinition
	end record;

	-- Subprograms
--	function Get_Character_Type (
--		M : Manifest_Type;
--		C : Profiles.Character_Type) return String;

	function Get_Manifest (
		Headers : Auth_Header_Type;
		M : Memberships.Membership_Type) return Manifest_Type;
end API.Manifest;
