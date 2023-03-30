with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

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

	package USL is new Ada.Containers.Vectors (Natural, Unbounded_String);
	subtype Unbounded_String_List is USL.Vector;
	type Destiny_Tier_Type is (Unknown, Currency, Basic, Common, Rare, Superior, Exotic);
	type Destiny_Inventory_Item_Definition is record
		Description : Unbounded_String;
		Name : Unbounded_String;
		Icon_Path : Unbounded_String;
		Watermark_Path : Unbounded_String;
		Shelved_Watermark_Path : Unbounded_String;
		Item_Type_And_Tier_Display_Name : Unbounded_String;
		Bucket_Type_Hash : Manifest_Hash;
		Tier_Type : Destiny_Tier_Type;
		Display_Version_Watermark_Icons : Unbounded_String_List;
		-- Stats?
		Default_Damage_Type_Hash : Manifest_Hash := 0; -- Nullable
			-- DestinyDamageTypeDefinition
	end record;
	package DIIDM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Inventory_Item_Definition);
	subtype Destiny_Inventory_Item_Map is DIIDM.Map;

	type Destiny_Damage_Type_Definition is record
		Description : Unbounded_String;
		Name : Unbounded_String;
		Icon_Path : Unbounded_String;
		Show_Icon : Boolean;
	end record;
	package DDTDM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Damage_Type_Definition);
	subtype Destiny_Damage_Type_Map is DDTDM.Map;

	type Destiny_Inventory_Bucket_Category is (Invisible, Item, Currency, Equippable, Ignored);
	type Destiny_Inventory_Bucket_Definition is record
		Description : Unbounded_String;
		Name : Unbounded_String;
		Category: Destiny_Inventory_Bucket_Category;
		Bucket_Order : Integer_32;
		Item_Count : Integer_32;
		FIFO : Boolean;
	end record;
	package DIBDM is new Ada.Containers.Ordered_Maps (
		Key_Type => Manifest_Hash,
		Element_Type => Destiny_Inventory_Bucket_Definition);
	subtype Destiny_Inventory_Bucket_Map is DIBDM.Map;

	-- Fields ordered by Manifest order
	type Manifest_Type is record
		Destiny_Classes : Destiny_Class_Map;
			-- DestinyClassDefinition
		Destiny_Genders : Destiny_Gender_Map;
			-- DestinyGenderDefinition
		Destiny_Inventory_Buckets : Destiny_Inventory_Bucket_Map;
			-- DestinyInventoryBucketDefinition
		Destiny_Races : Destiny_Race_Map;
			-- DestinyRaceDefinition
		Destiny_Damage_Types : Destiny_Damage_Type_Map;
			-- DestinyDamageTypeDefinition
		Destiny_Inventory_Items : Destiny_Inventory_Item_Map;
			-- DestinyInventoryItemDefinition
		Destiny_Titles : Destiny_Title_Map;
			-- DestinyRecordDefinition (partial)
	end record;

	-- Subprograms
	function Get_Manifest (M : Memberships.Membership_Type) return Manifest_Type;
end API.Manifest;
