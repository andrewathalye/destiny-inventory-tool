with API.Profiles; use API.Profiles;

package API.Manifest.Tools is
	type Bucket_Location_Type is (
		Unknown,
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

	for Bucket_Location_Type use (
		Unknown => 0,
		Chest => 14239492,
		Leg => 20886954,
		General => 138197802,
		Postmaster => 215593132,
		Ship => 284967655,
		Engram => 375726501,
		Clan_Bounty => 444348033,
		Clan_Activity => 497170007,
		Power => 953998645,
		Emote_Collection => 1107761855,
		Quest => 1345459588,
		Consumable => 1469714392,
		Kinetic => 1498876634,
		Artefact => 1506418338,
		Class => 1585787867,
		Quest_Step => 1801258597,
		Sparrow => 2025709351,
		Emote => 2401704334,
		Dummy_A => 2422292810,
		Energy => 2465295065,
		Subclass => 3284755031,
		Modification => 3313201758,
		Helmet => 3448274439,
		Gauntlets => 3551918588,
		Dummy_B => 3621873013,
		Finisher => 3683254069,
		Shell => 4023194814,
		Emblem => 4274335291,
		Clan_Banner => 4292445962);

	-- Intended to store sufficient information about an
	-- item to display it without further Manifest lookups
	--
	-- Location, Bucket_Location, Bucket_Hash, and Transfer_Status should
	-- be modified if the item is to be virtually moved
	type Item_Description is record
		Name : Unbounded_String;
		Description : Unbounded_String;
		Item_Hash : Manifest_Hash;
			-- DestinyInventoryItemDefinition
		Item_Instance_ID : Unbounded_String;

		Quantity : Integer_32;
		Max_Stack_Size : Integer_32;
		Location : Item_Location_Type;

		Bucket_Hash, Default_Bucket_Hash : Manifest_Hash;
			-- DestinyInventoryBucketDefinition
		Bucket_Location, Default_Bucket_Location : Bucket_Location_Type;

		Category : Destiny_Inventory_Bucket_Category;
		State : Item_State_Type;
		Allow_Actions : Boolean;
		Transfer_Status : Transfer_Status_Type;

		Icon_Path : Unbounded_String;
		Watermark_Path : Unbounded_String;

		Style_Overridden : Boolean;

		Postmaster_Pull_Has_Side_Effects : Boolean;
		Item_Type : Destiny_Item_Type;
		Tier_Type : Destiny_Tier_Type;
		Item_Type_And_Tier_Display_Name : Unbounded_String;
	end record;

	function Get_Description (
		M : Manifest_Type;
		C : Character_Type) return String;

	function Get_Description (
		M : Manifest_Type;
		I : Item_Type) return Item_Description;
	
	function Get_Title (
		M : Manifest_Type;
		C : Character_Type) return Unbounded_String;
end API.Manifest.Tools;
