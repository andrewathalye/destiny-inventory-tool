with API.Profiles; use API.Profiles;

package API.Manifest.Tools is
	type Item_Description is record
		Name : Unbounded_String;
		Description : Unbounded_String;
		Item_Instance_ID : Unbounded_String;
		Quantity : Integer_32;
		Bucket_Hash : Manifest_Hash;
			-- DestinyInventoryBucketDefinition
		Bucket_Order : Integer_32;
		State : Item_State_Type;
		Icon_Path : Unbounded_String;
		Watermark_Path : Unbounded_String;
		Item_Type_And_Tier_Display_Name : Unbounded_String;
		Tier_Type : Destiny_Tier_Type;
	end record;

	function Get_Description (
		M : Manifest_Type;
		C : Character_Type) return String;

	function Get_Description (
		M : Manifest_Type;
		I : Item_Type) return Item_Description;
end API.Manifest.Tools;
