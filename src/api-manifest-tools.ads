with API.Profiles; use API.Profiles;

package API.Manifest.Tools is
	type Item_Description is record
		Name : Unbounded_String;
		Description : Unbounded_String;
		Item_Instance_ID : Unbounded_String;
		Quantity : Integer_32;
		Bucket_Hash, Default_Bucket_Hash : Manifest_Hash;
			-- DestinyInventoryBucketDefinition
		Category : Destiny_Inventory_Bucket_Category;
		State : Item_State_Type;
		Icon_Path : Unbounded_String;
		Watermark_Path : Unbounded_String;
		Style_Overridden : Boolean;
		Item_Type_And_Tier_Display_Name : Unbounded_String;
		Tier_Type : Destiny_Tier_Type;
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
