-- Local Packages
with Shared; use Shared;

package body API.Manifest.Tools is
	-- Private Subprograms
	function Get_Gender (
		M : Manifest_Type;
		C : Character_Type) return Destiny_Gender_Type
	is (M.Destiny_Genders (C.Gender_Hash).Gender_Type);

	-- Public Subprograms
	function Get_Description (
		M : Manifest_Type;
		C : Character_Type) return String
	is (
		(+M.Destiny_Races (C.Race_Hash) (Get_Gender (M, C)))
		& " " & (+M.Destiny_Classes (C.Class_Hash) (Get_Gender (M, C))));

	function Get_Description (
		M : Manifest_Type;
		I : Item_Type) return Item_Description
	is 
		Manifest_Item : constant Destiny_Inventory_Item_Definition :=
			M.Destiny_Inventory_Items (I.Item_Hash);
	begin
		return (
			Name => Manifest_Item.Name,
			Description => Manifest_Item.Description,
			Item_Instance_ID => I.Item_Instance_ID,
			Quantity => I.Quantity,
			Bucket_Hash => I.Bucket_Hash,
			Default_Bucket_Hash => Manifest_Item.Bucket_Type_Hash,
			Category => M.Destiny_Inventory_Buckets (I.Bucket_Hash).Category,
			Bucket_Order => M.Destiny_Inventory_Buckets (I.Bucket_Hash).Bucket_Order,
			State => I.State,
			Icon_Path => (case I.Override_Style_Item_Hash is
				when 0 => Manifest_Item.Icon_Path,
				when others => M.Destiny_Inventory_Items (I.Override_Style_Item_Hash).Icon_Path),
			Watermark_Path => (case I.Override_Style_Item_Hash is
				when 0 => Manifest_Item.Watermark_Path,
				when others => M.Destiny_Inventory_Items (I.Override_Style_Item_Hash).Watermark_Path),
			Item_Type_And_Tier_Display_Name => Manifest_Item.Item_Type_And_Tier_Display_Name,
			Tier_Type => Manifest_Item.Tier_Type);
	end Get_Description;

	function Get_Title (
		M : Manifest_Type;
		C : Character_Type) return Unbounded_String
	is begin
		if C.Title_Record_Hash /= 0 then
			return M.Destiny_Titles (C.Title_Record_Hash) (Get_Gender (M, C));
		end if;
		return Null_Unbounded_String;
	end Get_Title;
end API.Manifest.Tools;
