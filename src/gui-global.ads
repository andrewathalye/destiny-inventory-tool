-- Local Packages
with API.Manifest.Tools;

package GUI.Global is
	-- The Vault ("Global") stores three kinds of items
	-- General
	-- Consumables
	-- Modifications
	--
	-- General items are stored in the inventory by their default bucket locations
	-- rather than by their actual location (Manifest.Tools.General)
	
	-- Exceptions
	Item_Not_Found : exception;

	-- Subprograms
	-- Inventory Management
	procedure Add_Item (Item : Manifest.Tools.Item_Description);
	procedure Remove_Item (Item : Manifest.Tools.Item_Description);

	-- This accepts either General / Consumables / Modifications
	-- or any other valid Location, which will be intrinsically converted
	-- to General to return an item count
	function Item_Count (Location : Manifest.Tools.Bucket_Location_Type) return Natural;

	-- This accepts a Manifest Hash and searches the Vault for a matching item stack
	-- The Hash should be of an item that is stackable, but this is not checked.
	function Get_Item_Stack (Hash : Manifest.Manifest_Hash) return Manifest.Tools.Item_Description;
		-- Raises Item_Not_Found on failure

	-- Status Updates
	procedure Update_Inventory;
	procedure Render;

	-- Internal Callbacks
	procedure Tick;
end GUI.Global;
