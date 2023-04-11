-- Local Packages
with API.Manifest.Tools;

package GUI.Character is
	-- IMPORTANT NOTE
	-- Update_Characters and Update_For_Character
	-- must be called before any other subprogram
	-- in this package
	
	-- Common State
	Current_Character : Profiles.Character_Type;

	-- Inventory Management
	procedure Add_Item (
		Character : Profiles.Character_Type;
		Item : Manifest.Tools.Item_Description);

	procedure Remove_Item (
		Character : Profiles.Character_Type;
		Item : Manifest.Tools.Item_Description);

	procedure Equip_Item (
		Character : Profiles.Character_Type;
		Item : Manifest.Tools.Item_Description);
	
	function Item_Count (
		Character : Profiles.Character_Type;
		Location : Manifest.Tools.Bucket_Location_Type) return Natural;

	-- Status Updates
	procedure Render;
		-- Just render the internal state
	procedure Render_Contents (Location : Manifest.Tools.Bucket_Location_Type);
		-- Render only the current contents window
		-- This will not show the contents grid if
		-- the popover isn't currently on screen

	procedure Update_Characters (Profile : Profiles.Profile_Type);
		-- Load all Character data into internal format
	procedure Update_For_Character (Character : Profiles.Character_Type);
		-- Update UI elements for the new Character and Render
	
	-- Internal Callbacks
	procedure Tick;
end GUI.Character;
