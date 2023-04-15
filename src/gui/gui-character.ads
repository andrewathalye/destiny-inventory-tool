-- Local Packages
with API.Manifest.Tools;
with API.Inventories.Character;
use API;

package GUI.Character is
	-- Common State
	Current_Character : Profiles.Character_Type;
	Inventory : Inventories.Character.Character_Inventory_Type;

	-- Status Updates
	-- All Render* subprograms require an unlocked GUI, so they should be
	-- used with GUI.Locked_Wrapper if being used from a callback

	procedure Render;
		-- Just render the internal state
	procedure Render_Contents (Location : Manifest.Tools.Bucket_Location_Type);
		-- Render only the current contents window
		-- This will not show the contents grid if
		-- the popover isn't currently on screen
	procedure Locked_Render_Contents (Location : Manifest.Tools.Bucket_Location_Type);
		-- Idem, but usable from a callback

	procedure Update_For_Character (Character : Profiles.Character_Type);
		-- Update UI elements for the new Character and Render
end GUI.Character;
