--  Local Packages
with API.Manifest.Tools;
with API.Profiles;
with API.Inventories.Character; use API;

package GUI.Character is
   --  Common State
   Inventory : Inventories.Character.Character_Inventory_Array;

   --  Character State
   function Current_Character return Profiles.Character_Type with
     Inline;
   function Current_Character_Index return Profiles.Character_Range with
     Inline;

   --  Status Updates
   procedure Render;
   --  Just render the internal state

   procedure Render_Contents (Location : Manifest.Tools.Bucket_Location_Type);
   --  Render only the current contents window This will not show the contents
   --  grid if the popover isn't currently on screen

   procedure Update_For_Character
     (Character_Index : API.Profiles.Character_Range);
   --  Update UI elements for the new Character and Render
end GUI.Character;
