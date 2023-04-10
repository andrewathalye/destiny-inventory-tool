package GUI.Character is
	-- Common State
	Current_Character : Profiles.Character_Type;

	-- Subprograms
	procedure Update_For_Character (Character : Profiles.Character_Type);

	procedure Render;
	procedure Tick;
end GUI.Character;
