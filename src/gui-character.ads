package GUI.Character is
	procedure Emblem_Button_Clicked_Handler (Builder : access Gtkada_Builder_Record'Class);

	procedure Update_For_Character (Character : Profiles.Character_Type);

	procedure Render;
end GUI.Character;
