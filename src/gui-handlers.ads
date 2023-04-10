-- Local Packages
with API.Profiles;
use API;

package GUI.Handlers is
	-- Global Handlers
	procedure Set_Handlers;

	-- Dynamic Handlers
	procedure Character_Menu_Button_Clicked_Handler (
		Button : access Gtk_Widget_Record'Class;
		User_Data : Natural);

	procedure Transfer_Handler (
		Button : access Gtk_Widget_Record'Class;
		Target : Profiles.Character_Type);

	procedure Vault_Handler (Button : access Gtk_Widget_Record'Class);
end GUI.Handlers;
