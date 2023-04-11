-- Gtk
with Gtk.Handlers; use Gtk.Handlers;

-- Local Packages
with API.Profiles;
with API.Manifest.Tools;
use API;

package GUI.Handlers is
	-- Instantiations
	package Widget_Callback is new Callback (Gtk_Widget_Record);

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

	procedure Item_Button_Handler (
		Widget : access Gtk_Widget_Record'Class;
		User_Data : Manifest.Tools.Item_Description);
end GUI.Handlers;
