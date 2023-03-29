pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

-- GtkAda
with Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Gtkada.Builder; use Gtkada.Builder;
with GLib.Error; use GLib.Error;
with GLib.Object; use GLib.Object;
with Gtk.Label; use Gtk.Label;
use GLib;

-- Local Packages
with GUI;
with GUI.Character;
with Shared; use Shared;

procedure Inventory_Tool is
	-- Constants
	Discard : Guint;
	Error : aliased GError;

	Window : Gtk_Window;
	Name : Gtk_Label;
begin
	-- Print Welcome Message
	Put_Line ("Destiny Inventory Tool v0.2");
	
	-- Load Interface
	Gtk.Main.Init;
	Gtk_New (GUI.Builder);
	Discard := Add_From_File (GUI.Builder, "gui.glade", Error'Access);

	-- Register Callbacks
	Register_Handler (GUI.Builder, "window_close_handler", GUI.Window_Close_Handler'Access);
	Register_Handler (GUI.Builder, "switch_button_clicked_handler", GUI.Character.Switch_Button_Clicked_Handler'Access);
	Do_Connect (GUI.Builder);

	Window := Gtk_Window (GUI.Builder.Get_Object ("root"));
	Name := Gtk_Label (GUI.Builder.Get_Object ("name"));

	Gtk.Window.Show (Window);
	Set_Label (Name, +GUI.Membership.Bungie_Net_User.Unique_Name);

	-- Load Initial Character
	GUI.Character.Update_For_Character (GUI.Profile.Characters.Element (0));
	
	Gtk.Main.Main;
end Inventory_Tool;
