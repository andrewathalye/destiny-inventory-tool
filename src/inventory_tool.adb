pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

-- GtkAda
with Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Gtkada.Builder; use Gtkada.Builder;
with GLib.Error; use GLib.Error;
with GLib.Object; use GLib.Object;
use GLib;

-- Local Packages
with GUI;
with GUI.Global;
with GUI.Character;

procedure Inventory_Tool is
	-- Constants
	Discard_G : Guint;
	Discard_B : Boolean;
	Error : aliased GError;

	Window : Gtk_Window;
begin
	-- Print Welcome Message
	Put_Line ("Destiny Inventory Tool v0.5");
	
	-- Load Interface
	Gtk.Main.Init;
	Gtk_New (GUI.Builder);
	Discard_G := Add_From_File (GUI.Builder, "res/experimental.glade", Error'Access);

	-- Register Callbacks
	Register_Handler (GUI.Builder, "window_close_handler", GUI.Window_Close_Handler'Access);
	GUI.Global.Set_Callbacks;
	
	-- Setup window
	Window := Gtk_Window (GUI.Builder.Get_Object ("root"));
	Gtk.Window.Show (Window);

	-- Update and render inventory elements
	GUI.Global.Update_Inventory;
	GUI.Character.Update_For_Character (GUI.Profile.Characters (0));
	
	Do_Connect (GUI.Builder);

	-- Accept GTK events and update internal data (downloads, etc.)
	loop
		GUI.Global.Tick;
		GUI.Character.Tick;
		Discard_B := Gtk.Main.Main_Iteration_Do (Blocking => False);
	end loop;
end Inventory_Tool;
