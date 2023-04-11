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
with GUI.Global;
with GUI.Character;
with GUI.Base;
with GUI.Handlers;

procedure Inventory_Tool is
	-- Constants
	Discard_G : Guint;
	Discard_B : Boolean;
	Error : aliased GError;

	Window : Gtk_Window;
begin
	-- Print Welcome Message
	Put_Line ("Destiny Inventory Tool v0.9");
	
	-- Load Interface
	Gtk.Main.Init;
	Gtk_New (GUI.Builder);
	Discard_G := Add_From_File (GUI.Builder, "res/experimental.glade", Error'Access);

	GUI.Handlers.Set_Handlers;
	Do_Connect (GUI.Builder);
	
	-- Setup window
	Window := Gtk_Window (GUI.Builder.Get_Object ("root"));
	Gtk.Window.Show (Window);

	-- Update GUI data
	GUI.Base.Reload_Data;

	-- Accept GTK events and update internal data (downloads, etc.)
	loop
		GUI.Global.Tick;
		GUI.Character.Tick;
		Discard_B := Gtk.Main.Main_Iteration;
	end loop;
end Inventory_Tool;
