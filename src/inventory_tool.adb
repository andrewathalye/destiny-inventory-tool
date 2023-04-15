pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

-- GtkAda
with Gtk.Main;
with Gtkada.Builder; use Gtkada.Builder;
with GLib.Error; use GLib.Error;
use GLib;

-- Local Packages
with GUI.Base;
with GUI.Handlers;

procedure Inventory_Tool is
	-- Constants
	Discard_G : Guint;
	Discard_B : Boolean;
	Error : aliased GError;
begin
	-- Print Welcome Message
	Put_Line ("Destiny Inventory Tool v0.13");
	
	-- Load Interface
	Gtk.Main.Init;
	Gtk_New (GUI.Builder);
	Discard_G := Add_From_File (GUI.Builder, "res/gui.glade", Error'Access);

	GUI.Handlers.Set_Handlers;
	Do_Connect (GUI.Builder);
	
	-- Update GUI data
	-- This also creates all necessary windows
	GUI.Base.Reload_Data;

	-- Maintain an exclusive lock over the GUI
	-- while executing the main loop.
	--
	-- Download_Tasks would otherwise attempt to
	-- edit widgets while they are being drawn
	loop
		GUI.Locking_Main_Iteration;
	end loop;
end Inventory_Tool;
