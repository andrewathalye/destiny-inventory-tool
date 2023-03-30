pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

-- GtkAda
with Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Gtkada.Builder; use Gtkada.Builder;
with GLib.Error; use GLib.Error;
with GLib.Object; use GLib.Object;
use GLib;

-- Local Packages
with GUI;
with GUI.Character;
with GUI.Global;

procedure Inventory_Tool is
	-- Constants
	Discard : Guint;
	Error : aliased GError;

	Window : Gtk_Window;
begin
	-- Print Welcome Message
	Put_Line ("Destiny Inventory Tool v0.3");
	
	-- Load Interface
	Gtk.Main.Init;
	Gtk_New (GUI.Builder);
	Discard := Add_From_File (GUI.Builder, "res/gui.glade", Error'Access);

	-- Register Callbacks
	Register_Handler (GUI.Builder, "window_close_handler", GUI.Window_Close_Handler'Access);
	Register_Handler (GUI.Builder, "emblem_button_clicked_handler", GUI.Character.Emblem_Button_Clicked_Handler'Access);
	Register_Handler (GUI.Builder, "search_changed_handler", GUI.Global.Search_Changed_Handler'Access);

	Window := Gtk_Window (GUI.Builder.Get_Object ("root"));

	Gtk.Window.Show (Window);

	GUI.Global.Update_Inventory;

	-- Load Initial Character
	GUI.Character.Update_For_Character (GUI.Profile.Characters.Element (0));
	
	Do_Connect (GUI.Builder);

	declare
		Now : Time := Clock;
		TThen : Time;
	begin
		for I in 1 .. 50 loop
			GUI.Global.Render;
			GUI.Character.Render;
		end loop;
		TThen := Clock;
		Put_Line (To_Duration (TThen - Now)'Image);
	end;
	-- Benchmark
	Gtk.Main.Main;
end Inventory_Tool;
