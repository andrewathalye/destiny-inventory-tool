pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- GtkAda
with Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Builder; use Gtkada.Builder;
with GLib.Error; use GLib.Error;
with GLib.Object; use GLib.Object;
with Gtk.Label; use Gtk.Label;
use GLib;

-- Local Packages
with API.Authorise;
with API.Memberships;
with API.Manifest;
with API.Manifest.Tools;
with API.Profiles;
with API; use API;

with GUI;
with Shared; use Shared;

procedure Inventory_Tool is
	-- Constants
	Discard : Guint;
	Error : aliased GError;

	Window : Gtk_Window;
	Name : Gtk_Label;
begin
	-- Print Welcome Message
	Put_Line ("Destiny Inventory Tool v0.1");
--	Put_Line ("Your default platform appears to be "
--		& Memberships.Find_Default_Platform (Membership)'Image & ".");
	
	Gtk.Main.Init;
	Gtk_New (GUI.Builder);
	Discard := Add_From_File (GUI.Builder, "gui.glade", Error'Access);

	Register_Handler (GUI.Builder, "window_close_handler", GUI.Window_Close_Handler'Access);
	Register_Handler (GUI.Builder, "switch_button_clicked_handler", GUI.Switch_Button_Clicked_Handler'Access);
	Do_Connect (GUI.Builder);

	Window := Gtk_Window (GUI.Builder.Get_Object ("root"));
	Name := Gtk_Label (GUI.Builder.Get_Object ("name"));

	Gtk.Window.Show (Window);
	Set_Label (Name, +GUI.Membership.Bungie_Net_User.Unique_Name);
	
	for C of GUI.Profile.Characters loop
		GUI.Update_For_Character (C);
		exit;
	end loop;

	Gtk.Main.Main;
	return;

--	Put_Line ("Global Inventory:");
--	declare
--		D : Manifest.Tools.Item_Description;
--	begin
--		for I of Profile.Profile_Currencies loop
--			D := Manifest.Tools.Get_Description (M, I);
--			Put_Line (
--				D.Quantity'Image & "x "
--				& (+D.Name)
--				& " | " & (+D.Item_Type_And_Tier_Display_Name));

--		end loop;
--	end;

--	Put_Line ("Active Characters:");

--	for C of Profile.Characters loop
--		Put_Line (Manifest.Tools.Get_Description (M, C) & " -" & C.Light'Image);
--		declare
--			D : Manifest.Tools.Item_Description;
--		begin
--			for I of Profile.Character_Inventories (C.Character_ID) loop
--				D := Manifest.Tools.Get_Description (M, I);
--				Put_Line (
--					D.Quantity'Image & "x "
--					& (+D.Name)
--					& " | " & (+D.Item_Type_And_Tier_Display_Name));
--			end loop;
--			raise Program_Error;
--		end;
--	end loop;

	-- Print Current Loadout on Active Character
	
	-- Command Line Input
end Inventory_Tool;
