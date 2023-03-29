pragma Ada_2022;

-- Gtkada
with Gtk.Label; use Gtk.Label;
with Gtk.Search_Entry; use Gtk.Search_Entry;

with Gtk.Image; use Gtk.Image;
with Gtk.Button; use Gtk.Button;

-- AWS
with AWS.Client;
with AWS.Response;
use AWS;

-- Local Packages
with Shared; use Shared;
with GUI.Character;
with API.Profiles; use API.Profiles;
with API.Manifest.Tools;

package body GUI.Global is
	Profile_Inventory : Item_Description_List;

	procedure Render is
		Vault_Bucket : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault"));
	begin
		Clear_Bucket (Vault_Bucket);
		Render_Items (Profile_Inventory, Vault_Bucket, 10, 60);
	end Render;

	pragma Warnings (Off, "is not referenced");
	procedure Search_Changed_Handler (Builder : access Gtkada_Builder_Record'Class) is
		Search : constant Gtk_Search_Entry := Gtk_Search_Entry (GUI.Builder.Get_Object ("search"));
	begin
		GUI.Search_Query := +Search.Get_Chars (0);
		Render;
		GUI.Character.Render;
	end Search_Changed_Handler;
	pragma Warnings (On, "is not referenced");

	procedure Setup_Transfer_Menu is
		Transfer_Grid : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("transfer_grid"));

		Count : Gint := 0;

		Vault_Button : Gtk_Button;
	begin
		for C of Profile.Characters loop
			declare
				Image : Gtk_Image;
				Button : Gtk_Button;

				Data : Response.Data;
			begin
				Gtk_New (Image);
				Gtk_New (Button, Manifest.Tools.Get_Description (The_Manifest, C));

				-- Load emblem
				if Has_Cached (+C.Emblem_Path) then
					Image.Set (
						Load_Image (
							+C.Emblem_Path,
							Get_Cached (+C.Emblem_Path)));
				else
					Put_Debug ("Get mini emblem");
					Data := Client.Get (Bungie_Root & (+C.Emblem_Path));
					Cache (+C.Emblem_Path, Response.Message_Body (Data));
					Image.Set (
						Load_Image (
							+C.Emblem_Path,
							Response.Message_Body (Data)));
				end if;

				Image.Show;
				Button.Show;
					
				Transfer_Grid.Attach (Image, 0, Count);
				Transfer_Grid.Attach (Button, 1, Count);

				Count := @ + 1;
			end;
		end loop;

		Gtk_New (Vault_Button, "Vault");
		Vault_Button.Show;

		Transfer_Grid.Attach (Vault_Button, 1, Count);
	end Setup_Transfer_Menu;

	procedure Update_Inventory is
		Name : constant Gtk_Label := Gtk_Label (GUI.Builder.Get_Object ("name"));
	begin
		-- Update username
		Set_Label (Name, +GUI.Membership.Bungie_Net_User.Unique_Name);

		Setup_Transfer_Menu;

		-- Load profile inventory
		for I of Profile.Profile_Inventory loop
			if I.Location = Profiles.Vault then
				Profile_Inventory.Append (Manifest.Tools.Get_Description (The_Manifest, I));
			end if;
		end loop;

		-- Draw global inventories
		Render;
	end Update_Inventory;
end GUI.Global;
