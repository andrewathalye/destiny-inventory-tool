pragma Ada_2022;

-- Gtkada
with Gtk.Label; use Gtk.Label;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Popover; use Gtk.Popover;

with Gtk.Image; use Gtk.Image;
with Gtk.Button; use Gtk.Button;

with Gdk.Pixbuf; use Gdk.Pixbuf;

-- Local Packages
with Shared; use Shared;

with GUI.Handlers;

with API.Profiles; use API.Profiles;
with API.Manifest.Tools; use API.Manifest.Tools; -- For enums
use API.Manifest; -- For '='

package body GUI.Global is
	-- Instantiations
	package User_Callback_Natural is new User_Callback (Gtk_Widget_Record, Natural);
	package User_Callback_Character is new User_Callback (Gtk_Widget_Record, Profiles.Character_Type);
	package Widget_Callback is new Callback (Gtk_Widget_Record);

--	Vault_Inventory : array (Bucket_Location_Type) of Item_Description_List;

	-- Cache
	Placeholder_Icon : constant Gdk_Pixbuf := Load_Image ("png",
							Get_Data ("res/placeholder_icon.png"));

	-- Redirections
	procedure Render_Items (
		List : Item_Description_List;
		Bucket : Gtk_Grid;
		Max_Left : Gint := 2;
		T : Tasks.Download.Download_Task := Tasks.Download.Global_Task)
	is begin
		GUI.Render_Items (List, Bucket, T, Max_Left);
	end Render_Items;

	-- Private Subprograms
	-- Global UI Setup
	procedure Setup_Character_Menu is
		Character_Grid : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("character_grid"));
		Emblem_Button : constant Gtk_Button := Gtk_Button (GUI.Builder.Get_Object ("emblem_button"));
		Character_Menu : constant Gtk_Popover := Gtk_Popover (GUI.Builder.Get_Object ("character_menu"));

		Count : Gint := 0;
	begin
		Character_Menu.Set_Relative_To (Emblem_Button);

		for C of Profile.Characters loop
			declare
				Image : Gtk_Image;
				Button : Gtk_Button;
			begin
				Gtk_New (Image);
				Gtk_New (Button, Manifest.Tools.Get_Description (The_Manifest, C));

				-- Load emblem
				if Global_Pixbuf_Cache.Contains (C.Emblem_Path) then
					Image.Set (Global_Pixbuf_Cache.Element (C.Emblem_Path));
				else
--					Put_Debug ("Get mini emblem");
					Image.Set (Placeholder_Icon);
					Tasks.Download.Global_Task.Download (
						C.Emblem_Path,
						Gtk_Widget (Image));
				end if;

				User_Callback_Natural.Connect (Button,
					"clicked",
					User_Callback_Natural.To_Marshaller (
						Handlers.Character_Menu_Button_Clicked_Handler'Access),
					User_Data => Natural (Count));

				Image.Show;
				Button.Show;
					
				Character_Grid.Attach (Image, 0, Count);
				Character_Grid.Attach (Button, 1, Count);

				Count := @ + 1;
			end;
		end loop;
	end Setup_Character_Menu;

	procedure Setup_Transfer_Menu is
		Transfer_Grid : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("transfer_grid"));

		Count : Gint := 0;

		Vault_Button : Gtk_Button;
	begin
		for C of Profile.Characters loop
			declare
				Image : Gtk_Image;
				Button : Gtk_Button;
			begin
				Gtk_New (Image);
				Gtk_New (Button, Manifest.Tools.Get_Description (The_Manifest, C));

				-- Load emblem
				if Global_Pixbuf_Cache.Contains (C.Emblem_Path) then
					Image.Set (Global_Pixbuf_Cache.Element (C.Emblem_Path));
				else
--					Put_Debug ("Get mini emblem");
					Image.Set (Placeholder_Icon);
						
					Tasks.Download.Global_Task.Download (
						C.Emblem_Path,
						Gtk_Widget (Image));
				end if;

				Image.Show;
				User_Callback_Character.Connect (
					Button,
					"clicked",
					User_Callback_Character.To_Marshaller (
						Handlers.Transfer_Handler'Access),
					C);
				Button.Show;
					
				Transfer_Grid.Attach (Image, 0, Count);
				Transfer_Grid.Attach (Button, 1, Count);

				Count := @ + 1;
			end;
		end loop;

		Gtk_New (Vault_Button, "Vault");
		Widget_Callback.Connect (
			Vault_Button,
			"clicked",
			Widget_Callback.To_Marshaller (
				Handlers.Vault_Handler'Access));
		Vault_Button.Show;

		Transfer_Grid.Attach (Vault_Button, 1, Count);
	end Setup_Transfer_Menu;

	-- Global UI Render
	procedure Render is
		Vault_Kinetic : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_kinetic"));
		Vault_Energy : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_energy"));
		Vault_Power : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_power"));
		Vault_Shell : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_shell"));
		Vault_Artefact : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_artefact"));

		Vault_Helmet : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_helmet"));
		Vault_Gauntlets : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_gauntlets"));
		Vault_Chest : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_chest"));
		Vault_Leg : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_leg"));
		Vault_Class : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_class"));

		Vault_Emblem : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_emblem"));
		Vault_Sparrow : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_sparrow"));
		Vault_Ship : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_ship"));

		Vault_Consumable : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_consumable"));
		Vault_Modification : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_modification"));

		Vault_Other : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_other"));
	begin
		Clear_Bucket (Vault_Kinetic);
		Clear_Bucket (Vault_Energy);
		Clear_Bucket (Vault_Power);
		Clear_Bucket (Vault_Shell);
		Clear_Bucket (Vault_Artefact);

		Render_Items (Vault_Inventory (Kinetic), Vault_Kinetic, 10);
		Render_Items (Vault_Inventory (Energy), Vault_Energy, 10);
		Render_Items (Vault_Inventory (Power), Vault_Power, 10);
		Render_Items (Vault_Inventory (Shell), Vault_Shell, 10);
		Render_Items (Vault_Inventory (Artefact), Vault_Artefact, 10);

		Clear_Bucket (Vault_Helmet);
		Clear_Bucket (Vault_Gauntlets);
		Clear_Bucket (Vault_Chest);
		Clear_Bucket (Vault_Leg);
		Clear_Bucket (Vault_Class);

		Render_Items (Vault_Inventory (Helmet), Vault_Helmet, 10);
		Render_Items (Vault_Inventory (Gauntlets), Vault_Gauntlets, 10);
		Render_Items (Vault_Inventory (Chest), Vault_Chest, 10);
		Render_Items (Vault_Inventory (Leg), Vault_Leg, 10);
		Render_Items (Vault_Inventory (Class), Vault_Class, 10);

		Clear_Bucket (Vault_Emblem);
		Clear_Bucket (Vault_Sparrow);
		Clear_Bucket (Vault_Ship);

		Render_Items (Vault_Inventory (Emblem), Vault_Emblem, 10);
		Render_Items (Vault_Inventory (Sparrow), Vault_Sparrow, 10);
		Render_Items (Vault_Inventory (Ship), Vault_Ship, 10);

		Clear_Bucket (Vault_Consumable);
		Clear_Bucket (Vault_Modification);

		Render_Items (Vault_Inventory (Consumable), Vault_Consumable, 10);
		Render_Items (Vault_Inventory (Modification), Vault_Modification, 10);

		-- Theoretically, no items should appear here.
		Clear_Bucket (Vault_Other);
		Render_Items (Vault_Inventory (Unknown), Vault_Other, 10);
	end Render;

	-- Public Subprograms

	-- Global Set_Callbacks
	
	procedure Setup_Descriptions is
		function Make_Label (Hash : Manifest.Manifest_Hash) return Gtk_Label is
			Result : Gtk_Label;
		begin
			Gtk_New (Result, +The_Manifest.Destiny_Inventory_Buckets (Hash).Name);
			Result.Show;
			return Result;
		end Make_Label;

		Descriptions : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("descriptions"));
	begin
		Descriptions.Attach (Make_Label (Manifest.Tools.Subclass'Enum_Rep), 0, 0);

		Descriptions.Attach (Make_Label (Kinetic'Enum_Rep), 0, 1);
		Descriptions.Attach (Make_Label (Energy'Enum_Rep), 0, 2);
		Descriptions.Attach (Make_Label (Power'Enum_Rep), 0, 3);
		Descriptions.Attach (Make_Label (Shell'Enum_Rep), 0, 4);
		Descriptions.Attach (Make_Label (Artefact'Enum_Rep), 0, 5);


		Descriptions.Attach (Make_Label (Helmet'Enum_Rep), 1, 1);
		Descriptions.Attach (Make_Label (Gauntlets'Enum_Rep), 1, 2);
		Descriptions.Attach (Make_Label (Chest'Enum_Rep), 1, 3);
		Descriptions.Attach (Make_Label (Leg'Enum_Rep), 1, 4);
		Descriptions.Attach (Make_Label (Class'Enum_Rep), 1, 5);
	end Setup_Descriptions;

	-- Global Update_Inventory
	procedure Update_Inventory is
		Name : constant Gtk_Label := Gtk_Label (GUI.Builder.Get_Object ("name"));
	begin
		-- Update username
		Set_Label (Name, +GUI.Membership.Bungie_Net_User.Unique_Name);

		-- One-time setup per profile
		Setup_Transfer_Menu;
		Setup_Character_Menu;
		Setup_Descriptions;

		-- Load vault inventory
		for I of Profile.Profile_Inventory loop
			if I.Location = Profiles.Vault then
				declare
					D : constant Manifest.Tools.Item_Description := Manifest.Tools.Get_Description (
						The_Manifest,
						I);
				begin
					Vault_Inventory (D.Default_Bucket_Location).Append (D);
				end;
			end if;
		end loop;

		-- Draw global inventories
		Render;
	end Update_Inventory;

	procedure Tick is
		Download_Data : Tasks.Download.Download_Data_Type;
	begin
		select
			Tasks.Download.Global_Task.Complete (Download_Data);
			GUI.Image_Callback (Download_Data.Path, Download_Data.Widget, Download_Data.Data);
		else
			select
				Tasks.Download.Global_Task.Execute;
			else
				null;
			end select;
		end select;
	end Tick;
end GUI.Global;
