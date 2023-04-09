pragma Ada_2022;

-- Gtkada
with Gtk.Label; use Gtk.Label;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Popover; use Gtk.Popover;

with Gtk.Image; use Gtk.Image;
with Gtk.Button; use Gtk.Button;

with Gdk.Pixbuf; use Gdk.Pixbuf;

-- Local Packages
with Shared; use Shared;
with GUI.Character;
with API.Profiles; use API.Profiles;
with API.Manifest.Tools;
use API.Manifest;

package body GUI.Global is
	-- Instantiations
	package User_Callback_Natural is new User_Callback (Gtk_Widget_Record, Natural);
	use User_Callback_Natural;

	Vault_Inventory : array (Bucket_Location) of Item_Description_List;

	-- Cache
	Placeholder_Icon : constant Gdk_Pixbuf := Load_Image ("png",
							Get_Data ("res/placeholder_icon.png"));

	-- Redirections
	procedure Render_Items (
		List : Item_Description_List;
		Bucket : Gtk_Grid;
		Max_Left : Gint := 2;
		Max_Top : Gint := 2;
		T : Tasks.Download.Download_Task := Tasks.Download.Global_Task)
	is begin
		GUI.Render_Items (List, Bucket, T, Max_Left, Max_Top);
	end Render_Items;

	-- Private Subprograms
	-- Global UI Callbacks
	pragma Warnings (Off, "is not referenced");
	procedure Search_Changed_Handler (Builder : access Gtkada_Builder_Record'Class) is
		Search : constant Gtk_Search_Entry := Gtk_Search_Entry (GUI.Builder.Get_Object ("search"));
	begin
		GUI.Search_Query := +Search.Get_Chars (0);
		Render;
		GUI.Character.Render;
	end Search_Changed_Handler;
	pragma Warnings (On, "is not referenced");

	pragma Warnings (Off, "is not referenced");
	procedure Emblem_Button_Clicked_Handler (Builder : access Gtkada_Builder_Record'Class) is
		Character_Menu : constant Gtk_Popover := Gtk_Popover (GUI.Builder.Get_Object ("character_menu"));
	begin
		Character_Menu.Popup;
	end Emblem_Button_Clicked_Handler;
	pragma Warnings (On, "is not referenced");

	pragma Warnings (Off, "is not referenced");
	procedure Character_Menu_Button_Clicked_Handler (
		Button : access Gtk_Widget_Record'Class;
		User_Data : Natural)
	is
		Character_Menu : constant Gtk_Popover := Gtk_Popover (GUI.Builder.Get_Object ("character_menu"));
	begin
		Character_Menu.Popdown;
		Tasks.Download.Character_Task.Clear;
		Character.Update_For_Character (Profile.Characters (User_Data));
	end Character_Menu_Button_Clicked_Handler;
	pragma Warnings (On, "is not referenced");

	Entries : Natural := 0;

	procedure Contents_Closed_Handler (Builder : access Gtkada_Builder_Record'Class)
	is begin
		Entries := 0;
	end Contents_Closed_Handler;

	function Contents_Leave_Handler (Builder : access Gtkada_Builder_Record'Class) return Boolean
	is
		Contents : constant Gtk_Popover := Gtk_Popover (Builder.Get_Object ("full_contents"));
		Contents_Grid : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("full_contents_grid"));
	begin
		if Entries > 1 then
			Contents.Popdown;

			Entries := 0;
		end if;

		return False; -- Do not execute other handlers
	end Contents_Leave_Handler;

	function Contents_Enter_Handler (Builder : access Gtkada_Builder_Record'Class) return Boolean
	is begin
		Entries := @ + 1;
		return True; -- Do not execute other handlers
	end Contents_Enter_Handler;

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

				Connect (Button,
					"clicked",
					To_Marshaller (Character_Menu_Button_Clicked_Handler'Access),
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

		Vault_Other : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("vault_other"));
	begin
		Clear_Bucket (Vault_Kinetic);
		Clear_Bucket (Vault_Energy);
		Clear_Bucket (Vault_Power);
		Clear_Bucket (Vault_Shell);
		Clear_Bucket (Vault_Artefact);

		Clear_Bucket (Vault_Helmet);
		Clear_Bucket (Vault_Gauntlets);
		Clear_Bucket (Vault_Chest);
		Clear_Bucket (Vault_Leg);
		Clear_Bucket (Vault_Class);

		Clear_Bucket (Vault_Emblem);
		Clear_Bucket (Vault_Sparrow);
		Clear_Bucket (Vault_Ship);

		Clear_Bucket (Vault_Other);

		Render_Items (Vault_Inventory (Kinetic), Vault_Kinetic, 10, 60);
		Render_Items (Vault_Inventory (Energy), Vault_Energy, 10, 60);
		Render_Items (Vault_Inventory (Power), Vault_Power, 10, 60);
		Render_Items (Vault_Inventory (Shell), Vault_Shell, 10, 60);
		Render_Items (Vault_Inventory (Artefact), Vault_Artefact, 10, 60);

		Render_Items (Vault_Inventory (Helmet), Vault_Helmet, 10, 60);
		Render_Items (Vault_Inventory (Gauntlets), Vault_Gauntlets, 10, 60);
		Render_Items (Vault_Inventory (Chest), Vault_Chest, 10, 60);
		Render_Items (Vault_Inventory (Leg), Vault_Leg, 10, 60);
		Render_Items (Vault_Inventory (Class), Vault_Class, 10, 60);

		Render_Items (Vault_Inventory (Emblem), Vault_Emblem, 10, 60);
		Render_Items (Vault_Inventory (Sparrow), Vault_Sparrow, 10, 60);
		Render_Items (Vault_Inventory (Ship), Vault_Ship, 10, 60);

		Render_Items (Vault_Inventory (Unknown), Vault_Other, 10, 60);
	end Render;

	-- Public Subprograms

	-- Global Set_Callbacks
	procedure Set_Callbacks is
	begin
		Register_Handler (Builder, "emblem_button_clicked_handler", Emblem_Button_Clicked_Handler'Access);
		Register_Handler (Builder, "search_changed_handler", Search_Changed_Handler'Access);
		Register_Handler (GUI.Builder, "contents_leave_handler", Contents_Leave_Handler'Access);
		Register_Handler (GUI.Builder, "contents_enter_handler", Contents_Enter_Handler'Access);
		Register_Handler (GUI.Builder, "contents_closed_handler", Contents_Closed_Handler'Access);
	end Set_Callbacks;

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
		Descriptions.Attach (Make_Label (Subclass'Enum_Rep), 0, 0);

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
					Vault_Inventory (Bucket_Location'Enum_Val (D.Default_Bucket_Hash)).Append (D);
				exception
					when Constraint_Error =>
						Vault_Inventory (Unknown).Append (D);
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
			GUI.Image_Callback (Download_Data.Path, Download_Data.Widget, Download_Data.Data.all);
			Tasks.Download.Free (Download_Data.Data);
		else
			select
				Tasks.Download.Global_Task.Execute;
			else
				null;
			end select;
		end select;
	end Tick;
end GUI.Global;
