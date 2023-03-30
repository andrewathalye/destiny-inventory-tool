pragma Ada_2022;

-- Gtkada
with Gtk.Label; use Gtk.Label;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Popover; use Gtk.Popover;

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
use API.Manifest;

package body GUI.Global is
	-- Instantiations
	package User_Callback_Natural is new User_Callback (Gtk_Widget_Record, Natural);
	use User_Callback_Natural;

	Vault_Inventory : array (Bucket_Location) of Item_Description_List;

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
		Character.Update_For_Character (Profile.Characters (User_Data));
	end Character_Menu_Button_Clicked_Handler;
	pragma Warnings (On, "is not referenced");

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

				Data : Response.Data;
			begin
				Gtk_New (Image);
				Gtk_New (Button, Manifest.Tools.Get_Description (The_Manifest, C));

				-- Load emblem
				if Has_Cached (+C.Emblem_Path) then
					Image.Set (
						Caching_Load_Image (C.Emblem_Path,
							Get_Cache_Path (+C.Emblem_Path)));
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
	end Set_Callbacks;

	procedure Setup_Descriptions is
		Kinetic_Hash : constant := 1498876634;
		Energy_Hash : constant := 2465295065;
		Power_Hash : constant := 953998645;
		Shell_Hash : constant := 4023194814;
		Artefact_Hash : constant := 1506418338;

		Helmet_Hash : constant := 3448274439;
		Gauntlets_Hash : constant := 3551918588;
		Chest_Hash : constant := 14239492;
		Leg_Hash : constant := 20886954;
		Class_Hash : constant := 1585787867;

		function Make_Label (Hash : Manifest.Manifest_Hash) return Gtk_Label is
			Result : Gtk_Label;
		begin
			Gtk_New (Result, +The_Manifest.Destiny_Inventory_Buckets (Hash).Name);
			Result.Show;
			return Result;
		end Make_Label;

		Descriptions : constant Gtk_Grid := Gtk_Grid (GUI.Builder.Get_Object ("descriptions"));
	begin
		Descriptions.Attach (Make_Label (Kinetic_Hash), 0, 0);
		Descriptions.Attach (Make_Label (Energy_Hash), 0, 1);
		Descriptions.Attach (Make_Label (Power_Hash), 0, 2);
		Descriptions.Attach (Make_Label (Shell_Hash), 0, 3);
		Descriptions.Attach (Make_Label (Artefact_Hash), 0, 4);


		Descriptions.Attach (Make_Label (Helmet_Hash), 1, 0);
		Descriptions.Attach (Make_Label (Gauntlets_Hash), 1, 1);
		Descriptions.Attach (Make_Label (Chest_Hash), 1, 2);
		Descriptions.Attach (Make_Label (Leg_Hash), 1, 3);
		Descriptions.Attach (Make_Label (Class_Hash), 1, 4);
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
					if D.Category = Manifest.Equippable then
						Vault_Inventory (Bucket_Location'Enum_Val (D.Default_Bucket_Order)).Append (D);
					else
						Vault_Inventory (Unknown).Append (D);
					end if;
				exception
					when Constraint_Error =>
						Vault_Inventory (Unknown).Append (D);
				end;
			end if;
		end loop;

		-- Draw global inventories
		Render;
	end Update_Inventory;
end GUI.Global;
