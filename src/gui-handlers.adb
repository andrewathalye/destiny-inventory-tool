pragma Ada_2022;

with GNAT.OS_Lib; use GNAT.OS_Lib;

-- Gtk
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Popover; use Gtk.Popover;

-- Local Packages
with GUI.Character;
with GUI.Global;
with GUI.Base;

with API.Transfers;
with API.Manifest.Tools; use API.Manifest.Tools; -- For enums
with API.Profiles; use API.Profiles; -- For '='
with API.Manifest; use API.Manifest; -- For '='
use API;

with Shared; use Shared;

package body GUI.Handlers is
	-- Global Handlers (Private)
	pragma Warnings (Off, "is not referenced");
	procedure Window_Close_Handler (Builder : access Gtkada_Builder_Record'Class) is
	begin
		OS_Exit (0);
	end Window_Close_Handler;
	 
	procedure Emblem_Button_Clicked_Handler (Builder : access Gtkada_Builder_Record'Class) is
		Character_Menu : constant Gtk_Popover := Gtk_Popover (GUI.Builder.Get_Object ("character_menu"));
	begin
		Character_Menu.Popup;
	end Emblem_Button_Clicked_Handler;

	procedure Search_Changed_Handler (Builder : access Gtkada_Builder_Record'Class) is
		Search : constant Gtk_Search_Entry := Gtk_Search_Entry (GUI.Builder.Get_Object ("search"));
	begin
		Base.Search_Query := +Search.Get_Chars (0);
		GUI.Global.Render;
		GUI.Character.Render;
	end Search_Changed_Handler;
	
	procedure Postmaster_Vault_Handler (Button : access Gtk_Widget_Record'Class)
	is begin
		Put_Debug ("Postmaster Vault Item");

		begin
			Transfers.Postmaster_Pull (
				GUI.Current_Item,
				GUI.Character.Current_Character);
		exception
			when Transfers.Out_Of_Space =>
				Put_Debug ("Out of Vault space, aborting attempt");
				return;
		end;

		-- Update UI State
		GUI.Character.Remove_Item (GUI.Character.Current_Character, GUI.Current_Item);
		GUI.Global.Add_Item (GUI.Current_Item);

		GUI.Global.Render;
		GUI.Character.Render;
	end Postmaster_Vault_Handler;
	pragma Warnings (On, "is not referenced");

	-- Install Handlers
	procedure Set_Handlers is
		Vault_Button : constant Gtk_Widget := Gtk_Widget (Builder.Get_Object ("vault_button"));
	begin
		Register_Handler (GUI.Builder, "window_close_handler", Window_Close_Handler'Access);

		Register_Handler (Builder, "emblem_button_clicked_handler", Emblem_Button_Clicked_Handler'Access);

		Register_Handler (Builder, "search_changed_handler", Search_Changed_Handler'Access);

		Widget_Callback.Connect (
			Vault_Button,
			"clicked",
			Widget_Callback.To_Marshaller (
				Postmaster_Vault_Handler'Access));

	end Set_Handlers;

	-- Dynamic Handlers (Public)
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
	
	-- The two handlers below are additionally responsible for simulating the
	-- transfer clientside
	procedure Transfer_Handler (
		Button : access Gtk_Widget_Record'Class;
		Target : Profiles.Character_Type)
	is begin
		Put_Debug ("Item Transfer");
		Put_Debug ("Source: " & Manifest.Tools.Get_Description (
			The_Manifest,
			GUI.Character.Current_Character));
		Put_Debug ("Target: " & Manifest.Tools.Get_Description (
			The_Manifest,
			Target));

		-- Unvault
		if GUI.Current_Item.Location = Vault then
			Put_Debug ("Method: Unvault");
			begin
				Transfers.Unvault (GUI.Current_Item, Target);
			exception
				when Transfers.Out_Of_Space =>
					Put_Debug ("Couldn't unvault because the destination is out of space");
					return;
				when Transfers.Already_Here =>
					Put_Debug ("Couldn't unvault is already where it would have been sent");
					return;
			end;

			-- Update UI State
			GUI.Global.Remove_Item (GUI.Current_Item);
			GUI.Character.Add_Item (Target, GUI.Current_Item);
			GUI.Global.Render;
			return;
		end if;

		-- Retrieve from Postmaster
		-- Note: Can't check Location because Postmaster isn't always returned for that
		if GUI.Current_Item.Bucket_Location = Postmaster then
			Put_Debug ("Method: Postmaster_Pull");
			begin
				Transfers.Postmaster_Pull (
					GUI.Current_Item,
					GUI.Character.Current_Character);
			exception
				when Transfers.Out_Of_Space =>
					Put_Debug ("Failed to pull from postmaster: out of space");
					return;
			end;

			-- Transfer to correct character
			if GUI.Character.Current_Character /= Target then
				begin
					Transfers.Transfer (
						GUI.Current_Item,
						GUI.Character.Current_Character,
						Target);
				exception -- The previous action cannot be undone, so keep going
					when Transfers.Out_Of_Space =>
						Put_Debug ("Failed to finish transfer, but postmaster pull can't be rolled back!");
				end;
			end if;

			-- Update UI State
			GUI.Character.Remove_Item (GUI.Character.Current_Character, GUI.Current_Item);
			GUI.Character.Add_Item (Target, GUI.Current_Item);
			GUI.Character.Render;
			return;
		end if;

		-- Equip
		if GUI.Character.Current_Character = Target and Current_Item.Category = Equippable
		then
			Put_Debug ("Method: Equip");
			Transfers.Equip (GUI.Current_Item, Target);

			-- Update UI State
			GUI.Character.Remove_Item (Target, GUI.Current_Item);
			GUI.Character.Equip_Item (Target, GUI.Current_Item);
			GUI.Character.Render;
			return;
		end if;

		-- Normal Transfer
		Put_Debug ("Method: Transfer");

		begin
			Transfers.Transfer (
				GUI.Current_Item,
				GUI.Character.Current_Character,
				Target);	
		exception
			when Transfers.Out_Of_Space =>
				Put_Debug ("Out of space somewhere along the chain, aborting transfer");
				return;
		end;

		-- Update UI State
		GUI.Character.Remove_Item (GUI.Character.Current_Character, GUI.Current_Item);
		GUI.Character.Add_Item (Target, GUI.Current_Item);
		GUI.Character.Render_Contents (GUI.Current_Item.Bucket_Location);
	end Transfer_Handler;

	-- Vault an Item
	procedure Vault_Handler (Button : access Gtk_Widget_Record'Class)
	is
	begin
		Put_Debug ("Vault Item");

		begin
			Transfers.Vault (
				GUI.Current_Item,
				GUI.Character.Current_Character);
		exception
			when Transfers.Out_Of_Space =>
				Put_Debug ("Out of Vault space, aborting attempt");
				return;
			when Transfers.Already_Here =>
				Put_Debug ("The item was already there, aborting attempt");
				return;
		end;

		GUI.Character.Remove_Item (GUI.Character.Current_Character, GUI.Current_Item);
		GUI.Global.Add_Item (GUI.Current_Item);

		GUI.Global.Render;

		-- Redraw as little as possible for performance :)
		if GUI.Current_Item.Location = Postmaster then
			GUI.Character.Render;
		else
			GUI.Character.Render_Contents (GUI.Current_Item.Bucket_Location);
				-- A smaller render that will be faster (hopefully)
		end if;
	end Vault_Handler;
	pragma Warnings (On, "is not referenced");

	-- Display Transfer Menu When Item is Clicked
	procedure Item_Button_Handler (
		Widget : access Gtk_Widget_Record'Class;
		User_Data : Manifest.Tools.Item_Description)
	is
		Transfer_Menu : constant Gtk_Popover := Gtk_Popover (Builder.Get_Object ("transfer_menu"));
		Vault_Menu : constant Gtk_Popover := Gtk_Popover (Builder.Get_Object ("vault_menu"));
	begin
		Current_Item := User_Data;

		-- Don't show the normal transfer menu for
		-- nontransferrables
		if User_Data.Transfer_Status /= Can_Transfer then
			-- It is often possible to transfer items
			-- in the postmaster to the Vault
			if User_Data.Bucket_Location = Postmaster and not User_Data.Postmaster_Pull_Has_Side_Effects then
				Vault_Menu.Set_Relative_To (Widget);
				Vault_Menu.Popup;
			end if;

			return;
		end if;

		Transfer_Menu.Set_Relative_To (Widget);
		Transfer_Menu.Popup;
	end Item_Button_Handler;
end GUI.Handlers;
