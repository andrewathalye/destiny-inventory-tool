pragma Ada_2022;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Interfaces;

-- Gtk
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Popover; use Gtk.Popover;

-- Local Packages
with GUI.Character;
with GUI.Global;
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
		GUI.Search_Query := +Search.Get_Chars (0);
		GUI.Global.Render;
		GUI.Character.Render;
	end Search_Changed_Handler;
	
	-- TODO: Find a better setup
	Entries : Natural := 0;

	procedure Contents_Closed_Handler (Builder : access Gtkada_Builder_Record'Class)
	is begin
		Entries := 0;
	end Contents_Closed_Handler;

	function Contents_Leave_Handler (Builder : access Gtkada_Builder_Record'Class) return Boolean
	is
		Contents : constant Gtk_Popover := Gtk_Popover (Builder.Get_Object ("full_contents"));
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
	pragma Warnings (On, "is not referenced");

	-- Install Handlers
	procedure Set_Handlers is
	begin
		Register_Handler (GUI.Builder, "window_close_handler", Window_Close_Handler'Access);

		Register_Handler (Builder, "emblem_button_clicked_handler", Emblem_Button_Clicked_Handler'Access);

		Register_Handler (Builder, "search_changed_handler", Search_Changed_Handler'Access);

		Register_Handler (GUI.Builder, "contents_closed_handler", Contents_Closed_Handler'Access);
		Register_Handler (GUI.Builder, "contents_leave_handler", Contents_Leave_Handler'Access);
		Register_Handler (GUI.Builder, "contents_enter_handler", Contents_Enter_Handler'Access);
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

	procedure Switch_Item (
		D : Item_Description;
		New_Bucket_Location : Bucket_Location_Type;
		Source : in out Item_Description_List;
		Target : in out Item_Description_List)
	is
		use Interfaces;

		Modified_D : Item_Description := D;
		Idx : Integer := -1;
	begin
		for I in Source.First_Index .. Source.Last_Index loop
			if 
				Source (I).Item_Hash = D.Item_Hash
				and Source (I).Item_Instance_ID = D.Item_Instance_ID
			then
				Idx := I;
			end if;
		end loop;

		if Idx /= -1 then
			Source.Delete (Natural (Idx));
		end if;

		Modified_D.Bucket_Location := New_Bucket_Location;

		Target.Append (Modified_D);
	end Switch_Item;

	-- The two handlers below are additionally responsible for simulating the
	-- transfer clientside
	-- TODO none of this is safe for multiple characters yet
	procedure Transfer_Handler (
		Button : access Gtk_Widget_Record'Class;
		Target : Profiles.Character_Type)
	is begin
		-- Unvault
		if GUI.Current_Item.Bucket_Location = General then
--			Transfers.Unvault (GUI.Current_Item, Target);

			Switch_Item (
				GUI.Current_Item,
				GUI.Current_Item.Default_Bucket_Location,
				Vault_Inventory (GUI.Current_Item.Default_Bucket_Location),
				Character_Items (GUI.Current_Item.Default_Bucket_Location));

			GUI.Global.Render;
			return;
		end if;

		-- Retrieve from Postmaster
		if GUI.Current_Item.Bucket_Location = Postmaster then
--			Transfers.Postmaster_Pull (
--				GUI.Current_Item,
--				GUI.Character.Current_Character);

			-- Transfer to correct character
--			if GUI.Character.Current_Character /= Target then
--				Transfers.Transfer (
--					GUI.Current_Item,
--					GUI.Character.Current_Character,
--					Target);
--			end if;

			Switch_Item (
				GUI.Current_Item,
				GUI.Current_Item.Default_Bucket_Location,
				Character_Items (GUI.Current_Item.Bucket_Location),
				Character_Items (GUI.Current_Item.Default_Bucket_Location));

			GUI.Character.Render;
			return;
		end if;

		-- Equip
		if GUI.Character.Current_Character = Target and Current_Item.Category = Equippable
		then
--			Transfers.Equip (GUI.Current_Item, Target);

			Equipped_Items (GUI.Current_Item.Default_Bucket_Location) := GUI.Current_Item;
			GUI.Character.Render;
			return;
		end if;

		-- Normal Transfer
--		Transfers.Transfer (
--			GUI.Current_Item,
--			GUI.Character.Current_Character,
--			Target);	

		Switch_Item (
			GUI.Current_Item,
			GUI.Current_Item.Default_Bucket_Location,
			Character_Items (GUI.Current_Item.Default_Bucket_Location),
			Character_Items (GUI.Current_Item.Default_Bucket_Location)); --todo big issue
	end Transfer_Handler;


	procedure Vault_Handler (Button : access Gtk_Widget_Record'Class)
	is
	begin
--		Transfers.Vault (
--			GUI.Current_Item,
--			GUI.Character.Current_Character);

		Switch_Item (
			GUI.Current_Item,
			General,
			Character_Items (GUI.Current_Item.Default_Bucket_Location),
			Vault_Inventory (GUI.Current_Item.Default_Bucket_Location));
	end Vault_Handler;
	pragma Warnings (On, "is not referenced");
end GUI.Handlers;
