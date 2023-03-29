pragma Ada_2022;

-- Gtk
with Gtk.Label; use Gtk.Label;
with Gtk.Image; use Gtk.Image;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Button; use Gtk.Button;
with Gtk.Popover; use Gtk.Popover;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget; use Gtk.Widget;

-- AWS
with AWS.Response;
with AWS.Client;
use AWS;

-- Local Packages
with API.Manifest; use API.Manifest;
with Shared; use Shared;

package body GUI.Character is
	-- Instantiations
	package User_Callback_Natural is new User_Callback (Gtk_Widget_Record, Natural);
	use User_Callback_Natural;

	-- State
	Character_Items : array (Bucket_Location) of Item_Description_List;

	-- Subprograms
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
		Update_For_Character (Profile.Characters.Element (User_Data));
	end Character_Menu_Button_Clicked_Handler;
	pragma Warnings (On, "is not referenced");

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

	procedure Render is
		-- Buckets (Grids) that need to be updated
		Kinetic_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("kinetic"));
		Energy_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("energy"));
		Power_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("power"));

		Helmet_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("helmet"));
		Gauntlets_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("gauntlets"));
		Chest_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("chest"));
		Leg_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("leg"));
		Class_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("class"));
	begin
		Clear_Bucket (Kinetic_Bucket);
		Render_Items (Character_Items (Kinetic), Kinetic_Bucket);

		Clear_Bucket (Energy_Bucket);
		Render_Items (Character_Items (Energy), Energy_Bucket);

		Clear_Bucket (Power_Bucket);
		Render_Items (Character_Items (Power), Power_Bucket);

		Clear_Bucket (Helmet_Bucket);
		Render_Items (Character_Items (Helmet), Helmet_Bucket);

		Clear_Bucket (Gauntlets_Bucket);
		Render_Items (Character_Items (Gauntlets), Gauntlets_Bucket);

		Clear_Bucket (Chest_Bucket);
		Render_Items (Character_Items (Chest), Chest_Bucket);

		Clear_Bucket (Leg_Bucket);
		Render_Items (Character_Items (Leg), Leg_Bucket);

		Clear_Bucket (Class_Bucket);
		Render_Items (Character_Items (Class), Class_Bucket);

		-- TODO: Render engrams and postmaster, render equipped items
	end Render;

	procedure Update_For_Character (Character : Profiles.Character_Type) is
		-- Labels and Images to be updated for each character
		Title : constant Gtk_Label := Gtk_Label (Builder.Get_Object ("title"));
		Light : constant Gtk_Label := Gtk_Label (Builder.Get_Object ("light"));
		Emblem_Button : constant Gtk_Button := Gtk_Button (Builder.Get_Object ("emblem_button"));

		Emblem : Gtk_Image;
	begin
		-- Update Labels
		Set_Label (Title, +Manifest.Tools.Get_Title (The_Manifest, Character));
		Set_Label (Light, Character.Light'Image);

		-- Update Emblem
		Gtk_New (Emblem);
		if Has_Cached (+Character.Emblem_Background_Path) then
			Put_Debug ("Load cached emblem");
			Set (Emblem, Load_Image (
				+Character.Emblem_Background_Path,
				Get_Cached (+Character.Emblem_Background_Path)));
		else
			declare
				Data : Response.Data;
			begin
				Put_Debug ("Get emblem");
				Data := Client.Get (Bungie_Root & (+Character.Emblem_Background_Path));
				Cache (+Character.Emblem_Background_Path, Response.Message_Body (Data));
				Set (Emblem, Load_Image (
					+Character.Emblem_Background_Path,
					Response.Message_Body (Data)));
			end;
		end if;

		Emblem_Button.Set_Image (Emblem);

		Setup_Character_Menu;

		-- Update Items
		for IDL of Character_Items loop
			IDL.Clear;
		end loop;

		-- Inventory Items (not equipped)
		for I of Profile.Character_Inventories (Character.Character_ID) loop
			declare
				D : constant Manifest.Tools.Item_Description := Manifest.Tools.Get_Description (The_Manifest, I);
			begin
				if D.Category = Manifest.Equippable then
					Character_Items (Bucket_Location'Enum_Val (D.Bucket_Order)).Append (D);	
				end if;
			exception
				when Constraint_Error =>
					Character_Items (Unknown).Append (D);
			end;
		end loop;

		Render;
	end Update_For_Character;
end GUI.Character;
