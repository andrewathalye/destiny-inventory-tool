pragma Ada_2022;

-- Gtk
with Gtk.Label; use Gtk.Label;
with Gtk.Image; use Gtk.Image;
with Gtk.Grid; use Gtk.Grid;

-- AWS
with AWS.Response;
with AWS.Client;
use AWS;

-- Local Packages
with API.Manifest; use API.Manifest;
with Shared; use Shared;

package body GUI.Character is
	-- State
	Character_Index : Natural := 0;
	Character_Items : array (Bucket_Location) of Item_Description_List;

	-- Subprograms
	procedure Switch_Button_Clicked_Handler (Builder : access Gtkada_Builder_Record'Class) is
	begin
		Character_Index := @ + 1;

		if Character_Index >= Natural (Profile.Characters.Length) then
			Character_Index := 0;
		end if;

		Update_For_Character (Profile.Characters.Element (Character_Index));
	end Switch_Button_Clicked_Handler;

	procedure Update_For_Character (Character : Profiles.Character_Type) is
		-- Labels and Images to be updated for each character
		Title : Gtk_Label := Gtk_Label (Builder.Get_Object ("title"));
		Light : Gtk_Label := Gtk_Label (Builder.Get_Object ("light"));
		Emblem : Gtk_Image := Gtk_Image (Builder.Get_Object ("emblem"));

		-- Buckets (Grids) that need to be updated
		Kinetic_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("kinetic"));
		Energy_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("energy"));
		Power_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("power"));

		Helmet_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("helmet"));
		Gauntlets_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("gauntlets"));
		Chest_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("chest"));
		Leg_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("leg"));
		Class_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("class"));
	begin
		-- Update Labels
		Set_Label (Title, +Manifest.Tools.Get_Title (The_Manifest, Character));
		Set_Label (Light, Character.Light'Image);

		-- Update Emblem
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

		-- Update Items
		for IDL of Character_Items loop
			IDL.Clear;
		end loop;

		-- Inventory Items (not equipped)
		for I of Profile.Character_Inventories (Character.Character_ID) loop
			declare
				D : Manifest.Tools.Item_Description := Manifest.Tools.Get_Description (The_Manifest, I);
			begin
				if D.Category = Manifest.Equippable then
					Character_Items (Bucket_Location'Enum_Val (D.Bucket_Order)).Append (D);	
				end if;
			exception
				when Constraint_Error =>
					Character_Items (Unknown).Append (D);
			end;
		end loop;

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

	end Update_For_Character;
end GUI.Character;
