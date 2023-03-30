pragma Ada_2022;

-- Gtk
with Gtk.Label; use Gtk.Label;
with Gtk.Image; use Gtk.Image;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Button; use Gtk.Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Box; use Gtk.Box;
with Gtk.Overlay; use Gtk.Overlay;
with Gtk.Alignment; use Gtk.Alignment;

-- AWS
with AWS.Response;
with AWS.Client;
use AWS;

-- Local Packages
with API.Manifest; use API.Manifest;
use API;
with Shared; use Shared;

package body GUI.Character is
	-- State
	Character_Items : array (Bucket_Location) of Item_Description_List;
	Equipped_Items : array (Bucket_Location) of Manifest.Tools.Item_Description;

	type Item_Alignment_Type is (Left, Centre, Right);
	procedure Render_Item (
		D : Manifest.Tools.Item_Description;
		Box : Gtk_Box;
		Item_Alignment : Item_Alignment_Type := Centre)
	is
		Overlay : Gtk_Overlay := Get_Overlay (D);
		Alignment : Gtk_Alignment := Gtk_Alignment_New (
			Xalign => (case Item_Alignment is
				when Left => 0.0,
				when Centre => 0.5,
				when Right => 1.0),
			Yalign => 0.5,
			Xscale => 0.0,
			Yscale => 0.0);
	begin
		Overlay.Show;

		-- Note: The alignment ensures the button is the same size as the icon
		Alignment.Add (Overlay);
		Alignment.Show;
		Box.Add (Alignment);
	end Render_Item;

	procedure Render is
		-- Buckets (Grids) that need to be updated
--		Kinetic_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("kinetic"));
--		Energy_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("energy"));
--		Power_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("power"));

--		Helmet_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("helmet"));
--		Gauntlets_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("gauntlets"));
--		Chest_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("chest"));
--		Leg_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("leg"));
--		Class_Bucket : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("class"));

		Kinetic_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("kinetic"));
		Energy_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("energy"));
		Power_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("power"));
		Shell_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("shell"));
		Artefact_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("artefact"));

		Helmet_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("helmet"));
		Gauntlets_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("gauntlets"));
		Chest_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("chest"));
		Leg_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("leg"));
		Class_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("class"));

		Emblem_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("emblem"));
		Sparrow_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("sparrow"));
		Ship_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("ship"));

		Finisher_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("finisher"));
	begin
		Render_Item (Equipped_Items (Kinetic), Kinetic_Box, Right);
		Render_Item (Equipped_Items (Energy), Energy_Box, Right);
		Render_Item (Equipped_Items (Power), Power_Box, Right);
		Render_Item (Equipped_Items (Shell), Shell_Box, Right);
		Render_Item (Equipped_Items (Artefact), Artefact_Box, Right);

		Render_Item (Equipped_Items (Helmet), Helmet_Box, Left);
		Render_Item (Equipped_Items (Gauntlets), Gauntlets_Box, Left);
		Render_Item (Equipped_Items (Chest), Chest_Box, Left);
		Render_Item (Equipped_Items (Leg), Leg_Box, Left);
		Render_Item (Equipped_Items (Class), Class_Box, Left);

		Render_Item (Equipped_Items (Emblem), Emblem_Box, Right);
		Render_Item (Equipped_Items (Sparrow), Sparrow_Box, Right);
		Render_Item (Equipped_Items (Ship), Ship_Box, Right);

		Render_Item (Equipped_Items (Finisher), Finisher_Box, Left);

--		Clear_Bucket (Kinetic_Bucket);
--		Render_Items (Character_Items (Kinetic), Kinetic_Bucket);

--		Clear_Bucket (Energy_Bucket);
--		Render_Items (Character_Items (Energy), Energy_Bucket);

--		Clear_Bucket (Power_Bucket);
--		Render_Items (Character_Items (Power), Power_Bucket);

--		Clear_Bucket (Helmet_Bucket);
--		Render_Items (Character_Items (Helmet), Helmet_Bucket);

--		Clear_Bucket (Gauntlets_Bucket);
--		Render_Items (Character_Items (Gauntlets), Gauntlets_Bucket);

--		Clear_Bucket (Chest_Bucket);
--		Render_Items (Character_Items (Chest), Chest_Bucket);

--		Clear_Bucket (Leg_Bucket);
--		Render_Items (Character_Items (Leg), Leg_Bucket);

--		Clear_Bucket (Class_Bucket);
--		Render_Items (Character_Items (Class), Class_Bucket);

		-- TODO: Render engrams and postmaster, render equipped items
	end Render;

	procedure Update_For_Character (Character : Profiles.Character_Type) is
		-- Constants
		-- Labels and Images to be updated for each character
		Title : constant Gtk_Label := Gtk_Label (Builder.Get_Object ("title"));
		Light : constant Gtk_Label := Gtk_Label (Builder.Get_Object ("light"));
		Emblem_Button : constant Gtk_Button := Gtk_Button (Builder.Get_Object ("emblem_button"));

		Emblem : Gtk_Image;
	begin
		-- Update Labels
		Set_Label (Title, +Manifest.Tools.Get_Title (The_Manifest, Character));
		Set_Label (Light, Character.Light'Image);

		-- Update Emblem (technically a Global UI element) todo
		Gtk_New (Emblem);
		if Has_Cached (+Character.Emblem_Background_Path) then
--			Put_Debug ("Load cached emblem");
			Set (Emblem, Caching_Load_Image (Character.Emblem_Background_Path,
				Get_Cache_Path (+Character.Emblem_Background_Path)));
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

		for I of Profile.Character_Equipment (Character.Character_ID) loop
			declare
				D : constant Manifest.Tools.Item_Description := Manifest.Tools.Get_Description (The_Manifest, I);
			begin
				if D.Category = Manifest.Equippable then
					Equipped_Items (Bucket_Location'Enum_Val (D.Bucket_Order)) := D;	
				end if;
			exception
				when Constraint_Error => null;
				-- raise Program_Error with "Equipment parse failed";
			end;

		end loop;

		Render;
	end Update_For_Character;
end GUI.Character;
