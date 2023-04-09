pragma Ada_2022;

with Interfaces; use Interfaces;

-- Gtk
with Gtk.Label; use Gtk.Label;
with Gtk.Image; use Gtk.Image;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Button; use Gtk.Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Box; use Gtk.Box;
with Gtk.Overlay; use Gtk.Overlay;
with Gtk.Popover; use Gtk.Popover;
with Gtk.Alignment; use Gtk.Alignment;

with Gdk.Pixbuf; use Gdk.Pixbuf;

-- Local Packages
with API.Manifest; use API.Manifest;
use API;
with Shared; use Shared;

package body GUI.Character is
	-- State
	Character_Items : array (Bucket_Location) of Item_Description_List;
	Equipped_Items : array (Bucket_Location) of Manifest.Tools.Item_Description;

	-- Redirections
	procedure Render_Items (
		List : Item_Description_List;
		Bucket : Gtk_Grid;
		T : Tasks.Download.Download_Task := Tasks.Download.Character_Task;
		Max_Left : Gint := 2)
	is begin
		GUI.Render_Items (List, Bucket, T, Max_Left);
	end Render_Items;

	-- Cache
	Placeholder_Emblem : constant Gdk_Pixbuf := Load_Image (".png",
		Get_Data ("res/placeholder_emblem.png"));

	function Item_Overlay_Entered_Handler (Widget : access Gtk_Widget_Record'Class; User_Data : Manifest.Tools.Item_Description) return GBoolean
	is
		Contents : constant Gtk_Popover := Gtk_Popover (Builder.Get_Object ("full_contents"));
		Contents_Grid : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("full_contents_grid"));
	begin
		Clear_Bucket (Contents_Grid);

		-- TODO Emotes are special and only some are supported currently
		if User_Data.Bucket_Hash = Emote_Collection'Enum_Rep then
			Render_Items (
				Character_Items (Special_Emote),
				Contents_Grid,
				Tasks.Download.Contents_Task);
		else
			Render_Items (
				Character_Items (
					Bucket_Location'Enum_Val (
						User_Data.Bucket_Hash)),
				Contents_Grid,
				Tasks.Download.Contents_Task);
		end if;

		Contents.Set_Relative_To (Widget);
		Contents.Popup;

		return 1; -- Propagate further signals
	end Item_Overlay_Entered_Handler;

	type Item_Alignment_Type is (Left, Centre, Right);
	procedure Render_Item (
		D : Manifest.Tools.Item_Description;
		Box : Gtk_Box;
		Item_Alignment : Item_Alignment_Type := Centre)
	is
		Overlay : Gtk_Overlay;
		Alignment : constant Gtk_Alignment := Gtk_Alignment_New (
			Xalign => (case Item_Alignment is
				when Left => 0.0,
				when Centre => 0.5,
				when Right => 1.0),
			Yalign => 0.5,
			Xscale => 0.0,
			Yscale => 0.0);
	begin
		-- Skip rendering the item if the bucket is empty
		if Length (D.Name) = 0 then
			return;
		end if;

		Overlay := Get_Overlay (D, Tasks.Download.Character_Task);

		Connect (Widget_List.Get_Data (Widget_List.First (Overlay.Get_Children)),
			"enter-notify-event",
			To_Marshaller (Item_Overlay_Entered_Handler'Access),
			User_Data => D);

		Overlay.Show;

		-- Note: The alignment ensures the button is the same size as the icon
		Alignment.Add (Overlay);
		Alignment.Show;
		Box.Add (Alignment);
	end Render_Item;

	procedure Render is
		Postmaster_Grid : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("postmaster"));

		Subclass_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("subclass"));

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
		Emote_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("emote"));
	begin
		Clear_Bucket (Postmaster_Grid);
		Render_Items (
			Character_Items (Postmaster),
			Postmaster_Grid, 
			Tasks.Download.Character_Task,
			6);

		Clear_Bucket (Subclass_Box);
		Render_Item (Equipped_Items (Subclass), Subclass_Box, Right);

		Clear_Bucket (Kinetic_Box);
		Clear_Bucket (Energy_Box);
		Clear_Bucket (Power_Box);
		Clear_Bucket (Shell_Box);
		Clear_Bucket (Artefact_Box);
		Render_Item (Equipped_Items (Kinetic), Kinetic_Box, Right);
		Render_Item (Equipped_Items (Energy), Energy_Box, Right);
		Render_Item (Equipped_Items (Power), Power_Box, Right);
		Render_Item (Equipped_Items (Shell), Shell_Box, Right);
		Render_Item (Equipped_Items (Artefact), Artefact_Box, Right);

		Clear_Bucket (Helmet_Box);
		Clear_Bucket (Gauntlets_Box);
		Clear_Bucket (Chest_Box);
		Clear_Bucket (Leg_Box);
		Clear_Bucket (Class_Box);
		Render_Item (Equipped_Items (Helmet), Helmet_Box, Left);
		Render_Item (Equipped_Items (Gauntlets), Gauntlets_Box, Left);
		Render_Item (Equipped_Items (Chest), Chest_Box, Left);
		Render_Item (Equipped_Items (Leg), Leg_Box, Left);
		Render_Item (Equipped_Items (Class), Class_Box, Left);

		Clear_Bucket (Emblem_Box);
		Clear_Bucket (Sparrow_Box);
		Clear_Bucket (Ship_Box);
		Render_Item (Equipped_Items (Emblem), Emblem_Box, Right);
		Render_Item (Equipped_Items (Sparrow), Sparrow_Box, Right);
		Render_Item (Equipped_Items (Ship), Ship_Box, Right);

		Clear_Bucket (Finisher_Box);
		Clear_Bucket (Emote_Box);
		Render_Item (Equipped_Items (Finisher), Finisher_Box, Left);
		Render_Item (Equipped_Items (Emote_Collection), Emote_Box, Left);

		-- TODO: Render engrams
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
		if Global_Pixbuf_Cache.Contains (Character.Emblem_Background_Path) then
--			Put_Debug ("Load cached emblem");
			Emblem.Set (Global_Pixbuf_Cache.Element (Character.Emblem_Background_Path));
		else
--			Put_Debug ("Get emblem");
			Emblem.Set (Placeholder_Emblem);
			Tasks.Download.Character_Task.Download (Character.Emblem_Background_Path, Gtk_Widget (Emblem));
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
				Character_Items (Bucket_Location'Enum_Val (I.Bucket_Hash)).Append (D);	
			exception
				when Constraint_Error =>
					Character_Items (Unknown).Append (D);
			end;
		end loop;

		for I of Profile.Character_Equipment (Character.Character_ID) loop
			declare
				D : constant Manifest.Tools.Item_Description := Manifest.Tools.Get_Description (The_Manifest, I);
			begin
				Equipped_Items (Bucket_Location'Enum_Val (I.Bucket_Hash)) := D;	
			exception
				when Constraint_Error =>
					raise Program_Error with "Equipment parse failed";
			end;
		end loop;

		Render;
	end Update_For_Character;

	procedure Tick is
		Download_Data : Tasks.Download.Download_Data_Type;
	begin
		select
			Tasks.Download.Character_Task.Complete (Download_Data);
			GUI.Image_Callback (Download_Data.Path, Download_Data.Widget, Download_Data.Data);
		else
			select
				Tasks.Download.Character_Task.Execute;
			else
				null;
			end select;
		end select;

		select
			Tasks.Download.Contents_Task.Complete (Download_Data);
			GUI.Image_Callback (Download_Data.Path, Download_Data.Widget, Download_Data.Data);
		else
			select
				Tasks.Download.Contents_Task.Execute;
			else
				null;
			end select;
		end select;

	end Tick;

end GUI.Character;
