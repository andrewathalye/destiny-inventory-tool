pragma Ada_2022;

with Ada.Containers.Hashed_Maps;

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

with Glib; use Glib;

-- Local Packages
with API.Profiles; use API.Profiles; -- Only for enums
with API.Manifest.Tools; use API.Manifest.Tools; -- Only for enums
use API.Manifest; -- Only for '='
use API; -- For general reference

with GUI.Base;

with Shared; use Shared;

with Tasks.Download;

package body GUI.Character is
	-- Instantiations
	type IDL_BLT_Array is array (Manifest.Tools.Bucket_Location_Type) of Base.Item_Description_List;
	type ID_BLT_Array is array (Manifest.Tools.Bucket_Location_Type) of Manifest.Tools.Item_Description;

	package IDL_BLT_Array_Maps is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => IDL_BLT_Array,
		Hash => Hash,
		Equivalent_Keys => Equivalent_Key);

	package ID_BLT_Array_Maps is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => ID_BLT_Array,
		Hash => Hash,
		Equivalent_Keys => Equivalent_Key);

	subtype IDL_BLT_Array_Map is IDL_BLT_Array_Maps.Map;
	subtype ID_BLT_Array_Map is ID_BLT_Array_Maps.Map;

	-- State
	All_Character_Items : IDL_BLT_Array_Map; -- Index by US then by M.BLT => B.IDL
	All_Equipped_Items : ID_BLT_Array_Map; -- Index by US then by M.BLT => M.T.ID
	
	-- Redirections
	procedure Render_Items (
		List : Base.Item_Description_List;
		Bucket : Gtk_Grid;
		T : Tasks.Download.Download_Task := Tasks.Download.Character_Task;
		Max_Left : Gint := 2)
	is begin
		Base.Render_Items (List, Bucket, T, Max_Left);
	end Render_Items;

	-- Cache
	Placeholder_Emblem : constant Gdk_Pixbuf := Load_Image (".png",
		Get_Data ("res/placeholder_emblem.png"));

	-- TODO WARNING EVERYTHING BELOW IS UNSAFE IF CURRENT_CHARACTER IS UNDEFINED
	procedure Render_Contents (Location : Manifest.Tools.Bucket_Location_Type)
	is
		Contents_Grid : constant Gtk_Grid := Gtk_Grid (Builder.Get_Object ("full_contents_grid"));
	begin
		Base.Clear_Bucket (Contents_Grid);
		Render_Items (
			All_Character_Items (Current_Character.Character_ID) (Location),
			Contents_Grid,
			Tasks.Download.Contents_Task);
	end Render_Contents;

	-- Popup full bucket contents if equipped item is clicked
	procedure Equipped_Clicked_Handler (
		Widget : access Gtk_Widget_Record'Class;
		User_Data : Manifest.Tools.Item_Description)
	is
		Contents : constant Gtk_Popover := Gtk_Popover (Builder.Get_Object ("full_contents"));
	begin
		-- Emotes are a special case here
		if User_Data.Bucket_Location = Emote_Collection then
			Render_Contents (Emote);
		else
			Render_Contents (User_Data.Bucket_Location);
		end if;

		Contents.Set_Relative_To (Widget);
		Contents.Popup;
	end Equipped_Clicked_Handler;

	-- Render an individual item onto a Gtk_Box
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

		Overlay := Base.Get_Overlay (
			D,
			Tasks.Download.Character_Task,
			Base.User_Callback_Item_Description.To_Marshaller (Equipped_Clicked_Handler'Access));
		Overlay.Show;

		-- Note: The alignment ensures the button is the same size as the icon
		Alignment.Add (Overlay);
		Alignment.Show;
		Box.Add (Alignment);
	end Render_Item;

	-- Public Subprograms
	-- (Virtual) Inventory Management
	procedure Add_Item (
		Character : Profiles.Character_Type;
		Item : Manifest.Tools.Item_Description)
	is
		Character_Items : IDL_BLT_Array renames All_Character_Items (Character.Character_ID);
		Relevant_Items : Base.Item_Description_List renames Character_Items (Item.Default_Bucket_Location);

		Modified_Item : Manifest.Tools.Item_Description := Item;
	begin
		Modified_Item.Location := Inventory;
		Modified_Item.Bucket_Location := Item.Default_Bucket_Location;
			-- Item is not vaulted (any longer), so ensure Bucket_Location
			-- is updated
		Modified_Item.Transfer_Status := Can_Transfer;

		Relevant_Items.Append (Modified_Item);
	end Add_Item;

	procedure Remove_Item (
		Character : Profiles.Character_Type;
		Item : Manifest.Tools.Item_Description)
	is
		Character_Items : IDL_BLT_Array renames All_Character_Items (Character.Character_ID);
		Relevant_Items : Base.Item_Description_List renames Character_Items (Item.Bucket_Location);
	begin
		for I in Relevant_Items.First_Index .. Relevant_Items.Last_Index loop
			if Relevant_Items (I) = Item then
				Relevant_Items.Delete (I);
				goto Done;
			end if;
		end loop;

		raise Program_Error with "GUI.Character: Virtual remove failed";
		<<Done>>
	end Remove_Item;

	procedure Equip_Item (
		Character : Profiles.Character_Type;
		Item : Manifest.Tools.Item_Description)
	is
		Equipped_Items : ID_BLT_Array renames All_Equipped_Items (Character.Character_ID);
		Relevant_Item : Manifest.Tools.Item_Description renames Equipped_Items (Item.Bucket_Location);
	begin
		-- Virtually transfer currently-equipped item to bucket
		Relevant_Item.Transfer_Status := Can_Transfer;
		Add_Item (Character, Relevant_Item);

		Relevant_Item := Item;
		Relevant_Item.Transfer_Status := Item_Is_Equipped;
	end Equip_Item;

	function Item_Count (
		Character : Profiles.Character_Type;
		Location : Manifest.Tools.Bucket_Location_Type) return Natural
	is (Natural (All_Character_Items (Character.Character_ID) (Location).Length));

	-- Status Updates
	-- Draw items from internal state
	procedure Render is
		-- Renames
		Character_Items : IDL_BLT_Array renames All_Character_Items (Current_Character.Character_ID);
		Equipped_Items : ID_BLT_Array renames All_Equipped_Items (Current_Character.Character_ID);

		-- Buckets
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
		-- Update Buckets
		Base.Clear_Bucket (Postmaster_Grid);
		Render_Items (
			Character_Items (Postmaster),
			Postmaster_Grid, 
			Tasks.Download.Character_Task,
			6);

		Base.Clear_Bucket (Subclass_Box);
		Render_Item (Equipped_Items (Subclass), Subclass_Box, Right);

		Base.Clear_Bucket (Kinetic_Box);
		Base.Clear_Bucket (Energy_Box);
		Base.Clear_Bucket (Power_Box);
		Base.Clear_Bucket (Shell_Box);
		Base.Clear_Bucket (Artefact_Box);
		Render_Item (Equipped_Items (Kinetic), Kinetic_Box, Right);
		Render_Item (Equipped_Items (Energy), Energy_Box, Right);
		Render_Item (Equipped_Items (Power), Power_Box, Right);
		Render_Item (Equipped_Items (Shell), Shell_Box, Right);
		Render_Item (Equipped_Items (Artefact), Artefact_Box, Right);

		Base.Clear_Bucket (Helmet_Box);
		Base.Clear_Bucket (Gauntlets_Box);
		Base.Clear_Bucket (Chest_Box);
		Base.Clear_Bucket (Leg_Box);
		Base.Clear_Bucket (Class_Box);
		Render_Item (Equipped_Items (Helmet), Helmet_Box, Left);
		Render_Item (Equipped_Items (Gauntlets), Gauntlets_Box, Left);
		Render_Item (Equipped_Items (Chest), Chest_Box, Left);
		Render_Item (Equipped_Items (Leg), Leg_Box, Left);
		Render_Item (Equipped_Items (Class), Class_Box, Left);

		Base.Clear_Bucket (Emblem_Box);
		Base.Clear_Bucket (Sparrow_Box);
		Base.Clear_Bucket (Ship_Box);
		Render_Item (Equipped_Items (Emblem), Emblem_Box, Right);
		Render_Item (Equipped_Items (Sparrow), Sparrow_Box, Right);
		Render_Item (Equipped_Items (Ship), Ship_Box, Right);

		Base.Clear_Bucket (Finisher_Box);
		Base.Clear_Bucket (Emote_Box);
		Render_Item (Equipped_Items (Finisher), Finisher_Box, Left);
		Render_Item (Equipped_Items (Emote_Collection), Emote_Box, Left);

		-- TODO: Render engrams?
	end Render;

	-- Update UI elements for new character
	procedure Update_For_Character (Character : Profiles.Character_Type) is
		-- Labels and Images to be updated for each character
		Title : constant Gtk_Label := Gtk_Label (Builder.Get_Object ("title"));
		Light : constant Gtk_Label := Gtk_Label (Builder.Get_Object ("light"));
		Emblem_Button : constant Gtk_Button := Gtk_Button (Builder.Get_Object ("emblem_button"));

		Emblem : Gtk_Image;
	begin
		Current_Character := Character;

		-- Update Labels
		Set_Label (Title, +Manifest.Tools.Get_Title (The_Manifest, Current_Character));
		Set_Label (Light, Current_Character.Light'Image);

		-- Update Emblem
		Gtk_New (Emblem);
		if Global_Pixbuf_Cache.Contains (Character.Emblem_Background_Path) then
			Emblem.Set (Global_Pixbuf_Cache.Element (Character.Emblem_Background_Path));
		else
			Emblem.Set (Placeholder_Emblem);
			Tasks.Download.Character_Task.Download (Character.Emblem_Background_Path, Gtk_Widget (Emblem));
		end if;

		Emblem_Button.Set_Image (Emblem);

		Render;
	end Update_For_Character;

	-- Update inventory data for characters
	procedure Update_Characters (Profile : Profiles.Profile_Type)
	is
		Blank_IDL_BLT_Array : IDL_BLT_Array;
		Blank_ID_BLT_Array : ID_BLT_Array;
	begin
		Put_Debug ("Update character inventories");

		-- Clear existing data
		All_Character_Items.Clear;
		All_Equipped_Items.Clear;

		-- Add character data
		Add_Characters :
		for C of Profile.Characters loop
			All_Character_Items.Insert (C.Character_ID, Blank_IDL_BLT_Array);
			All_Equipped_Items.Insert (C.Character_ID, Blank_ID_BLT_Array);

			-- Inventory Items (not equipped)
			for I of Profile.Character_Inventories (C.Character_ID) loop
				declare
					D : constant Manifest.Tools.Item_Description := Manifest.Tools.Get_Description (The_Manifest, I);
				begin
					All_Character_Items (C.Character_ID) (D.Bucket_Location).Append (D);
				end;
			end loop;

			for I of Profile.Character_Equipment (C.Character_ID) loop
				declare
					D : constant Manifest.Tools.Item_Description := Manifest.Tools.Get_Description (The_Manifest, I);
				begin
					All_Equipped_Items (C.Character_ID) (D.Bucket_Location) := D;	
				end;
			end loop;
		end loop Add_Characters;
	end Update_Characters;
	
	-- Internal Callbacks
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
