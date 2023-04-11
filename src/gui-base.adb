pragma Ada_2022;

with Interfaces; use Interfaces;

-- Gtk
with Gtk.Container; use Gtk.Container;
with Gtk.Button; use Gtk.Button;
with Gtk.Window; use Gtk.Window;
with Gtk.Main;
with Gtk.Image; use Gtk.Image;
with Gtk.Label; use Gtk.Label;
with Gtk.Alignment; use Gtk.Alignment;

with Pango.Attributes; use Pango.Attributes;

-- Local Packages
with GUI.Handlers;
with GUI.Character;
with GUI.Global;

with API.Authorise;

package body GUI.Base is
	-- Instantiations
	package FUD_Container is new Gtk.Container.Foreach_User_Data (Gtk_Container);

	-- Cached High-Frequency Pixbufs
	Placeholder_Icon : constant Gdk_Pixbuf := Load_Image ("png",
		Get_Data ("res/placeholder_icon.png"));
	Crafted_Masterwork_Overlay : constant Gdk_Pixbuf := Load_Image ("png",
		Get_Data ("res/crafted_masterwork_overlay.png"));
	Crafted_Overlay : constant Gdk_Pixbuf := Load_Image ("png",
		Get_Data ("res/crafted_overlay.png"));
	Masterwork_Overlay : constant Gdk_Pixbuf := Load_Image ("png",
		Get_Data ("res/masterwork_overlay.png"));
	Normal_Overlay : constant Gdk_Pixbuf := Load_Image ("png",
		Get_Data ("res/normal_overlay.png"));
	Ornament_Overlay : constant Gdk_Pixbuf := Load_Image ("png",
		Get_Data ("res/ornament_overlay.png"));

	-- Bucket Management
	-- Internal
	procedure Remove_Callback (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class; Container : Gtk_Container)
	is begin
		Container.Remove (Widget);
	end Remove_Callback;

	-- Exported
	procedure Clear_Bucket (G : Gtk_Grid)
	is begin
		FUD_Container.Foreach (Gtk_Container (G), Remove_Callback'Access, Gtk_Container (G));
	end Clear_Bucket;

	procedure Clear_Bucket (B : Gtk_Box)
	is begin
		FUD_Container.Foreach (Gtk_Container (B), Remove_Callback'Access, Gtk_Container (B));
	end Clear_Bucket;

	-- Inventory Item Rendering (Exported)
	function Get_Overlay (
		D : Manifest.Tools.Item_Description;
		T : Tasks.Download.Download_Task;
		Handler : User_Callback_Item_Description.Marshallers.Marshaller) return Gtk_Overlay
	is
		-- Subprograms
		-- TODO: Not 100% game accurate, but fast
		function Should_Watermark (D : Manifest.Tools.Item_Description) return Boolean is
			(case D.Item_Type is
				when Manifest.Emblem
					| Manifest.Subclass
					| Manifest.Consumable
					| Manifest.DIT_Mod
					| Manifest.Dummy
					| Manifest.None => False,
				when others => True) with Inline;

		function Should_State_Overlay (D : Manifest.Tools.Item_Description) return Boolean is
			(case D.Item_Type is
				when Manifest.Subclass => False,
				when Manifest.Engram => False,
				when others => True) with Inline;

		-- Variables
		Image : Gtk_Image;
		Button : Gtk_Button;
		Overlay : Gtk_Overlay;
		State_Overlay : Gtk_Image;
	begin
		Gtk_New (Image);
		Gtk_New (Button);
		Gtk_New (Overlay);

		-- Setup Icon and Button
		if Global_Pixbuf_Cache.Contains (D.Icon_Path) then
			Image.Set (Global_Pixbuf_Cache.Element (D.Icon_Path));
		else -- Asynchronously download the icon
			Image.Set (Placeholder_Icon);
			T.Download (D.Icon_Path, Gtk_Widget (Image));
		end if;

		Set_Image (Button, Image);

		User_Callback_Item_Description.Connect (
			Button,
			"clicked",
			Handler,
			User_Data => D);

		Button.Show;

		-- Add Button to Overlay
		Overlay.Add (Button);
		
		if Should_Watermark (D) then
			-- First Overlay
			-- Add Watermark to Overlay
			if Length (D.Watermark_Path) > 0 then
				declare
					Watermark : Gtk_Image;
				begin
					Gtk_New (Watermark);
					if Global_Pixbuf_Cache.Contains (D.Watermark_Path) then
						Watermark.Set (Global_Pixbuf_Cache.Element (D.Watermark_Path));
					else -- Asynchronously download the watermark
						T.Download (D.Watermark_Path, Gtk_Widget (Watermark));
					end if;

					Watermark.Show;
					Overlay.Add_Overlay (Watermark);
					Overlay.Set_Overlay_Pass_Through (Watermark, True);
				end;
			end if;
		end if;

		-- Intermediate Overlay
		-- Add Ornament Icon to Overlay
		if D.Style_Overridden then
			declare
				Ornament_Overlay_GI : Gtk_Image;
			begin
				Gtk_New (Ornament_Overlay_GI);
				Set (Ornament_Overlay_GI, Ornament_Overlay);
				Ornament_Overlay_GI.Show;
				Overlay.Add_Overlay (Ornament_Overlay_GI);
				Overlay.Set_Overlay_Pass_Through (Ornament_Overlay_GI, True);
			end;
		end if;

		if Should_State_Overlay (D) then
			-- Final Overlay
			-- Add Masterwork / Crafted / Normal Overlay
			Gtk_New (State_Overlay);
			Set (State_Overlay, (
				if D.State.Masterwork and D.State.Crafted then
					Crafted_Masterwork_Overlay
				elsif D.State.Masterwork then
					Masterwork_Overlay
				elsif D.State.Crafted then
					Crafted_Overlay
				else Normal_Overlay));

			State_Overlay.Show;
			Overlay.Add_Overlay (State_Overlay);
			Overlay.Set_Overlay_Pass_Through (State_Overlay, True);
		end if;

		-- Setup Quantity Label if Needed
		if D.Quantity > 1 then
			declare
				Label : Gtk_Label;
				Alignment : Gtk_Alignment;
				Attrs : Pango_Attr_List;
			begin
				Gdk_New (Attrs);
				Gtk_New (Label);
				Gtk_New (Alignment,
					Xalign => 0.95,
					Yalign => 0.92,
					Xscale => 0.0,
					Yscale => 0.0);

				Attrs.Change (Attr_Background_New (65535, 65535, 65535));
				Attrs.Change (Attr_Foreground_New (0, 0, 0));
				Label.Set_Attributes (Attrs);
				Label.Set_Label (D.Quantity'Image (D.Quantity'Image'First + 1 .. D.Quantity'Image'Last));
				Label.Show;

				Alignment.Add (Label);
				Alignment.Show;

				Overlay.Add_Overlay (Alignment);
				Overlay.Set_Overlay_Pass_Through (Alignment, True);
			end;
		end if;

		Overlay.Set_Tooltip_Text (
			(+D.Name) & ASCII.LF
			& (+D.Item_Type_And_Tier_Display_Name));
		return Overlay;
	end Get_Overlay;

	procedure Render_Items (
		List : Item_Description_List;
		Bucket : Gtk_Grid;
		T : Tasks.Download.Download_Task;
		Max_Left : Gint := 2)
	is
		Left : Gint := 0;
		Top : Gint := 0;

		-- Local Copies
		Search : constant String := +Search_Query;
	begin
		for D of List loop
			-- Filter Items based upon Search Query
			if Search'Length /= 0 and then Index (D.Name, Search) = 0
			then
				goto Skip_Item;
			end if;

			declare
				Overlay : constant Gtk_Overlay := Get_Overlay (
					D,
					T,
					User_Callback_Item_Description.To_Marshaller (
						Handlers.Item_Button_Handler'Access));
			begin
				-- Display Overlay and Attach
				Overlay.Show;
				Bucket.Attach (Overlay, Left, Top);

				Left := @ + 1;

				if Left > Max_Left then
					Left := 0;
					Top := @ + 1;
				end if;
			end;

			<<Skip_Item>>
		end loop;

		Show (Bucket);

		-- TODO: Render light level for weapons / armour instead of quantity?
	end Render_Items;

	task Status_Task is
		entry Start;
		entry Update (Text : String);
		entry Stop;
	end Status_Task;

	task body Status_Task is
		Window : Gtk_Window;
		Status_Window : Gtk_Window;
		Status_Name : Gtk_Label;

		Discard : Boolean;
	begin
		loop
			select
				accept Start;

				Window := Gtk_Window (Builder.Get_Object ("root"));
				Status_Window := Gtk_Window (Builder.Get_Object ("status_window"));
				Status_Name := Gtk_Label (Builder.Get_Object ("status_name"));

				accept Update (Text : String) do
					Status_Name.Set_Label (Text);
				end Update;

				Window.Hide;
				Status_Window.Show;
				
				Main_Loop : loop
					select
						accept Stop do
							Status_Window.Hide;
							Window.Show;
						end Stop;

						exit Main_Loop;
					else
						select
							accept Update (Text : String) do
								Status_Name.Set_Label (Text);
							end Update;
						else
							Discard := Gtk.Main.Main_Iteration_Do (Blocking => False);
						end select;
					end select;
				end loop Main_Loop;
			or
				terminate;
			end select;
		end loop;
	end Status_Task;

	-- Public Subprograms
	procedure Reload_Profile_Data is
	begin
		GUI.Lock_Object.Lock;
		Status_Task.Start;

		Put_Debug ("Reloading profile data");

		Status_Task.Update ("Loading profile...");
		Profile := Profiles.Get_Profile (Membership);

		Status_Task.Update ("Loading vault...");
		GUI.Global.Update_Inventory;

		Status_Task.Update ("Loading character inventories...");
		GUI.Character.Update_Characters (GUI.Profile);

		GUI.Character.Update_For_Character (GUI.Profile.Characters (0));
		Status_Task.Stop;
		GUI.Lock_Object.Unlock;
	end Reload_Profile_Data;

	procedure Reload_Data is
	begin
		GUI.Lock_Object.Lock;
		Status_Task.Start;
		Put_Debug ("Reloading all data");

		Status_Task.Update ("Authorising...");
		Auth_Data := Authorise.Do_Authorise;
		Headers := Create_Headers (Auth_Data);

		Status_Task.Update ("Loading memberships...");
		Membership := Memberships.Get_Memberships;

		Status_Task.Update ("Loading manifest... (this can take a bit)");
		The_Manifest := Manifest.Get_Manifest;

		Status_Task.Stop;

		GUI.Lock_Object.Unlock;
		Reload_Profile_Data;
	end Reload_Data;
end GUI.Base;
