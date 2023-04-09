pragma Ada_2022;

with Ada.Streams; use Ada.Streams;
with Ada.Containers.Hashed_Maps;
with System;
with Interfaces; use Interfaces;

with GNAT.OS_Lib; use GNAT.OS_Lib;

-- Gtkada
with Gtk.Image; use Gtk.Image;
with Gtk.Button; use Gtk.Button;
with Gtk.Label; use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Container; use Gtk.Container;
with Gtk.Overlay; use Gtk.Overlay;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Popover; use Gtk.Popover;

with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib.Error; use Glib.Error;
use Glib;

with Pango.Attributes; use Pango.Attributes;

-- Local Packages
with Shared; use Shared;

package body GUI is
	-- Private Subprograms
	-- Private-Exported
	-- Exclusively for JPEG / PNG format images
	function Load_Image (File_Name : String; Data : Stream_Element_Array) return Gdk_Pixbuf
	is
		-- Imported Subprograms
		type Pixbuf_Loader is null record;

		function Gdk_Pixbuf_Loader_New_With_Type (
			Image_Type : String;
			Error : access GError) return access Pixbuf_Loader
		with
			Import => True,
			Convention => C;

		function Gdk_Pixbuf_Loader_Write (
			Loader : access Pixbuf_Loader;
			Buffer : System.Address;
			Count : GSize;
			Error : access GError) return GBoolean
		with
			Import => True,
			Convention => C;

		function Gdk_Pixbuf_Loader_Get_Pixbuf (
			Loader : access Pixbuf_Loader) return System.Address
		with
			Import => True,
			Convention => C;

		function Gdk_Pixbuf_Loader_Close (
			Loader : access Pixbuf_Loader;
			Error : access GError) return GBoolean
		with
			Import => True,
			Convention => C;

		-- Constants
		FType : constant String := (
			if File_Name (File_Name'Last -2 .. File_Name'Last) = "png" then
				"png"
			else
				"jpeg");

		-- Variables
		Loader : access Pixbuf_Loader;
		Pixbuf : System.Address;
		Discard : GBoolean;
	begin
		Loader := Gdk_Pixbuf_Loader_New_With_Type (FType & ASCII.NUL, null);

		Discard := Gdk_Pixbuf_Loader_Write (Loader, Data (Data'First)'Address, Data'Length, null);

		Pixbuf := Gdk_Pixbuf_Loader_Get_Pixbuf (Loader);

		if Gdk_Pixbuf'(Convert (Pixbuf)) = null then
			raise Program_Error with "Image could not be processed";
		end if;

		Discard := Gdk_Pixbuf_Loader_Close (Loader, null);
		
		return Convert (Pixbuf);
	end Load_Image;

	-- Called by Inventory_Tool after data has been downloaded
	procedure Image_Callback (
		File_Name : Unbounded_String;
		Widget : Gtk_Widget;
		Data : Stream_Element_Array)
	is
		Temp : constant Gdk_Pixbuf := Load_Image (+File_Name, Data);
	begin
		if Global_Pixbuf_Cache.Contains (File_Name) then
			Gtk_Image (Widget).Set (Temp);
			return;
		end if;

		-- Cache Pixbuf
		Global_Pixbuf_Cache.Insert (File_Name, Temp);

		Gtk_Image (Widget).Set (Temp);
	end Image_Callback;

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

	package FUD_Container is new Gtk.Container.Foreach_User_Data (Gtk_Container);
	procedure Remove_Callback (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class; Container : Gtk_Container)
	is begin
		Container.Remove (Widget);
	end Remove_Callback;

	-- Private-Exported
	procedure Clear_Bucket (G : Gtk_Grid)
	is begin
		FUD_Container.Foreach (Gtk_Container (G), Remove_Callback'Access, Gtk_Container (G));
	end Clear_Bucket;

	procedure Clear_Bucket (B : Gtk_Box)
	is begin
		FUD_Container.Foreach (Gtk_Container (B), Remove_Callback'Access, Gtk_Container (B));
	end Clear_Bucket;

	procedure Item_Button_Clicked_Handler (Widget : access Gtk_Widget_Record'Class; User_Data : Manifest.Tools.Item_Description)
	is
		Transfer_Name : constant Gtk_Label := Gtk_Label (Builder.Get_Object ("transfer_name"));
		Transfer_Type : constant Gtk_Label := Gtk_Label (Builder.Get_Object ("transfer_type"));
		Transfer_Menu : constant Gtk_Popover := Gtk_Popover (Builder.Get_Object ("transfer_menu"));
	begin
		Transfer_Name.Set_Label (+User_Data.Name);
		Transfer_Type.Set_Label (+User_Data.Item_Type_And_Tier_Display_Name);

		Transfer_Menu.Set_Relative_To (Widget);
		Transfer_Menu.Popup;
	end Item_Button_Clicked_Handler;

	function Get_Overlay (
		D : Manifest.Tools.Item_Description;
		T : Tasks.Download.Download_Task) return Gtk_Overlay
	is
		Overlay : Gtk_Overlay;
		Button : Gtk_Button;
		Image : Gtk_Image;
		State_Overlay : Gtk_Image;
	begin
		Gtk_New (Overlay);
		Gtk_New (Button);
		Gtk_New (Image);

		-- Setup Icon and Button
		if Global_Pixbuf_Cache.Contains (D.Icon_Path) then
			Image.Set (Global_Pixbuf_Cache.Element (D.Icon_Path));
--					Put_Debug ("Load cached icon");
		else -- Asynchronously download the icon
--			Put_Debug ("Get icon");
			Image.Set (Placeholder_Icon);
			T.Download (D.Icon_Path, Gtk_Widget (Image));
		end if;

		Set_Image (Button, Image);

		Connect (Button,
			"clicked",
			To_Marshaller (Item_Button_Clicked_Handler'Access),
			User_Data => D);

		Button.Show;

		-- Add Button to Overlay
		Overlay.Add (Button);
		
		-- First Overlay
		-- Add Watermark to Overlay
		if Length (D.Watermark_Path) > 0 then
			declare
				Watermark : Gtk_Image;
			begin
				Gtk_New (Watermark);
				if Global_Pixbuf_Cache.Contains (D.Watermark_Path) then
--					Put_Debug ("Load cached watermark");
					Watermark.Set (Global_Pixbuf_Cache.Element (D.Watermark_Path));
				else -- Asynchronously download the watermark
--					Put_Debug ("Get watermark");
					T.Download (D.Watermark_Path, Gtk_Widget (Watermark));
				end if;

				Watermark.Show;
				Overlay.Add_Overlay (Watermark);
				Overlay.Set_Overlay_Pass_Through (Watermark, True);
			end;
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

		return Overlay;
	end Get_Overlay;

	-- Private-Exported
	procedure Render_Items (
		List : Item_Description_List;
		Bucket : Gtk_Grid;
		T : Tasks.Download.Download_Task;
		Max_Left : Gint := 2;
		Max_Top : Gint := 2)
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
				Overlay : constant Gtk_Overlay := Get_Overlay (D, T);
			begin
				-- Display Overlay and Attach
				Overlay.Show;
				Bucket.Attach (Overlay, Left, Top);

				Left := @ + 1;

				if Left > Max_Left then
					Left := 0;
					Top := @ + 1;
				end if;

				if Top > Max_Top then
					null;
					--raise Program_Error;
				end if;
			end;

			<<Skip_Item>>
		end loop;

		Show (Bucket);

		-- TODO: Render light level for weapons / armour instead of quantity?
	end Render_Items;

	-- Public Subprograms
	pragma Warnings (Off, "is not referenced");
	procedure Window_Close_Handler (Builder : access Gtkada_Builder_Record'Class) is
	begin
		OS_Exit (0);
	end Window_Close_Handler;
	pragma Warnings (On, "is not referenced");
end GUI;
