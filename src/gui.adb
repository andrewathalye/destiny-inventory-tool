pragma Ada_2022;

with Ada.Streams; use Ada.Streams;
with System;

-- Gtkada
with Gtk.Main;
with Gtk.Image; use Gtk.Image;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Button; use Gtk.Button;
with Gtk.Widget;
with Gtk.Container; use Gtk.Container;

with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib.Error; use Glib.Error;
use Glib;

-- AWS
with AWS.Client;
with AWS.Response;
use AWS;

-- Local Packages
with API.Manifest.Tools;
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

	procedure Remove_Callback (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class; Grid : Gtk_Grid)
	is begin
		Grid.Remove (Widget);
	end Remove_Callback;

	-- Private-Exported
	procedure Clear_Bucket (G : Gtk_Grid)
	is 
		package FUD_Grid is new Gtk.Container.Foreach_User_Data (Gtk_Grid);
	begin
		FUD_Grid.Foreach (G, Remove_Callback'Access, G);
	end Clear_Bucket;

	-- Private-Exported
	procedure Render_Items (
		List : Item_Description_List;
		Bucket : Gtk_Grid;
		Max_Left : Gint := 2;
		Max_Top : Gint := 2)
	is
		Left : Gint := 0;
		Top : Gint := 0;
	begin
		for D of List loop
			declare
				Button : Gtk_Button;
				Image : Gtk_Image;
			begin
				Gtk_New (Button);
				Gtk_New (Image);
				
				if Has_Cached (+D.Icon_Path) then
--					Put_Debug ("Load cached icon");
					Set (Image, Load_Image (
						+D.Icon_Path,
						Get_Cached (+D.Icon_Path)));
				else
					declare
						Data : Response.Data;
					begin
						Put_Debug ("Get icon");
						Data := Client.Get (Bungie_Root & (+D.Icon_Path));
						Cache (+D.Icon_Path, Response.Message_Body (Data));
						Set (Image, Load_Image (
							+D.Icon_Path,
							Response.Message_Body (Data)));
					end;
				end if;

				Set_Image (Button, Image);

				Show (Button);
				Bucket.Attach (Button, Left, Top);

				Left := @ + 1;

				if Left > Max_Left then
					Left := 0;
					Top := @ + 1;
				end if;
			end;
		end loop;

		Show (Bucket);

		-- TODO: Render masterwork status, locked status, ornament status as overlays
	end Render_Items;

	-- Public Subprograms
	procedure Window_Close_Handler (Builder : access Gtkada_Builder_Record'Class) is
	begin
		GTK.Main.Main_Quit;
	end Window_Close_Handler;
end GUI;
