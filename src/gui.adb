pragma Ada_2022;

with System;

-- Gtkada
with Gtk.Image; use Gtk.Image;

with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib.Error; use Glib.Error;
use Glib;

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

	-- Called by the Tick subprograms after data is downloaded
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
end GUI;
