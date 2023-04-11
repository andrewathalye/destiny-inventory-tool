private with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
private with Ada.Streams;

-- Gtkada
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;

private with Gdk.Pixbuf;

-- Local Packages
with API.Profiles;
with API.Manifest;
private with API.Manifest.Tools;
with API.Memberships;
use API;
private with Shared;

package GUI is
	-- Variables
	Builder : Gtkada_Builder; -- Left uninitialised, should be setup along with Gtk_Main

	-- Before use, execute GUI.Base.Reload_Data
	-- Undefined behaviour will occur otherwise
	Auth_Data : Auth_Storage_Type;
	Headers : Auth_Header_Type;

	Membership : Memberships.Membership_Type;
	Profile : Profiles.Profile_Type;
	The_Manifest : Manifest.Manifest_Type;
private
	use Ada.Streams;

	use Gdk.Pixbuf;

	use Shared;
	use API.Manifest.Tools;

	-- Subprograms (Other Utilities in GUI.Base)
	-- Image Loading
	function Load_Image (File_Name : String; Data : Stream_Element_Array) return Gdk_Pixbuf;

	-- Note: To be used with Tasks.Download.Download_Task
	procedure Image_Callback (
		File_Name : Unbounded_String;
		Widget : Gtk_Widget;
		Data : Stream_Element_Array);

	-- Instantiations
	package Pixbuf_Hash_Maps is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => Gdk_Pixbuf,
		Hash => Hash,
		Equivalent_Keys => Equivalent_Key);
	subtype Pixbuf_Hash_Map is Pixbuf_Hash_Maps.Map;

	-- Global State
	Global_Pixbuf_Cache : Pixbuf_Hash_Map;
	Current_Item : Manifest.Tools.Item_Description;
end GUI;
