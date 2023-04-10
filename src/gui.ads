private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Gtkada
with Gtkada.Builder; use Gtkada.Builder;
private with Gtk.Grid;
private with Gtk.Box;
private with Gtk.Overlay;
with Gtk.Widget; use Gtk.Widget;

private with Gdk.Pixbuf;
private with Glib;

-- Local Packages
with API.Authorise;
with API.Profiles;
with API.Manifest;
private with API.Manifest.Tools;
with API.Memberships;
use API;
private with Shared;
private with Tasks.Download;

package GUI is
	-- Variables
	Auth_Data : constant Auth_Storage_Type := Authorise.Do_Authorise;
	Headers : constant Auth_Header_Type := Create_Headers (Auth_Data);

	Membership : constant Memberships.Membership_Type := Memberships.Get_Memberships (Headers);
	Profile : Profiles.Profile_Type := Profiles.Get_Profile (Headers, Membership);
	The_Manifest : constant Manifest.Manifest_Type := Manifest.Get_Manifest (Membership);

	Builder : Gtkada_Builder; -- Left unitialised

	-- Subprograms
	procedure Image_Callback (
		File_Name : Unbounded_String;
		Widget : Gtk_Widget;
		Data : Stream_Element_Array);
private
	use Gtk.Grid;
	use Gtk.Box;
	use Gtk.Overlay;

	use Gdk.Pixbuf;
	use Glib;

	use Shared;

	-- Instantiations
	package Pixbuf_Hash_Maps is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => Gdk_Pixbuf,
		Hash => Hash,
		Equivalent_Keys => Equivalent_Key);
	subtype Pixbuf_Hash_Map is Pixbuf_Hash_Maps.Map;

	-- Types for use in GUI. child packages
	pragma Warnings (Off, "is not referenced");
	function "=" (L, R : Manifest.Tools.Item_Description) return Boolean is (False);
	pragma Warnings (On, "is not referenced");

	package IDV is new Ada.Containers.Vectors (Natural, Manifest.Tools.Item_Description);
	subtype Item_Description_List is IDV.Vector;

	-- Global State
	Global_Pixbuf_Cache : Pixbuf_Hash_Map;
	Current_Item : Manifest.Tools.Item_Description;
	Search_Query : Unbounded_String;

	Vault_Inventory : array (API.Manifest.Tools.Bucket_Location_Type) of Item_Description_List;
	Character_Items : array (API.Manifest.Tools.Bucket_Location_Type) of Item_Description_List;
	Equipped_Items : array (API.Manifest.Tools.Bucket_Location_Type) of Manifest.Tools.Item_Description;

	-- Private-Exported Subprograms
	function Load_Image (File_Name : String; Data : Stream_Element_Array) return Gdk_Pixbuf;
	procedure Clear_Bucket (G : Gtk_Grid);
	procedure Clear_Bucket (B : Gtk_Box);
	function Get_Overlay (
		D : Manifest.Tools.Item_Description;
		T : Tasks.Download.Download_Task) return Gtk_Overlay;
	procedure Render_Items (
		List : Item_Description_List;
		Bucket : Gtk_Grid;
		T : Tasks.Download.Download_Task;
		Max_Left : Gint := 2);
end GUI;
