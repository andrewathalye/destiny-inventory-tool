private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Gtkada
with Gtkada.Builder; use Gtkada.Builder;
private with Gtk.Grid;
private with Gtk.Box;
private with Gtk.Overlay;
private with Gtk.Handlers;
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
--	Auth_Data : constant Auth_Storage_Type := Authorise.Do_Authorise;
	Auth_Data : Auth_Storage_Type;
	Headers : constant Auth_Header_Type := Create_Headers (Auth_Data);

	Membership : constant Memberships.Membership_Type := Memberships.Get_Memberships (Headers);
	Profile : Profiles.Profile_Type := Profiles.Get_Profile (Headers, Membership);
	The_Manifest : constant Manifest.Manifest_Type := Manifest.Get_Manifest (Membership);

	Builder : Gtkada_Builder; -- Left unitialised

	-- Subprograms
	procedure Window_Close_Handler (Builder : access Gtkada_Builder_Record'Class);
	procedure Image_Callback (
		File_Name : Unbounded_String;
		Widget : Gtk_Widget;
		Data : Stream_Element_Array);
private
	use Gtk.Grid;
	use Gtk.Box;
	use Gtk.Overlay;
	use Gtk.Handlers;

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

	package User_Callback_Item_Description is new User_Callback (Gtk_Widget_Record, Manifest.Tools.Item_Description);
	use User_Callback_Item_Description;

	package User_Return_Callback_Item_Description is new User_Return_Callback (Gtk_Widget_Record, GBoolean, Manifest.Tools.Item_Description);
	use User_Return_Callback_Item_Description;

	-- Global State
	Global_Pixbuf_Cache : Pixbuf_Hash_Map;
	Search_Query : Unbounded_String;

	-- Types for use in GUI. child packages
	pragma Warnings (Off, "is not referenced");
	function "=" (L, R : Manifest.Tools.Item_Description) return Boolean is (False);
	pragma Warnings (On, "is not referenced");

	package IDV is new Ada.Containers.Vectors (Natural, Manifest.Tools.Item_Description);
	subtype Item_Description_List is IDV.Vector;
	Bucket_Items : Item_Description_List;

	type Bucket_Location is (
		Unknown,
		Chest,
		Leg,
		Postmaster,
		Ship,
		Power,
		Emote_Collection,
		Kinetic,
		Artefact,
		Class,
		Sparrow,
		Special_Emote,
		Energy,
		Subclass,
		Helmet,
		Gauntlets,
		Finisher,
		Shell,
		Emblem,
		Clan_Banner);

	for Bucket_Location use (
		Unknown => 0,
		Chest => 14239492,
		Leg => 20886954,
		Postmaster => 215593132,
		Ship => 284967655,
		Power => 953998645,
		Emote_Collection => 1107761855,
		Kinetic => 1498876634,
		Artefact => 1506418338,
		Class => 1585787867,
		Sparrow => 2025709351,
		Special_Emote => 2401704334,
		Energy => 2465295065,
		Subclass => 3284755031,
		Helmet => 3448274439,
		Gauntlets => 3551918588,
		Finisher => 3683254069,
		Shell => 4023194814,
		Emblem => 4274335291,
		Clan_Banner => 4292445962);

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
