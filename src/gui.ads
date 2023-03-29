private with Ada.Containers.Vectors;
private with Ada.Streams;
private with Ada.Strings.Unbounded;

-- Gtkada
with Gtkada.Builder; use Gtkada.Builder;
private with Gtk.Grid;

private with Gdk.Pixbuf;
private with Glib;

-- Local Packages
with API.Authorise;
with API.Profiles;
with API.Manifest;
private with API.Manifest.Tools;
with API.Memberships;
use API;

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
private
	use Ada.Streams;
	use Gdk.Pixbuf;
	use Gtk.Grid;
	use Glib;
	use Ada.Strings.Unbounded;

	-- Shared State
	Search_Query : Unbounded_String;

	-- Types for use in GUI. child packages
	pragma Warnings (Off, "is not referenced");
	function "=" (L, R : Manifest.Tools.Item_Description) return Boolean is (False);
	pragma Warnings (On, "is not referenced");

	package IDV is new Ada.Containers.Vectors (Natural, Manifest.Tools.Item_Description);
	subtype Item_Description_List is IDV.Vector;
	Bucket_Items : Item_Description_List;

	type Bucket_Location is (
		Kinetic,
		Energy,
		Power,
		Helmet,
		Gauntlets,
		Chest,
		Leg,
		Class,
		Unknown); -- TODO Add a lot more :)

	for Bucket_Location use (
		Kinetic => 20,
		Energy => 30,
		Power => 40,
		Helmet => 50,
		Gauntlets => 60,
		Chest => 70,
		Leg => 80,
		Class => 90,
		Unknown => 9999);

	-- Private-Exported Subprograms
	function Load_Image (File_Name : String; Data : Stream_Element_Array) return Gdk_Pixbuf;
	procedure Clear_Bucket (G : Gtk_Grid);
	procedure Render_Items (
		List : Item_Description_List;
		Bucket : Gtk_Grid;
		Max_Left : Gint := 2;
		Max_Top : Gint := 2);
end GUI;
