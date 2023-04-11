with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

-- Gtk
with Gtk.Grid; use Gtk.Grid;
with Gtk.Box; use Gtk.Box;
with Gtk.Overlay; use Gtk.Overlay;
with Gtk.Handlers; use Gtk.Handlers;

with Glib; use Glib;

-- Local Packages
with API.Manifest.Tools; use API.Manifest.Tools;
use API;

with Tasks.Download;

package GUI.Base is
	-- Types
	package IDV is new Ada.Containers.Vectors (Natural, Manifest.Tools.Item_Description);
	subtype Item_Description_List is IDV.Vector;

	-- Instantiations
	package User_Callback_Item_Description is new User_Callback (Gtk_Widget_Record, Manifest.Tools.Item_Description);

	-- State
	Search_Query : Unbounded_String;

	-- Bucket Management
	procedure Clear_Bucket (G : Gtk_Grid);
	procedure Clear_Bucket (B : Gtk_Box);

	-- Rendering Inventory Items
	
	-- Returns an overlay for a generic inventory item
	function Get_Overlay (
		D : Manifest.Tools.Item_Description;
		T : Tasks.Download.Download_Task;
		Handler : User_Callback_Item_Description.Marshallers.Marshaller) return Gtk_Overlay;
	
	-- Renders a set of inventory items into a Gtk_Grid from
	-- an Item_Description_List. Each item will have an on-click
	-- handler added.
	procedure Render_Items (
		List : Item_Description_List;
		Bucket : Gtk_Grid;
		T : Tasks.Download.Download_Task;
		Max_Left : Gint := 2);
	
	-- Note: The below functions require GUI.Lock_Object to be in the unlocked position or they
	-- will block until the lock is released
	
	-- Reloads all data used by the GUI _excluding_ the Manifest and auth data
	-- If these need to be reloaded too, use Reload_Data instead
	procedure Reload_Profile_Data;

	-- Reloads all data used by the GUI
	procedure Reload_Data;
end GUI.Base;
