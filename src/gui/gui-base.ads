private with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  Gtk
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Grid;     use Gtk.Grid;
with Gtk.Box;      use Gtk.Box;
with Gtk.Handlers; use Gtk.Handlers;
with Glib;         use Glib;

private with Gdk.Pixbuf;

--  Local Packages
with API.Manifest.Tools; use API.Manifest.Tools;
with API.Inventories;    use API;
with Tasks.Download;

private with Shared.Files;

package GUI.Base is
   --  Instantiations
   package User_Callback_Item_Description is new User_Callback
     (Gtk_Widget_Record, Manifest.Tools.Item_Description);

   --  State
   Search_Query : Unbounded_String;

   --  Subprograms
   --  Bucket Management
   procedure Clear_Bucket (G : Gtk_Grid);
   procedure Clear_Bucket (B : Gtk_Box);

   --  Rendering Inventory Items
   --  Renders a set of inventory items into a Gtk_Grid from an
   --  Item_Description_List. Each item will have an on-click handler added.
   procedure Render_Items
     (List     : Inventories.Item_Description_List;
      Bucket   : Gtk_Grid;
      T        : Tasks.Download.Download_Task;
      Max_Left : Gint := 2);

   --  Displays an error message on screen
   procedure Error_Message (Name : String; Message : String);

   --  Reloads all data used by the GUI _excluding_ the Manifest and auth data
   --  If these need to be reloaded too, use Reload_Data instead
   procedure Reload_Profile_Data;

   --  Reloads all data used by the GUI
   procedure Reload_Data;

   --  Note: To be used with Tasks.Download.Download_Task
   --  This sends data back to the GUI thread so we can apply it to images etc.
   procedure Image_Callback
     (Cache : in out Tasks.Download.Download_Cache_Type);

   --  Note: This is the same as above _but_ does not pause the GUI task.
   --  This means it should only be used before the main loop begins or during
   --  a GTK event handler (when the GUI task cannot rendezvous with the Download Tasks)
   procedure Event_Image_Callback
     (Cache : in out Tasks.Download.Download_Cache_Type);
private
   use Gdk.Pixbuf;
   use Shared;

   --  Cached High-Frequency Pixbufs
   Placeholder_Icon : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/placeholder_icon.png"));
   Crafted_Masterwork_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/crafted_masterwork_overlay.png"));
   Crafted_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/crafted_overlay.png"));
   Masterwork_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/masterwork_overlay.png"));
   Normal_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/normal_overlay.png"));
   Ornament_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/ornament_overlay.png"));

   --  Constants for separate subprograms
   Unix_Epoch : constant Ada.Calendar.Time :=
     Ada.Calendar.Formatting.Value ("1970-01-01 00:00:00");
end GUI.Base;
