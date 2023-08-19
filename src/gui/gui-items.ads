with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  Gtk
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Grid;    use Gtk.Grid;
with Gtk.Overlay; use Gtk.Overlay;
with Gtk.Handlers;

with Glib; use Glib;

--  Local Packages
with GUI.GUI_Tasks; use GUI.GUI_Tasks;

with API.Manifest.Tools;
with API.Inventories;

package GUI.Items is
   --  Instantiations
   package User_Callback_Item_Description is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Manifest.Tools.Item_Description);

   --  State
   Search_Query : Unbounded_String;

   --  Rendering Inventory Items
   --  Renders a set of inventory items into a Gtk_Grid from an
   --  Item_Description_List. Each item will have an on-click handler added.
   procedure Render_Items
     (List     : API.Inventories.Item_Description_List;
      Bucket   : Gtk_Grid;
      T        : GUI_Download_Task.Download_Task;
      Max_Left : Gint := 2);

   --  Returns an Overlay for an item description, using the specified
   --  download queue and on-click handler
   function Get_Overlay
     (D       : API.Manifest.Tools.Item_Description;
      T       : GUI_Download_Task.Download_Task;
      Handler : User_Callback_Item_Description.Marshallers.Marshaller)
      return Gtk_Overlay;

   --  Populates the item details menu for a given item description.
   --  The menu must then be shown by the caller when needed.
   procedure Populate_Item_Details (D : API.Manifest.Tools.Item_Description);
end GUI.Items;
