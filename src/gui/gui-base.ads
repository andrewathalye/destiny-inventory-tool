--  Gtk
with Gtk.Grid; use Gtk.Grid;
with Gtk.Box;  use Gtk.Box;

--  Local Packages
with GUI.GUI_Tasks; use GUI.GUI_Tasks;

package GUI.Base is
   --  Subprograms
   --  Bucket Management
   procedure Clear_Bucket (G : Gtk_Grid);
   procedure Clear_Bucket (B : Gtk_Box);

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
     (Cache : in out GUI_Download_Task.Download_Cache_Type);

   --  Note: This is the same as above _but_ does not pause the GUI task.
   --  This means it should only be used before the main loop begins or during
   --  a GTK event handler (when the GUI task cannot rendezvous with the Download Tasks)
   procedure Event_Image_Callback
     (Cache : in out GUI_Download_Task.Download_Cache_Type);
end GUI.Base;
