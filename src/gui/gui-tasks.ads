--  Gtk
with Gtk.Widget; use Gtk.Widget;

--  Glib
with Glib.Object; use Glib.Object;

--  Local Packages
with API.Tasks.Download;

package GUI.Tasks is
   task GUI_Task is
      entry Start;
      entry Pause;
      entry Resume;
   end GUI_Task;

   --  Predefined Tasks
   procedure Reference (Widget : Gtk_Widget);
   package Download_Tasks is new API.Tasks.Download
     (Opaque_Type   => Gtk_Widget_Record,
      Opaque_Access => Gtk_Widget,
      Reference     => Reference);
   use Download_Tasks;

   Contents_Task  : Download_Task;
   Character_Task : Download_Task;
   Global_Task    : Download_Task;
end GUI.Tasks;
