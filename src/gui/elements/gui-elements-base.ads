--  Gtk
with Gtk.Window; use Gtk.Window;
with Gtk.Label;  use Gtk.Label;

with Gtk.Message_Dialog; use Gtk.Message_Dialog;

package GUI.Elements.Base is
   --  Status and Root Window
   Status_Window : constant Gtk_Window :=
     Gtk_Window (Builder.Get_Object ("status_window"));
   Status_Name : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("status_name"));
   Window : constant Gtk_Window := Gtk_Window (Builder.Get_Object ("root"));

   --  Error Box
   Error_Dialog : constant Gtk_Message_Dialog :=
     Gtk_Message_Dialog (Builder.Get_Object ("error_dialog"));

end GUI.Elements.Base;
