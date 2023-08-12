--  Gtk
with Gtk.Window; use Gtk.Window;
with Gtk.GEntry; use Gtk.GEntry;

package GUI.Elements.Authorise is
   Auth_Window : constant Gtk_Window :=
     Gtk_Window (Builder.Get_Object ("auth_window"));
   Auth_URL : constant Gtk_Entry :=
     Gtk_Entry (Builder.Get_Object ("auth_url"));
end GUI.Elements.Authorise;
