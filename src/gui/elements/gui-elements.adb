--  Gtk
with Gtk.Main;

with Glib;       use Glib;
with Glib.Error; use Glib.Error;

--  Local Packages
with Shared.Config;

package body GUI.Elements is
   Error     : aliased GError;
   Discard_G : Guint;

   Real_Builder : Gtkada_Builder;

   function Builder return Gtkada_Builder is (Real_Builder);
begin
   Gtk.Main.Init;

   Gtk_New (Real_Builder);
   Discard_G :=
     Real_Builder.Add_From_File
       (Shared.Config.Builder_File_Name, Error'Access);
end GUI.Elements;
