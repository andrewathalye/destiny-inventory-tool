with Gtk.Main;

with Glib;       use Glib;
with Glib.Error; use Glib.Error;

function GUI.Make_Builder (Name : String) return Gtkada_Builder is
   Builder : Gtkada_Builder;

   Error     : aliased GError;
   Discard_G : Guint;
begin
   Gtk.Main.Init;

   Gtk_New (Builder);
   Discard_G := Builder.Add_From_File (Name, Error'Access);

   return Builder;
end GUI.Make_Builder;
