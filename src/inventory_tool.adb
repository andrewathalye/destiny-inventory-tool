pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

--  GtkAda
with Gtkada.Builder; use Gtkada.Builder;

with Gtk.Main;
with Gtk.Css_Provider;   use Gtk.Css_Provider;
with Gtk.Style_Context;  use Gtk.Style_Context;
with Gtk.Style_Provider; use Gtk.Style_Provider;

with Glib.Error; use Glib.Error;
use Glib;

--  Local Packages
with GUI.Base;
with GUI.Handlers;

--  Alire
with Destiny_Inventory_Tool_Config;

procedure Inventory_Tool is
   Discard_G : Guint;
   Discard_B : Boolean;
   Error     : aliased GError;
   Provider  : Gtk_Css_Provider;
   Context   : Gtk_Style_Context;
begin
   --  Print Welcome Message
   Put_Line
     ("Destiny Inventory Tool v" &
      Destiny_Inventory_Tool_Config.Crate_Version);

   --  Load Interface
   Gtk.Main.Init;
   Gtk_New (GUI.Builder);
   Discard_G := GUI.Builder.Add_From_File ("res/gui.glade", Error'Access);

   --  Load CSS
   Gtk_New (Provider);
   Discard_B := Provider.Load_From_Path ("res/style.css", Error'Access);
   Gtk_New (Context);
   Context.Add_Provider (+Provider, Priority_Application);

   GUI.Handlers.Set_Handlers;
   Do_Connect (GUI.Builder);

   --  Update GUI data
   --  This also creates all necessary windows
   GUI.Base.Reload_Data;

   --  Maintain an exclusive lock over the GUI while executing the main loop.
   --
   --  Download_Tasks would otherwise attempt to edit widgets while they are
   --  being drawn
   loop
      GUI.Locking_Main_Iteration;
   end loop;
end Inventory_Tool;
