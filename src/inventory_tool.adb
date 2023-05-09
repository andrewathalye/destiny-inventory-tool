pragma Ada_2022;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

--  GtkAda
with Gtkada.Builder; use Gtkada.Builder;

with Gtk.Main;
with Gtk.Css_Provider;   use Gtk.Css_Provider;
with Gtk.Style_Context;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gdk.Screen;         use Gdk.Screen;

with Glib.Error; use Glib.Error;
use Glib;

--  Local Packages
with GUI.Base;
with GUI.Handlers;

with API.Debug;

with Shared.Debug; use Shared;

--  Alire
with Destiny_Inventory_Tool_Config;

procedure Inventory_Tool is
   Discard_G : Guint;
   Discard_B : Boolean;
   Provider  : Gtk_Css_Provider;
   Error     : aliased GError;
begin
   --  Print Welcome Message
   Put_Line
     ("Destiny Inventory Tool v" &
      Destiny_Inventory_Tool_Config.Crate_Version);

   --  Gtk Init
   Gtk.Main.Init;

   --  Load Interface
   Gtk_New (GUI.Builder);
   Discard_G := GUI.Builder.Add_From_File ("res/gui.glade", Error'Access);
   GUI.Handlers.Set_Handlers;
   Do_Connect (GUI.Builder);

   --  Load CSS
   Gtk_New (Provider);
   Discard_B := Provider.Load_From_Path ("res/style.css", Error'Access);
   Gtk.Style_Context.Add_Provider_For_Screen
     (Gdk.Screen.Get_Default, +Provider, Priority_Application);

   --  Update GUI data
   --  This also creates all necessary windows
   GUI.Base.Reload_Data;

   --  Maintain an exclusive lock over the GUI while executing the main loop.
   --
   --  Download_Tasks would otherwise attempt to edit widgets while they are
   --  being drawn
   loop
      GUI.Locking_Main_Iteration;

      --  Reload profile data if it is more than 120 seconds old
      if not API.Debug.Caching
        and then (Clock - GUI.Profile.Response_Minted_Timestamp) > 120.0
      then
         Debug.Put_Line
           ("Profile data is more than 120 seconds old, refreshing");
         GUI.Base.Reload_Profile_Data;
      end if;
   end loop;
end Inventory_Tool;
