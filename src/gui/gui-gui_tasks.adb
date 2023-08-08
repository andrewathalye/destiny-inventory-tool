with Ada.Calendar;   use Ada.Calendar;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with GNAT.OS_Lib; use GNAT.OS_Lib;

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
with Shared.Debug;

package body GUI.GUI_Tasks is
   --  Tasks
   task body GUI_Task is
      Discard_G : Guint;
      Discard_B : Boolean;
      Provider  : Gtk_Css_Provider;
      Error     : aliased GError;
   begin
      accept Start;
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

      Main_Loop :
         loop
            select
               accept Pause;
               accept Resume;
            else
               Discard_B := Gtk.Main.Main_Iteration;
            end select;

            --  Reload profile data if it is more than 120 seconds old
            --  No need to reload data if we’re caching it anyway
            if not API.Debug.Caching
              and then (Clock - GUI.Profile.Response_Minted_Timestamp) > 120.0
            then
               Shared.Debug.Put_Line
                 ("Profile data is more than 120 seconds old, refreshing");
               GUI.Base.Reload_Profile_Data;
            end if;
         end loop Main_Loop;
   exception
      when X : others =>
         New_Line (Standard_Error);
         Put_Line (Standard_Error, Exception_Information (X));
         OS_Exit (-1);
   end GUI_Task;

end GUI.GUI_Tasks;
