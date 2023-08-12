with Ada.Calendar; use Ada.Calendar;

--  Gtk
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
with GUI.Elements;

with Shared.Debug;
with Shared.Config;

package body GUI.GUI_Tasks is
   --  Tasks
   task body GUI_Task is
      Discard_B : Boolean;
      Provider  : Gtk_Css_Provider;
      Error     : aliased GError;
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      --  Gtk Init
      --  Elaboration of GUI.Elements already initialised Gtk, but we
      --  are on a different thread.
      Gtk.Main.Init;

      --  Load Interface
      --  Builder initialised during elaboration of GUI.Elements
      GUI.Handlers.Set_Handlers (GUI.Elements.Builder);
      GUI.Elements.Builder.Do_Connect;

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
            if not Shared.Config.Debug_API
              and then (Clock - GUI.Profile.Response_Minted_Timestamp) > 120.0
            then
               Shared.Debug.Put_Line
                 ("Profile data is more than 120 seconds old, refreshing");
               GUI.Base.Reload_Profile_Data;
            end if;
         end loop Main_Loop;
   end GUI_Task;

end GUI.GUI_Tasks;
