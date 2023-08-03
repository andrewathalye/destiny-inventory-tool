pragma Ada_2022;

with Ada.Calendar; use Ada.Calendar;

--  GtkAda
with Gtkada.Builder; use Gtkada.Builder;

--  Gtk
with Gtk.Main;
with Gtk.Container;      use Gtk.Container;
with Gtk.Window;         use Gtk.Window;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Label;          use Gtk.Label;
with Gtk.Overlay;        use Gtk.Overlay;
with Gtk.Image;          use Gtk.Image;

with Gdk.Pixbuf; use Gdk.Pixbuf;

--  Local Packages
with GUI.Handlers;
with GUI.Character;
with GUI.Global;
with GUI.Authorise;
with GUI.Base.Get_Overlay;
with GUI.GUI_Tasks; use GUI.GUI_Tasks;

with API.Inventories.Global;
with API.Memberships;
with API.Inventories.Character;
with API.Manifest;
with API.Profiles;
use all type API.Manifest.Destiny_Item_Type;
use API;

with Shared.Debug;
with Shared.Strings; use Shared.Strings;
with Shared.Streams; use Shared.Streams;
use Shared;

with Secrets; use Secrets;

package body GUI.Base is
   --  Instantiations
   package FUD_Container is new Gtk.Container.Foreach_User_Data
     (Gtk_Container);

   --  Subprograms
   --  Bucket Management
   --  Internal
   procedure Remove_Callback
     (Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Container : Gtk_Container)
   is
   begin
      Container.Remove (Widget);
   end Remove_Callback;

   --  Exported
   procedure Clear_Bucket (G : Gtk_Grid) is
   begin
      FUD_Container.Foreach
        (Gtk_Container (G), Remove_Callback'Access, Gtk_Container (G));
   end Clear_Bucket;

   procedure Clear_Bucket (B : Gtk_Box) is
   begin
      FUD_Container.Foreach
        (Gtk_Container (B), Remove_Callback'Access, Gtk_Container (B));
   end Clear_Bucket;

   --  Inventory Item Rendering (Exported)
   procedure Render_Items
     (List     : Inventories.Item_Description_List;
      Bucket   : Gtk_Grid;
      T        : Tasks.Download.Download_Task;
      Max_Left : Gint := 2)
   is

      Left : Gint := 0;
      Top  : Gint := 0;
      --  Local Copies
      Search : constant String := +Search_Query;

   begin
      for D of List loop
         --  Filter Items based upon Search Query
         if Search'Length /= 0 and then Index (D.Name, Search) = 0 then
            goto Skip_Item;
         end if;

         declare

            Overlay : constant Gtk_Overlay :=
              Get_Overlay
                (D,
                 T,
                 User_Callback_Item_Description.To_Marshaller
                   (Handlers.Item_Button_Handler'Access));

         begin
            --  Display Overlay and Attach
            Overlay.Show;
            Bucket.Attach (Overlay, Left, Top);
            Left := @ + 1;

            if Left > Max_Left then
               Left := 0;
               Top  := @ + 1;
            end if;
         end;
         <<Skip_Item>>
      end loop;
      Show (Bucket);
   end Render_Items;

   procedure Do_Events is

      Discard : Boolean;

   begin
      while Gtk.Main.Events_Pending loop
         Discard := Gtk.Main.Main_Iteration;
      end loop;
      delay 0.1;
   end Do_Events;

   --  Public Subprograms
   --  There is likely another Gtk Main_Loop running on the main thread when
   --  this is called, so we pretend that is the case and only use synchronous
   --  GUI calls

   procedure Reload_Profile_Data is

      Window : constant Gtk_Window := Gtk_Window (Builder.Get_Object ("root"));
      Status_Window : constant Gtk_Window :=
        Gtk_Window (Builder.Get_Object ("status_window"));
      Status_Name : constant Gtk_Label :=
        Gtk_Label (Builder.Get_Object ("status_name"));

   begin
      Status_Window.Show;

      Shared.Debug.Put_Line ("Reloading profile data");
      Status_Name.Set_Label ("Loading profile...");
      Do_Events;

      Profile := Profiles.Get_Profile (Secrets.Membership);
      Status_Name.Set_Label ("Loading vault...");
      Do_Events;

      API.Inventories.Global.Update_Inventory
        (GUI.Global.Inventory, GUI.Profile, GUI.The_Manifest);
      GUI.Global.Update_GUI;
      Status_Name.Set_Label ("Loading character inventories...");
      Do_Events;

      API.Inventories.Character.Update_Inventory
        (GUI.Character.Inventory, GUI.Profile, GUI.The_Manifest);
      GUI.Character.Update_For_Character (GUI.Profile.Characters (0));

      GUI.Global.Render;
      GUI.Character.Render;
      Status_Window.Hide;
      Window.Show;
   end Reload_Profile_Data;

   procedure Reload_Data is

      Window : constant Gtk_Window := Gtk_Window (Builder.Get_Object ("root"));
      Status_Window : constant Gtk_Window :=
        Gtk_Window (Builder.Get_Object ("status_window"));
      Status_Name : constant Gtk_Label :=
        Gtk_Label (Builder.Get_Object ("status_name"));

   begin
      Shared.Debug.Put_Line ("Reloading all data");

      Window.Hide;
      GUI.Authorise;
      Headers := API.Create_Headers (Auth_Data);

      Status_Window.Show;
      Status_Name.Set_Label ("Loading memberships...");
      Do_Events;

      Secrets.Membership := Memberships.Get_Memberships;
      Status_Name.Set_Label ("Loading manifest... (this can take a bit)");
      Do_Events;

      The_Manifest := Manifest.Get_Manifest;
      GUI.Global.Setup_Descriptions; -- One-time setup for GUI descriptions

      Reload_Profile_Data;
   end Reload_Data;

   procedure Error_Message (Name : String; Message : String) is

      Error_Dialog : constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog (Builder.Get_Object ("error_dialog"));

   begin
      Error_Dialog.Set_Markup (Name);
      Error_Dialog.Format_Secondary_Markup (Message);
      Error_Dialog.Show;
   end Error_Message;

   --  Semi-Public. Pauses GUI Thread and updates Widget image data.
   --  TODO is this safe??????
   procedure Image_Callback (Cache : in out Tasks.Download.Download_Cache_Type)
   is
      Temp : Gdk_Pixbuf;
   begin
      GUI_Task.Pause;
      for Cache_Entry of Cache loop
         Temp := GUI.Load_Image (+Cache_Entry.Path, Cache_Entry.Data.all);
         Free (Cache_Entry.Data);

         if not Global_Pixbuf_Cache.Contains (Cache_Entry.Path) then
            Global_Pixbuf_Cache.Insert (Cache_Entry.Path, Temp);
         end if;

         Gtk_Image (Cache_Entry.Widget).Set (Temp);

         Cache_Entry.Widget
           .Unref; --  Allow it to be disposed of if no longer needed
      end loop;

      Cache.Clear; --  Ensure that old Accesses are not reused (clear the queue for the download task)

      GUI_Task.Resume;
   end Image_Callback;
end GUI.Base;
