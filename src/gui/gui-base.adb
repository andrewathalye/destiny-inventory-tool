pragma Ada_2022;

--  Gtk
with Gtk.Main;
with Gtk.Container;      use Gtk.Container;
with Gtk.Window;         use Gtk.Window;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Label;          use Gtk.Label;
with Gtk.Image;          use Gtk.Image;
with Gtk.Widget;         use Gtk.Widget;

with Gdk.Pixbuf; use Gdk.Pixbuf;

--  Local Packages
with GUI.Character;
with GUI.Global;
with GUI.Authorise;
with GUI.GUI_Tasks; use GUI.GUI_Tasks;

with GUI.Elements.Base; use GUI.Elements.Base;

with API.Inventories.Global;
with API.Memberships;
with API.Manifest;
with API.Profiles;

with API.Definitions.Destiny_Inventory_Item;
use all type API.Definitions.Destiny_Inventory_Item.Destiny_Item_Type;

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
   begin
      Status_Window.Show;

      Shared.Debug.Put_Line ("Reloading profile data");
      Status_Name.Set_Label ("Loading profile...");
      Do_Events;

      Profile := Profiles.Get_Profile (Secrets.Membership);
      Status_Name.Set_Label ("Loading vault...");
      Do_Events;

      GUI.Global.Inventory.Update (GUI.Profile, GUI.The_Manifest);
      GUI.Global.Update_GUI;
      Status_Name.Set_Label ("Loading character inventories...");
      Do_Events;

      for Character of GUI.Profile.Characters loop
         GUI.Character.Inventory
           (GUI.Profile.Characters.Find_Index (Character))
           .Update
           (GUI.Profile, GUI.The_Manifest, Character);
      end loop;
      GUI.Character.Update_For_Character (API.Profiles.Character_Range'First);

      GUI.Global.Render;
      GUI.Character.Render;

      Status_Window.Hide;
   end Reload_Profile_Data;

   procedure Reload_Data is
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

      --  Present the (blank) main window
      Window.Show;
      Do_Events;

      Reload_Profile_Data;
   end Reload_Data;

   procedure Error_Message (Name : String; Message : String) is
   begin
      Error_Dialog.Set_Markup (Name);
      Error_Dialog.Format_Secondary_Markup (Message);
      Error_Dialog.Show;
   end Error_Message;

   --  Semi-Public. Does NOT pause GUI Thread. Updates Widget image data
   --  Note: NOT thread-safe!!!
   procedure Event_Image_Callback
     (Cache : in out Tasks.Download.Download_Cache_Type)
   is
      Temp : Gdk_Pixbuf;
   begin
      for Cache_Entry of Cache loop
         Temp := GUI.Load_Image (+Cache_Entry.Path, Cache_Entry.Data.Get);

         if not Global_Pixbuf_Cache.Contains (Cache_Entry.Path) then
            Global_Pixbuf_Cache.Insert (Cache_Entry.Path, Temp);
         end if;

         Gtk_Image (Cache_Entry.Widget).Set (Temp);

         Cache_Entry.Widget
           .Unref; --  Allow it to be disposed of if no longer needed
      end loop;

      Cache
        .Clear; --  Ensure that old Accesses are not reused (clear the queue for the download task)
   end Event_Image_Callback;

   --  Semi-Public. Pauses GUI Thread and updates Widget image data.
   --  TODO is this safe??????
   procedure Image_Callback (Cache : in out Tasks.Download.Download_Cache_Type)
   is
   begin
      GUI_Task.Pause;
      Event_Image_Callback (Cache);
      GUI_Task.Resume;
   end Image_Callback;

end GUI.Base;
