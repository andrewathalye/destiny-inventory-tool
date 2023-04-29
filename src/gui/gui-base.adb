pragma Ada_2022;

with Interfaces; use Interfaces;

--  Gtk
with Gtk.Container;      use Gtk.Container;
with Gtk.Button;         use Gtk.Button;
with Gtk.Window;         use Gtk.Window;
with Gtk.Main;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Image;          use Gtk.Image;
with Gtk.Label;          use Gtk.Label;
with Gtk.Alignment;      use Gtk.Alignment;
with Pango.Attributes;   use Pango.Attributes;

--  Local Packages
with GUI.Handlers;
with GUI.Character;
with GUI.Global;
with GUI.Authorise;

with API.Inventories.Global;
with API.Memberships;
with API.Inventories.Character;
with API.Manifest;
use all type API.Manifest.Destiny_Item_Type;
use API;

with Shared.Files;
with Shared.Debug;
with Shared.Strings; use Shared.Strings;
use Shared;

with Secrets; use Secrets;

package body GUI.Base is
   --  Instantiations

   package FUD_Container is new Gtk.Container.Foreach_User_Data
     (Gtk_Container);
   --  Cached High-Frequency Pixbufs
   Placeholder_Icon : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/placeholder_icon.png"));
   Crafted_Masterwork_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/crafted_masterwork_overlay.png"));
   Crafted_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/crafted_overlay.png"));
   Masterwork_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/masterwork_overlay.png"));
   Normal_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/normal_overlay.png"));
   Ornament_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/ornament_overlay.png"));
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

   function Get_Overlay
     (D       : Manifest.Tools.Item_Description;
      T       : Tasks.Download.Download_Task;
      Handler : User_Callback_Item_Description.Marshallers.Marshaller)
      return Gtk_Overlay
   is
      --  Subprograms
      --  TODO: Not 100% game accurate, but fast

      function Should_Watermark
        (D : Manifest.Tools.Item_Description) return Boolean is
        (case D.Item_Type is
           when Manifest.Emblem  |
             Manifest.Subclass   |
             Manifest.Consumable |
             Manifest.DIT_Mod    |
             Manifest.Dummy      |
             Manifest.None       =>
             False,
           when others => True) with
        Inline;

      function Should_State_Overlay
        (D : Manifest.Tools.Item_Description) return Boolean is
        (case D.Item_Type is
           when Manifest.Subclass => False,
           when Manifest.Engram   => False,
           when others            => True) with
        Inline;

      function Should_Display_Label
        (D : Manifest.Tools.Item_Description) return Boolean is
        (D.Quantity > 1 or D.Item_Type = Weapon or D.Item_Type = Armour) with
        Inline;

      --  Variables
      Image         : Gtk_Image;
      Button        : Gtk_Button;
      Overlay       : Gtk_Overlay;
      State_Overlay : Gtk_Image;

   begin
      Gtk_New (Image);
      Gtk_New (Button);
      Gtk_New (Overlay);
      --  Setup Icon and Button

      if Global_Pixbuf_Cache.Contains (D.Icon_Path) then
         Image.Set (Global_Pixbuf_Cache.Element (D.Icon_Path));

      else -- Asynchronously download the icon
         Image.Set (Placeholder_Icon);
         T.Download (D.Icon_Path, Gtk_Widget (Image));
      end if;
      Set_Image (Button, Image);
      User_Callback_Item_Description.Connect
        (Button, "clicked", Handler, User_Data => D);
      Button.Show;
      --  Add Button to Overlay
      Overlay.Add (Button);

      if Should_Watermark (D) then
         --  First Overlay
         --  Add Watermark to Overlay
         if Length (D.Watermark_Path) > 0 then
            declare

               Watermark : Gtk_Image;

            begin
               Gtk_New (Watermark);

               if Global_Pixbuf_Cache.Contains (D.Watermark_Path) then
                  Watermark.Set
                    (Global_Pixbuf_Cache.Element (D.Watermark_Path));

               else -- Asynchronously download the watermark
                  T.Download (D.Watermark_Path, Gtk_Widget (Watermark));
               end if;
               Watermark.Show;
               Overlay.Add_Overlay (Watermark);
               Overlay.Set_Overlay_Pass_Through (Watermark, True);
            end;
         end if;
      end if;
      --  Intermediate Overlay
      --  Add Ornament Icon to Overlay

      if D.Style_Overridden then
         declare

            Ornament_Overlay_GI : Gtk_Image;

         begin
            Gtk_New (Ornament_Overlay_GI);
            Set (Ornament_Overlay_GI, Ornament_Overlay);
            Ornament_Overlay_GI.Show;
            Overlay.Add_Overlay (Ornament_Overlay_GI);
            Overlay.Set_Overlay_Pass_Through (Ornament_Overlay_GI, True);
         end;
      end if;

      if Should_State_Overlay (D) then
         --  Final Overlay
         --  Add Masterwork / Crafted / Normal Overlay
         Gtk_New (State_Overlay);
         Set
           (State_Overlay,
            (if
               D.State.Masterwork and D.State.Crafted
             then
               Crafted_Masterwork_Overlay
             elsif D.State.Masterwork then Masterwork_Overlay
             elsif D.State.Crafted then Crafted_Overlay
             else Normal_Overlay));
         State_Overlay.Show;
         Overlay.Add_Overlay (State_Overlay);
         Overlay.Set_Overlay_Pass_Through (State_Overlay, True);
      end if;

      --  Setup Quantity / Light Level Label if Needed
      if Should_Display_Label (D) then
         declare

            Label       : Gtk_Label;
            Label_Value : constant String :=
              (if D.Quantity > 1 then D.Quantity'Image
               else D.Light_Level'Image);
            Alignment : Gtk_Alignment;
            Attrs     : Pango_Attr_List;

         begin
            Gdk_New (Attrs);
            Gtk_New (Label);
            Gtk_New
              (Alignment,
               Xalign => 0.95,
               Yalign => 0.92,
               Xscale => 0.0,
               Yscale => 0.0);
            Attrs.Change (Attr_Background_New (65_535, 65_535, 65_535));
            Attrs.Change (Attr_Foreground_New (0, 0, 0));
            Label.Set_Attributes (Attrs);
            Label.Set_Label
              (Label_Value (Label_Value'First + 1 .. Label_Value'Last));
            Label.Show;
            Alignment.Add (Label);
            Alignment.Show;
            Overlay.Add_Overlay (Alignment);
            Overlay.Set_Overlay_Pass_Through (Alignment, True);
         end;
      end if;
      Overlay.Set_Tooltip_Text
        ((+D.Name) & ASCII.LF & (+D.Item_Type_And_Tier_Display_Name));
      return Overlay;
   end Get_Overlay;

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
      Window.Hide;
      Status_Window.Show;
      Debug.Put_Line ("Reloading profile data");
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
      Debug.Put_Line ("Reloading all data");
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

end GUI.Base;
