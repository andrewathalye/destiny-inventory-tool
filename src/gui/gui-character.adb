pragma Ada_2022;

--  Gtk
with Gtk.Label;     use Gtk.Label;
with Gtk.Image;     use Gtk.Image;
with Gtk.Grid;      use Gtk.Grid;
with Gtk.Button;    use Gtk.Button;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Box;       use Gtk.Box;
with Gtk.Overlay;   use Gtk.Overlay;
with Gtk.Popover;   use Gtk.Popover;
with Gtk.Alignment; use Gtk.Alignment;
with Gdk.Pixbuf;    use Gdk.Pixbuf;
with Glib;          use Glib;

--  Local Packages
with API.Profiles;
use all type API.Profiles.Transfer_Status_Type;
with API.Manifest.Tools;
use all type API.Manifest.Tools.Bucket_Location_Type;
with API.Constants; use API.Constants;
use API; -- For general reference

with GUI.Base;
with GUI.Items;
with GUI.Tasks; use GUI.Tasks;

with GUI.Elements.Character; use GUI.Elements.Character;

with Shared.Strings; use Shared.Strings;
with Shared.Files;
with Shared.Debug;   use Shared;

package body GUI.Character is
   --  Global-ish state
   Global_Character_Index : Profiles.Character_Range;

   --  Accessors
   function Current_Character return Profiles.Character_Type is
     (GUI.Profile.Characters (Global_Character_Index));
   function Current_Character_Index return Profiles.Character_Range is
     (Global_Character_Index);

   --  Redirections
   procedure Render_Items
     (List     : Inventories.Item_Description_List;
      Bucket   : Gtk_Grid;
      T : Download_Tasks.Download_Task := GUI.Tasks.Character_Task;
      Max_Left : Gint                            := 2)
   is
   begin
      Items.Render_Items (List, Bucket, T, Max_Left);
   end Render_Items;

   --  Cache
   Placeholder_Emblem : constant Gdk_Pixbuf :=
     Load_Image (".png", Files.Get_Data ("res/placeholder_emblem.png").Get);

   procedure Render_Contents (Location : Manifest.Tools.Bucket_Location_Type)
   is
   begin
      Base.Clear_Bucket (Contents_Grid);
      Render_Items
        (Inventory (Global_Character_Index).Get (Location),
         Contents_Grid,
         GUI.Tasks.Contents_Task);
   end Render_Contents;

   --  Popup full bucket contents if equipped item is clicked
   procedure Equipped_Clicked_Handler
     (Widget    : access Gtk_Widget_Record'Class;
      User_Data : Manifest.Tools.Item_Description)
   is
   begin
      GUI.Tasks.Contents_Task.Interrupt;

      --  Emotes are a special case here
      if User_Data.Bucket_Location = Emote_Collection then
         Render_Contents (Emote);
      else
         Render_Contents (User_Data.Bucket_Location);
      end if;

      GUI.Tasks.Contents_Task.Execute (GUI.Base.Image_Callback'Access);

      Contents.Set_Relative_To (Widget);
      Contents.Popup;
   end Equipped_Clicked_Handler;
   --  Render an individual item onto a Gtk_Box

   type Item_Alignment_Type is (Left, Centre, Right);

   procedure Render_Item
     (D              : Manifest.Tools.Item_Description;
      Box            : Gtk_Box;
      Item_Alignment : Item_Alignment_Type := Centre)
   is

      Overlay   : Gtk_Overlay;
      Alignment : constant Gtk_Alignment :=
        Gtk_Alignment_New
          (Xalign =>
             (case Item_Alignment is
                when Left   => 0.0,
                when Centre => 0.5,
                when Right  => 1.0),
           Yalign => 0.5,
           Xscale => 0.0,
           Yscale => 0.0);

   begin
      --  Skip rendering the item if the bucket is empty
      if Length (D.Name) = 0 then
         return;
      end if;

      Overlay :=
        Items.Get_Overlay
          (D,
           GUI.Tasks.Character_Task,
           Items.User_Callback_Item_Description.To_Marshaller
             (Equipped_Clicked_Handler'Access));
      Overlay.Show;

      --  Note: The alignment ensures the button is the same size as the icon
      Alignment.Add (Overlay);
      Alignment.Show;
      Box.Add (Alignment);
   end Render_Item;
   --  Status Updates
   --  Draw items from internal state

   procedure Render is
      --  Renames
      Character_Items :
        Inventories.Item_Description_List_Bucket_Location_Type_Array renames
        Inventory (Global_Character_Index).Get_Sorted;
      Equipped_Items :
        Inventories.Item_Description_Bucket_Location_Type_Array renames
        Inventory (Global_Character_Index).Get_Equipped;
   begin
      GUI.Tasks.Character_Task.Interrupt;

      --  Update Buckets
      Base.Clear_Bucket (Postmaster_Grid);
      Render_Items
        (Character_Items (Postmaster),
         Postmaster_Grid,
         GUI.Tasks.Character_Task,
         6);

      Base.Clear_Bucket (Subclass_Box);
      Render_Item (Equipped_Items (Subclass), Subclass_Box, Right);

      Base.Clear_Bucket (Kinetic_Box);
      Base.Clear_Bucket (Energy_Box);
      Base.Clear_Bucket (Power_Box);
      Base.Clear_Bucket (Shell_Box);
      Base.Clear_Bucket (Artefact_Box);
      Render_Item (Equipped_Items (Kinetic), Kinetic_Box, Right);
      Render_Item (Equipped_Items (Energy), Energy_Box, Right);
      Render_Item (Equipped_Items (Power), Power_Box, Right);
      Render_Item (Equipped_Items (Shell), Shell_Box, Right);
      Render_Item (Equipped_Items (Artefact), Artefact_Box, Right);

      Base.Clear_Bucket (Helmet_Box);
      Base.Clear_Bucket (Gauntlets_Box);
      Base.Clear_Bucket (Chest_Box);
      Base.Clear_Bucket (Leg_Box);
      Base.Clear_Bucket (Class_Box);
      Render_Item (Equipped_Items (Helmet), Helmet_Box, Left);
      Render_Item (Equipped_Items (Gauntlets), Gauntlets_Box, Left);
      Render_Item (Equipped_Items (Chest), Chest_Box, Left);
      Render_Item (Equipped_Items (Leg), Leg_Box, Left);
      Render_Item (Equipped_Items (Class), Class_Box, Left);

      Base.Clear_Bucket (Emblem_Box);
      Base.Clear_Bucket (Sparrow_Box);
      Base.Clear_Bucket (Ship_Box);
      Render_Item (Equipped_Items (Emblem), Emblem_Box, Right);
      Render_Item (Equipped_Items (Sparrow), Sparrow_Box, Right);
      Render_Item (Equipped_Items (Ship), Ship_Box, Right);

      Base.Clear_Bucket (Finisher_Box);
      Base.Clear_Bucket (Emote_Box);
      Render_Item (Equipped_Items (Finisher), Finisher_Box, Left);
      Render_Item (Equipped_Items (Emote_Collection), Emote_Box, Left);
      --  TODO: Render engrams?

      --  Complete downloads queued by Render calls
      GUI.Tasks.Character_Task.Execute (GUI.Base.Image_Callback'Access);
   end Render;
   --  Update UI elements for new character

   procedure Update_For_Character
     (Character_Index : API.Profiles.Character_Range)
   is
      Emblem : Gtk_Image;

      --  Renames
      Character :
        Profiles.Character_Type renames
        GUI.Profile.Characters (Character_Index);

      Emblem_Secondary_Icon_Path :
        Unbounded_String renames
        GUI.The_Manifest.Destiny_Inventory_Items (Character.Emblem_Hash)
          .Secondary_Icon_Path;
   begin
      Debug.Put_Line
        ("Updating UI for " &
         Manifest.Tools.Get_Description (The_Manifest, Character));

      Global_Character_Index := Character_Index;

      --  Update Labels
      Set_Label (Title, +Manifest.Tools.Get_Title (The_Manifest, Character));
      Set_Label (Light, Character.Light'Image);

      --  Update Emblem
      Gtk_New (Emblem);

      if Global_Pixbuf_Cache.Contains
          (+(API.Constants.Bungie_Root & (+Emblem_Secondary_Icon_Path)))
      then
         Emblem.Set
           (Global_Pixbuf_Cache.Element
              (+(API.Constants.Bungie_Root & (+Emblem_Secondary_Icon_Path))));
      else
         Emblem.Set (Placeholder_Emblem);
         GUI.Tasks.Character_Task.Download
           (+(API.Constants.Bungie_Root & (+Emblem_Secondary_Icon_Path)),
            Gtk_Widget (Emblem));
      end if;
      Emblem_Button.Set_Image (Emblem);
   end Update_For_Character;

end GUI.Character;
