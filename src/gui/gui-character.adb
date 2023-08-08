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
use API; -- For general reference

with GUI.Base;
with GUI.Base.Get_Overlay;
with Shared.Strings; use Shared.Strings;
with Shared.Files;
with Shared.Debug;   use Shared;
with Tasks.Download;

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
      T        : Tasks.Download.Download_Task := Tasks.Download.Character_Task;
      Max_Left : Gint                         := 2)
   is
   begin
      Base.Render_Items (List, Bucket, T, Max_Left);
   end Render_Items;

   --  Cache
   Placeholder_Emblem : constant Gdk_Pixbuf :=
     Load_Image (".png", Files.Get_Data ("res/placeholder_emblem.png"));

   procedure Render_Contents (Location : Manifest.Tools.Bucket_Location_Type)
   is

      Contents_Grid : constant Gtk_Grid :=
        Gtk_Grid (Builder.Get_Object ("full_contents_grid"));

   begin
      Base.Clear_Bucket (Contents_Grid);
      Render_Items
        (Inventory (Global_Character_Index).Get (Location),
         Contents_Grid,
         Tasks.Download.Contents_Task);
   end Render_Contents;

   --  Popup full bucket contents if equipped item is clicked
   procedure Equipped_Clicked_Handler
     (Widget    : access Gtk_Widget_Record'Class;
      User_Data : Manifest.Tools.Item_Description)
   is

      Contents : constant Gtk_Popover :=
        Gtk_Popover (Builder.Get_Object ("full_contents"));

   begin
      Tasks.Download.Contents_Task.Interrupt;

      --  Emotes are a special case here
      if User_Data.Bucket_Location = Emote_Collection then
         Render_Contents (Emote);
      else
         Render_Contents (User_Data.Bucket_Location);
      end if;

      Tasks.Download.Contents_Task.Execute (GUI.Base.Image_Callback'Access);

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
        Base.Get_Overlay
          (D,
           Tasks.Download.Character_Task,
           Base.User_Callback_Item_Description.To_Marshaller
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

      --  Buckets
      Postmaster_Grid : constant Gtk_Grid :=
        Gtk_Grid (Builder.Get_Object ("postmaster"));
      Subclass_Box : constant Gtk_Box :=
        Gtk_Box (Builder.Get_Object ("subclass"));

      Kinetic_Box : constant Gtk_Box :=
        Gtk_Box (Builder.Get_Object ("kinetic"));
      Energy_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("energy"));
      Power_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("power"));
      Shell_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("shell"));
      Artefact_Box : constant Gtk_Box :=
        Gtk_Box (Builder.Get_Object ("artefact"));

      Helmet_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("helmet"));
      Gauntlets_Box : constant Gtk_Box :=
        Gtk_Box (Builder.Get_Object ("gauntlets"));
      Chest_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("chest"));
      Leg_Box   : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("leg"));
      Class_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("class"));

      Emblem_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("emblem"));
      Sparrow_Box : constant Gtk_Box :=
        Gtk_Box (Builder.Get_Object ("sparrow"));
      Ship_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("ship"));

      Finisher_Box : constant Gtk_Box :=
        Gtk_Box (Builder.Get_Object ("finisher"));
      Emote_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("emote"));

   begin
      Tasks.Download.Character_Task.Interrupt;
      --  Update Buckets
      Base.Clear_Bucket (Postmaster_Grid);
      Render_Items
        (Character_Items (Postmaster),
         Postmaster_Grid,
         Tasks.Download.Character_Task,
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
      Tasks.Download.Character_Task.Execute (GUI.Base.Image_Callback'Access);
   end Render;
   --  Update UI elements for new character

   procedure Update_For_Character
     (Character_Index : API.Profiles.Character_Range)
   is
      --  Labels and Images to be updated for each character
      Title : constant Gtk_Label  := Gtk_Label (Builder.Get_Object ("title"));
      Light : constant Gtk_Label  := Gtk_Label (Builder.Get_Object ("light"));
      Emblem_Button : constant Gtk_Button :=
        Gtk_Button (Builder.Get_Object ("emblem_button"));
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
          (+(Bungie_Root & (+Emblem_Secondary_Icon_Path)))
      then
         Emblem.Set
           (Global_Pixbuf_Cache.Element
              (+(Bungie_Root & (+Emblem_Secondary_Icon_Path))));
      else
         Emblem.Set (Placeholder_Emblem);
         Tasks.Download.Character_Task.Download
           (+(Bungie_Root & (+Emblem_Secondary_Icon_Path)),
            Gtk_Widget (Emblem));
      end if;
      Emblem_Button.Set_Image (Emblem);
   end Update_For_Character;

end GUI.Character;
