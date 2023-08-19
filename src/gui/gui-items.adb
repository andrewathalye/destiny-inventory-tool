pragma Ada_2022;

with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
use Ada.Calendar;
with Interfaces; use Interfaces;

--  Gtkada
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Level_Bar;    use Gtk.Level_Bar;
with Gtk.Label;        use Gtk.Label;
with Gtk.Separator;    use Gtk.Separator;
with Gtk.Image;        use Gtk.Image;
with Gtk.Button;       use Gtk.Button;
with Gtk.Alignment;    use Gtk.Alignment;

with Pango.Attributes; use Pango.Attributes;

--  Local Packages
with API.Definitions.Destiny_Inventory_Item;
use all type API.Definitions.Destiny_Inventory_Item.Destiny_Tier_Type;
use all type API.Definitions.Destiny_Inventory_Item.Destiny_Item_Type;

with API.Definitions.Hashes;
use type API.Definitions.Hashes.Destiny_Damage_Type_Definition_Manifest_Hash;
use type API.Definitions.Quantity_Type;

with GUI.Handlers;
with GUI.Base; use GUI.Base;

with GUI.Elements.Items; use GUI.Elements.Items;

with Shared.Strings; use Shared.Strings;
with Shared.Files;   use Shared.Files;
use Shared;

with Tasks.Synchronous_Download;

package body GUI.Items is
   --  Cached High-Frequency Pixbufs
   Placeholder_Icon : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/placeholder_icon.png").Get);
   Crafted_Masterwork_Overlay : constant Gdk_Pixbuf :=
     Load_Image
       ("png", Files.Get_Data ("res/crafted_masterwork_overlay.png").Get);
   Crafted_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/crafted_overlay.png").Get);
   Masterwork_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/masterwork_overlay.png").Get);
   Normal_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/normal_overlay.png").Get);
   Ornament_Overlay : constant Gdk_Pixbuf :=
     Load_Image ("png", Files.Get_Data ("res/ornament_overlay.png").Get);

   --  Constants for separate subprograms
   Unix_Epoch : constant Ada.Calendar.Time :=
     Ada.Calendar.Formatting.Value ("1970-01-01 00:00:00");

   --  Inventory Item Rendering (Exported)
   procedure Render_Items
     (List     : Inventories.Item_Description_List;
      Bucket   : Gtk_Grid;
      T        : GUI_Download_Task.Download_Task;
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

   function Get_Overlay
     (D       : Manifest.Tools.Item_Description;
      T       : GUI_Download_Task.Download_Task;
      Handler : User_Callback_Item_Description.Marshallers.Marshaller)
      return Gtk_Overlay
   is

      --  Subprograms
      --  TODO: Not 100% game accurate, but fast

      function Should_Watermark
        (D : Manifest.Tools.Item_Description) return Boolean is
        (case D.Item_Type is
           when Emblem  |
             Subclass   |
             Consumable |
             DIT_Mod    |
             Dummy      |
             Currency   |
             None       =>
             False,
           when others => True) with
        Inline;

      function Should_State_Overlay
        (D : Manifest.Tools.Item_Description) return Boolean is
        (case D.Item_Type is
           when Subclass => False,
           when Engram   => False,
           when others   => True) with
        Inline;

      function Should_Display_Label
        (D : Manifest.Tools.Item_Description) return Boolean is
        (D.Quantity > 1 or D.Item_Type = Weapon or D.Item_Type = Armour) with
        Inline;

      function Is_Crafted_Item_Masterworked
        (D : Manifest.Tools.Item_Description) return Boolean is
        (D.State.Crafted
         and then Manifest.Tools.Get_Weapon_Level (D) >= 30) with
        Inline;

      --  Variables
      Overlay       : Gtk_Overlay;
      State_Overlay : Gtk_Image;

   begin
      Gtk_New (Overlay);

      --  Setup Icon and Button
      --  First Overlay
      declare
         Image  : Gtk_Image;
         Button : Gtk_Button;
      begin
         Gtk_New (Image);
         Gtk_New (Button);

         if Length (D.Icon_Path) > 0 then
            if Global_Pixbuf_Cache.Contains (+(Bungie_Root & (+D.Icon_Path)))
            then
               Image.Set
                 (Global_Pixbuf_Cache.Element
                    (+(Bungie_Root & (+D.Icon_Path))));

            else -- Asynchronously download the icon
               Image.Set (Placeholder_Icon);
               T.Download
                 (+(Bungie_Root & (+D.Icon_Path)), Gtk_Widget (Image));
            end if;
         else -- No image available
            Image.Set (Placeholder_Icon);
         end if;

         Set_Image (Button, Image);
         User_Callback_Item_Description.Connect
           (Button, "clicked", Handler, User_Data => D);
         Button.Show;

         --  Add Button to Overlay
         Overlay.Add (Button);
      end;

      --  Add Watermark
      --  Second Overlay
      if Should_Watermark (D) and then Length (D.Watermark_Path) > 0 then
         declare

            Watermark : Gtk_Image;

         begin
            Gtk_New (Watermark);

            if Global_Pixbuf_Cache.Contains
                (+(Bungie_Root & (+D.Watermark_Path)))
            then
               Watermark.Set
                 (Global_Pixbuf_Cache.Element
                    (+(Bungie_Root & (+D.Watermark_Path))));

            else -- Asynchronously download the watermark
               T.Download
                 (+(Bungie_Root & (+D.Watermark_Path)),
                  Gtk_Widget (Watermark));
            end if;

            Watermark.Show;
            Overlay.Add_Overlay (Watermark);
            Overlay.Set_Overlay_Pass_Through (Watermark, True);
         end;
      end if;

      --  Add Ornament Icon to Overlay
      --  Third Overlay
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

      --  Add Masterwork / Crafted / Normal Overlay
      --  Fourth Overlay
      if Should_State_Overlay (D) then
         Gtk_New (State_Overlay);
         Set
           (State_Overlay,
            (if
               Is_Crafted_Item_Masterworked (D)
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
      --  Fifth Overlay
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
               Xalign => 0.92,
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

      --  Show Damage Type if Necessary
      --  Sixth Overlay
      if D.Default_Damage_Type_Hash /= 0
        and then The_Manifest.Destiny_Damage_Types (D.Default_Damage_Type_Hash)
          .Show_Icon
      then
         declare
            Pixbuf    : Gdk_Pixbuf;
            Image     : Gtk_Image;
            Alignment : Gtk_Alignment;

            Icon_Path : constant Unbounded_String :=
              +(Bungie_Root &
               (+The_Manifest.Destiny_Damage_Types (D.Default_Damage_Type_Hash)
                  .Icon_Path));
         begin
            Gtk_New (Image);

            if Global_Pixbuf_Cache.Contains (Icon_Path) then
               Image.Set (Global_Pixbuf_Cache (Icon_Path));
            else -- Note **SYNCHRONOUSLY** download the damage type icon. This is done so it can be scaled.
               Pixbuf :=
                 Scale_Simple
                   (Load_Image
                      (+Icon_Path,
                       Tasks.Synchronous_Download.Download (Icon_Path).Get),
                    24,
                    24);
               Global_Pixbuf_Cache.Insert (Icon_Path, Pixbuf);
               Image.Set (Pixbuf);
            end if;

            Image.Show;

            Gtk_New
              (Alignment,
               Xalign => 0.90,
               Yalign => 0.10,
               Xscale => 0.0,
               Yscale => 0.0);
            Alignment.Add (Image);
            Alignment.Show;

            Overlay.Add_Overlay (Alignment);
            Overlay.Set_Overlay_Pass_Through (Alignment, True);
         end;
      end if;

      Overlay.Set_Tooltip_Text
        ((+D.Name) & ASCII.LF & (+D.Item_Type_And_Tier_Display_Name));
      return Overlay;
   end Get_Overlay;

   procedure Populate_Item_Details (D : Manifest.Tools.Item_Description) is
      --  Compile-Time Constants (DestinyObjectiveDefinition)
      Crafting_Weapon_Timestamp : constant := 3_947_811_849;
      Engram_Tracker_Hash       : constant := 1_624_697_519;

      --  Indices
      Stat_Index                            : Gint := 0;
      Socket_Index_Horiz, Socket_Index_Vert : Gint := 0;
      Objective_Index                       : Gint := 0;

      --  Subprograms

      --  Format text with string variables
      --  Text.....{var:[STRING VARIABLE]}Text.....
      function Format_Description (Input : String) return String is
         Output : Unbounded_String;

         First_Pos, Second_Pos : Natural;

         I : Natural := Input'First;
      begin
         Find_First_Pos :
            while I <= Input'Last loop
               if Input (I) /= '{' then
                  Append (Output, Input (I));
               else
                  First_Pos := I + 5; --  first digit of ID

                  --  Exit the program if it is not possible to find the second bracket.
                  --  Continuing in this case would cause undefined behaviour
                  Outer_Loop :
                     loop
                        Find_Second_Pos :
                           for I2 in I .. Input'Last loop
                              if Input (I2) = '}' then
                                 Second_Pos := I2 - 1; --  last digit of ID
                                 exit Outer_Loop;
                              end if;
                           end loop Find_Second_Pos;

                        raise Program_Error
                          with "Format_Description: no corresponding closing bracket found.";
                     end loop Outer_Loop;

                     --  String variable formatting
                  declare
                     Text : constant String :=
                       API.Profiles.String_Variable_Type'Image
                         (GUI.Profile.Profile_String_Variables
                            (Unsigned_32'Value
                               (Input (First_Pos .. Second_Pos))));
                  begin
                     Append (Output, Text (Text'First + 1 .. Text'Last));
                  end;

                  I := Second_Pos + 1;
                  --  matches the '}'. Will be skipped over by the next statement
               end if;

               I := @ + 1;
            end loop Find_First_Pos;

         return +Output;
      end Format_Description;
   begin
      --  Interrupt the download task so more items can be queued
      GUI.GUI_Tasks.Contents_Task.Interrupt;

      --  Set background colour by rarity (using CSS)
      Name_Type_Box.Set_Name
        ((case D.Tier_Type is
            when Unknown |
              Basic      |
              Currency   =>
              "item_details_name_type_box_basic",
            when Common   => "item_details_name_type_box_common",
            when Rare     => "item_details_name_type_box_rare",
            when Superior => "item_details_name_type_box_superior",
            when Exotic   => "item_details_name_type_box_exotic"));

      Name.Set_Label (+D.Name);
      Item_Type.Set_Label (+D.Item_Type_And_Tier_Display_Name);

      --  Show description. Note: Some items require description parsing.
      case D.Item_Hash is
         when Engram_Tracker_Hash =>
            Description.Set_Text (Format_Description (+D.Description));
            Description.Show;
         when others =>
            if Length (D.Description) > 0 then
               Description.Set_Text (+D.Description);
               Description.Show;
            else
               Description.Hide;
            end if;
      end case;

      --  Show secondary icon if present
      if D.Secondary_Icon_Path /= Null_Unbounded_String then
         Separator_1.Show;
         Secondary_Icon.Show;
         Secondary_Icon.Set (Placeholder_Icon);

         --  Download the icon or use the cached version
         if Global_Pixbuf_Cache.Contains
             (+(Bungie_Root & (+D.Secondary_Icon_Path)))
         then
            Secondary_Icon.Set
              (Global_Pixbuf_Cache.Element
                 (+(Bungie_Root & (+D.Secondary_Icon_Path))));

         else -- Asynchronous download
            GUI.GUI_Tasks.Contents_Task.Download
              (+(Bungie_Root & (+D.Secondary_Icon_Path)),
               Gtk_Widget (Secondary_Icon));
         end if;
      else
         Separator_1.Hide;
         Secondary_Icon.Hide;
      end if;

      --  Show armour energy capacity
      if D.Energy_Capacity >= 0 then
         Energy_Bar.Set_Value (Gdouble (D.Energy_Capacity));
         Separator_2.Show;
         Energy_Bar.Show;
      else
         Separator_2.Hide;
         Energy_Bar.Hide;
      end if;

      --  Stats
      if D.Stats.Is_Empty then
         Separator_3.Hide;
      else
         Separator_3.Show;
      end if;

      Clear_Bucket (Stats);
      Populate_Stats :
         for C in D.Stats.Iterate loop
            declare
               Name         : constant Gtk_Label        := Gtk_Label_New;
               Progress_Bar : constant Gtk_Progress_Bar :=
                 Gtk_Progress_Bar_New;
            begin
               Name.Set_Label
                 (+The_Manifest.Destiny_Stats (Profiles.Stats_Maps.Key (C)));
               Name.Show;

               Progress_Bar.Set_Fraction
                 (Gdouble (Profiles.Stats_Maps.Element (C)) / Gdouble (100));
               Progress_Bar.Set_Show_Text (True);
               Progress_Bar.Set_Text (Profiles.Stats_Maps.Element (C)'Image);
               Progress_Bar.Show;

               Stats.Attach (Name, 0, Stat_Index);
               Stats.Attach (Progress_Bar, 1, Stat_Index);
               Stat_Index := @ + 1;
            end;
         end loop Populate_Stats;

         --  Sockets
      if D.Sockets.Is_Empty then
         Separator_4.Hide;
      else
         Separator_4.Show;
      end if;

      Clear_Bucket (Sockets);
      Clear_Bucket (Objectives);
      Populate_Sockets :
         for Socket of D.Sockets loop
            if Socket.Is_Visible then
               declare
                  Overlay : constant Gtk_Overlay :=
                    Get_Overlay
                      (Get_Description (The_Manifest, Socket.Plug_Hash),
                       GUI.GUI_Tasks.Contents_Task,
                       User_Callback_Item_Description.To_Marshaller
                         (Handlers.Socket_Item_Button_Handler'Access));
               begin
                  Overlay.Show;
                  Sockets.Attach
                    (Overlay, Socket_Index_Horiz, Socket_Index_Vert);

                  --  Line-wrapping to avoid excessively-wide menus
                  Socket_Index_Horiz := @ + 1;
                  if Socket_Index_Horiz > 4 then
                     Socket_Index_Horiz := 0;
                     Socket_Index_Vert  := @ + 1;
                  end if;
               end;
            end if;

            --  Add objectives
            Populate_Objectives :
               for Objective of Socket.Objectives loop
                  if Objective.Visible then
                     declare
                        Name         : constant Gtk_Label := Gtk_Label_New;
                        Progress_Bar : constant Gtk_Progress_Bar :=
                          Gtk_Progress_Bar_New;

                        Label : constant Gtk_Label := Gtk_Label_New;
                     begin
                        Name.Set_Label
                          ((+The_Manifest.Destiny_Inventory_Items
                              (Socket.Plug_Hash)
                              .Name) &
                           ": " &
                           (+The_Manifest.Destiny_Objectives
                              (Objective.Objective_Hash)
                              .Progress_Description));
                        Name.Show;
                        Objectives.Attach (Name, 0, Objective_Index);

                        --  Some objectives are really just numbers, rather than progress bars.
                        --  Others, like Catalyst objectives, are really progress bars
                        if Objective.Completion_Value > 1 then
                           Progress_Bar.Set_Fraction
                             (Gdouble (Objective.Progress) /
                              Gdouble (Objective.Completion_Value));
                           Progress_Bar.Set_Show_Text (True);
                           Progress_Bar.Set_Text
                             (Objective.Progress'Image & "/" &
                              Objective.Completion_Value'Image);
                           Progress_Bar.Show;
                           Objectives.Attach
                             (Progress_Bar, 1, Objective_Index);
                        else --  Not a progress bar
                           --  Format the Crafting Weapon Timestamp as a Date
                           case Objective.Objective_Hash is
                              when Crafting_Weapon_Timestamp =>
                                 Label.Set_Label
                                   (Image
                                      (Unix_Epoch +
                                       Duration (Objective.Progress)));
                              when others =>
                                 Label.Set_Label (Objective.Progress'Image);
                           end case;

                           Label.Show;
                           Objectives.Attach (Label, 1, Objective_Index);
                        end if;

                        Objective_Index := @ + 1;
                     end;
                  end if;
               end loop Populate_Objectives;

         end loop Populate_Sockets;

         --  Resume the download task
      GUI.GUI_Tasks.Contents_Task.Execute (GUI.Base.Image_Callback'Access);
   end Populate_Item_Details;
end GUI.Items;
