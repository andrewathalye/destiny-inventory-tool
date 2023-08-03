pragma Ada_2022;

with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
use Ada.Calendar;
with Interfaces; use Interfaces;

--  Gtkada
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Level_Bar;    use Gtk.Level_Bar;
with Gtk.Label;        use Gtk.Label;
with Gtk.Overlay;      use Gtk.Overlay;
with Gtk.Separator;    use Gtk.Separator;

--  Local Packages
with API.Manifest;
use all type API.Manifest.Destiny_Tier_Type;
use type API.Manifest.Quantity_Type;

with GUI.Base.Get_Overlay;
with GUI.Handlers; use GUI;

with Shared.Strings; use Shared.Strings;

procedure GUI.Base.Populate_Item_Details (D : Manifest.Tools.Item_Description)
is
   --  Compile-Time Constants (DestinyObjectiveDefinition)
   Crafting_Weapon_Timestamp : constant := 3_947_811_849;
   Engram_Tracker_Hash       : constant := 1_624_697_519;

   --  Widgets (Constant)
   Name_Type_Box : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("item_details_name_type_box"));
   Name : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("item_details_name"));
   Item_Type : constant Gtk_Label :=
     Gtk_Label
       (Builder.Get_Object ("item_details_type_and_tier_display_name"));

   Description : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("item_details_description"));

   Separator_1 : constant Gtk_Separator :=
     Gtk_Separator (Builder.Get_Object ("item_details_separator_1"));
   Energy : constant Gtk_Level_Bar :=
     Gtk_Level_Bar (Builder.Get_Object ("item_details_energy"));

   Separator_2 : constant Gtk_Separator :=
     Gtk_Separator (Builder.Get_Object ("item_details_separator_2"));
   Stats : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("item_details_stats"));

   Separator_3 : constant Gtk_Separator :=
     Gtk_Separator (Builder.Get_Object ("item_details_separator_3"));
   Sockets : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("item_details_sockets"));
   Objectives : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("item_details_objectives"));

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
   --  Set background colour by rarity (using CSS)
   Name_Type_Box.Set_Name
     ((case D.Tier_Type is
         when Unknown | Basic | Currency => "item_details_name_type_box_basic",
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

   --  Show armour energy capacity
   if D.Energy_Capacity >= 0 then
      Energy.Set_Value (Gdouble (D.Energy_Capacity));
      Separator_1.Show;
      Energy.Show;
   else
      Separator_1.Hide;
      Energy.Hide;
   end if;

   --  Stats
   if D.Stats.Is_Empty then
      Separator_2.Hide;
   else
      Separator_2.Show;
   end if;

   Clear_Bucket (Stats);
   Populate_Stats :
      for C in D.Stats.Iterate loop
         declare
            Name         : constant Gtk_Label        := Gtk_Label_New;
            Progress_Bar : constant Gtk_Progress_Bar := Gtk_Progress_Bar_New;
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

      --  Interrupt the download task so more items can be queued
   Tasks.Download.Contents_Task.Interrupt;

   --  Sockets
   if D.Sockets.Is_Empty then
      Separator_3.Hide;
   else
      Separator_3.Show;
   end if;

   Clear_Bucket (Sockets);
   Clear_Bucket (Objectives);
   Populate_Sockets :
      for Socket of D.Sockets loop
         if Socket.Is_Visible then
            declare
               Overlay : constant Gtk_Overlay :=
                 GUI.Base.Get_Overlay
                   (Get_Description (The_Manifest, Socket.Plug_Hash),
                    Tasks.Download.Contents_Task,
                    User_Callback_Item_Description.To_Marshaller
                      (Handlers.Socket_Item_Button_Handler'Access));
            begin
               Overlay.Show;
               Sockets.Attach (Overlay, Socket_Index_Horiz, Socket_Index_Vert);

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
                     Name         : constant Gtk_Label        := Gtk_Label_New;
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
                        Objectives.Attach (Progress_Bar, 1, Objective_Index);
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
   Tasks.Download.Contents_Task.Execute (GUI.Base.Image_Callback'Access);
end GUI.Base.Populate_Item_Details;
