pragma Ada_2022;

with Interfaces; use Interfaces;

--  Gtkada
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Level_Bar;    use Gtk.Level_Bar;
with Gtk.Label;        use Gtk.Label;
with Gtk.Overlay;      use Gtk.Overlay;

--  Local Packages
with GUI.Base.Get_Overlay;
with GUI.Handlers; use GUI;

with Shared.Strings; use Shared.Strings;

procedure GUI.Base.Populate_Item_Details (D : Manifest.Tools.Item_Description)
is
   --  Widgets (Constant)
   Name : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("item_details_name"));
   Description : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("item_details_description"));
   Energy : constant Gtk_Level_Bar :=
     Gtk_Level_Bar (Builder.Get_Object ("item_details_energy"));
   Stats : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("item_details_stats"));
   Sockets : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("item_details_sockets"));
   Objectives : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("item_details_objectives"));

   --  Indices
   Stat_Index                            : Gint := 0;
   Socket_Index_Horiz, Socket_Index_Vert : Gint := 0;
   Objective_Index                       : Gint := 0;

begin
   --  TODO set background colour by rarity
   Name.Set_Label (+D.Name);
   Description.Set_Label (+D.Item_Type_And_Tier_Display_Name);

   --  Show armour energy capacity
   if D.Energy_Capacity >= 0 then
      Energy.Set_Value (Gdouble (D.Energy_Capacity));
      Energy.Show;
   else
      Energy.Hide;
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
   GUI.Lock_Object.Unlock;
   begin
      Tasks.Download.Contents_Task.Interrupt;
   end;
   GUI.Lock_Object.Lock;

   Clear_Bucket (Objectives);
   Clear_Bucket (Sockets);
   Populate_Sockets :
      for Socket of D.Sockets loop
         --  Add objectives
         Populate_Objectives :
            for Objective of Socket.Objectives loop
               if Objective.Visible then
                  declare
                     Name         : constant Gtk_Label        := Gtk_Label_New;
                     Progress_Bar : constant Gtk_Progress_Bar :=
                       Gtk_Progress_Bar_New;
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

                     Progress_Bar.Set_Fraction
                       (Gdouble (Objective.Progress) /
                        Gdouble (Objective.Completion_Value));
                     Progress_Bar.Set_Show_Text (True);
                     Progress_Bar.Set_Text
                       (Objective.Progress'Image & "/" &
                        Objective.Completion_Value'Image);
                     Progress_Bar.Show;

                     Objectives.Attach (Name, 0, Objective_Index);
                     Objectives.Attach (Progress_Bar, 1, Objective_Index);
                     Objective_Index := @ + 1;
                  end;
               end if;
            end loop Populate_Objectives;

         if Socket.Is_Visible then
            declare
               Overlay : constant Gtk_Overlay :=
                 GUI.Base.Get_Overlay
                   (Get_Description (The_Manifest, Socket.Plug_Hash),
                    Tasks.Download.Contents_Task,
                    User_Callback_Item_Description.To_Marshaller
                      (Handlers.Null_Item_Button_Handler'Access));
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
      end loop Populate_Sockets;

      --  Resume the download task
   Tasks.Download.Contents_Task.Execute (GUI.Image_Callback'Access);
end GUI.Base.Populate_Item_Details;
