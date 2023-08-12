--  Gtk
with Gtk.Box;       use Gtk.Box;
with Gtk.Level_Bar; use Gtk.Level_Bar;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Image;     use Gtk.Image;
with Gtk.Label;     use Gtk.Label;
with Gtk.Grid;      use Gtk.Grid;

package GUI.Elements.Items is
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
   Energy_Bar : constant Gtk_Level_Bar :=
     Gtk_Level_Bar (Builder.Get_Object ("item_details_energy"));

   Separator_2 : constant Gtk_Separator :=
     Gtk_Separator (Builder.Get_Object ("item_details_separator_2"));
   Secondary_Icon : constant Gtk_Image :=
     Gtk_Image (Builder.Get_Object ("item_details_secondary_icon"));

   Separator_3 : constant Gtk_Separator :=
     Gtk_Separator (Builder.Get_Object ("item_details_separator_3"));
   Stats : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("item_details_stats"));

   Separator_4 : constant Gtk_Separator :=
     Gtk_Separator (Builder.Get_Object ("item_details_separator_4"));
   Sockets : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("item_details_sockets"));
   Objectives : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("item_details_objectives"));

end GUI.Elements.Items;
