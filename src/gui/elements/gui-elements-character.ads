--  Gtk
with Gtk.Box;     use Gtk.Box;
with Gtk.Grid;    use Gtk.Grid;
with Gtk.Label;   use Gtk.Label;
with Gtk.Button;  use Gtk.Button;
with Gtk.Popover; use Gtk.Popover;

package GUI.Elements.Character is
   --  Contents
   Contents_Grid : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("full_contents_grid"));
   Contents : constant Gtk_Popover :=
     Gtk_Popover (Builder.Get_Object ("full_contents"));

   --  Buckets
   Postmaster_Grid : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("postmaster"));
   Subclass_Box : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("subclass"));

   Kinetic_Box  : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("kinetic"));
   Energy_Box   : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("energy"));
   Power_Box    : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("power"));
   Shell_Box    : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("shell"));
   Artefact_Box : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("artefact"));

   Helmet_Box    : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("helmet"));
   Gauntlets_Box : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("gauntlets"));
   Chest_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("chest"));
   Leg_Box   : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("leg"));
   Class_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("class"));

   Emblem_Box  : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("emblem"));
   Sparrow_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("sparrow"));
   Ship_Box    : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("ship"));

   Finisher_Box : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("finisher"));
   Emote_Box : constant Gtk_Box := Gtk_Box (Builder.Get_Object ("emote"));

   --  Labels and Images to be updated for each character
   Title : constant Gtk_Label  := Gtk_Label (Builder.Get_Object ("title"));
   Light : constant Gtk_Label  := Gtk_Label (Builder.Get_Object ("light"));
   Emblem_Button : constant Gtk_Button :=
     Gtk_Button (Builder.Get_Object ("emblem_button"));
end GUI.Elements.Character;
