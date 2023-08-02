with Interfaces; use Interfaces;

--  Gtkada
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Label;     use Gtk.Label;
with Gtk.Image;     use Gtk.Image;
with Gtk.Button;    use Gtk.Button;

with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Pango.Attributes; use Pango.Attributes;

--  Local Packages
with Shared.Strings; use Shared.Strings;

with API.Manifest;
use all type API.Manifest.Destiny_Item_Type;
use type API.Manifest.Quantity_Type;
use API;

function GUI.Base.Get_Overlay
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
          Manifest.Currency   |
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

   function Is_Crafted_Item_Masterworked
     (D : Manifest.Tools.Item_Description) return Boolean is
     (D.State.Crafted and then Manifest.Tools.Get_Weapon_Level (D) >= 30) with
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
         if Global_Pixbuf_Cache.Contains (+(Bungie_Root & (+D.Icon_Path))) then
            Image.Set
              (Global_Pixbuf_Cache.Element (+(Bungie_Root & (+D.Icon_Path))));

         else -- Asynchronously download the icon
            Image.Set (Placeholder_Icon);
            T.Download (+(Bungie_Root & (+D.Icon_Path)), Gtk_Widget (Image));
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

         if Global_Pixbuf_Cache.Contains (+(Bungie_Root & (+D.Watermark_Path)))
         then
            Watermark.Set
              (Global_Pixbuf_Cache.Element
                 (+(Bungie_Root & (+D.Watermark_Path))));

         else -- Asynchronously download the watermark
            T.Download
              (+(Bungie_Root & (+D.Watermark_Path)), Gtk_Widget (Watermark));
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
         (if Is_Crafted_Item_Masterworked (D) then Crafted_Masterwork_Overlay
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
           (if D.Quantity > 1 then D.Quantity'Image else D.Light_Level'Image);
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
         else -- **SYNCHRONOUSLY** download the damage type icon. This is done so it can be scaled.
            Pixbuf :=
              Scale_Simple
                (Load_Image (+Icon_Path, Tasks.Download.Download (Icon_Path)),
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
end GUI.Base.Get_Overlay;
