pragma Ada_2022;

--  Gtkada
with Gtk.Label;    use Gtk.Label;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Popover;  use Gtk.Popover;
with Gtk.Grid;     use Gtk.Grid;
with Gtk.Box;      use Gtk.Box;
with Gtk.Image;    use Gtk.Image;
with Gtk.Button;   use Gtk.Button;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Glib;         use Glib;

--  Local Packages
with GUI.Handlers;
with GUI.Base;
with GUI.Items;
with GUI.Tasks; use GUI.Tasks;

with GUI.Elements.Global; use GUI.Elements.Global;

with API.Profiles;
with API.Manifest.Tools;
use all type API.Manifest.Tools.Bucket_Location_Type;

with API.Definitions.Hashes;
with API.Constants;

with Shared.Files;
with Shared.Strings; use Shared.Strings;

package body GUI.Global is
   --  Instantiations
   package User_Callback_Character_Range is new User_Callback
     (Gtk_Widget_Record, API.Profiles.Character_Range);
   package User_Callback_Character is new User_Callback
     (Gtk_Widget_Record, Profiles.Character_Type);

   --  Constants
   Vault_Vendor_Hash : constant := 1_037_843_411;

   --  Cache
   Placeholder_Icon : constant Gdk_Pixbuf :=
     Load_Image
       ("png", Shared.Files.Get_Data ("res/placeholder_icon.png").Get);

   --  Redirections
   procedure Render_Items
     (List     : Inventories.Item_Description_List;
      Bucket   : Gtk_Grid;
      Max_Left : Gint                            := 2;
      T        : Download_Tasks.Download_Task := GUI.Tasks.Global_Task)
   is
   begin
      Items.Render_Items (List, Bucket, T, Max_Left);
   end Render_Items;

   --  Private Subprograms

   procedure Setup_Character_Menu is
      Count : Gint := 0;
   begin
      --  It is possible for characters to change on profile reload, so be prepared
      Base.Clear_Bucket (Character_Grid);

      for C of Profile.Characters loop
         declare

            Image  : Gtk_Image;
            Button : Gtk_Button;

            --  Renames
            Emblem_Secondary_Overlay :
              Unbounded_String renames
              The_Manifest.Destiny_Inventory_Items (C.Emblem_Hash)
                .Secondary_Overlay_Path;
         begin
            Gtk_New (Image);
            Gtk_New (Button, Manifest.Tools.Get_Description (The_Manifest, C));

            --  Load emblem
            if Global_Pixbuf_Cache.Contains (Emblem_Secondary_Overlay) then
               Image.Set
                 (Global_Pixbuf_Cache.Element
                    (+(API.Constants.Bungie_Root & (+Emblem_Secondary_Overlay))));

            else
               Image.Set (Placeholder_Icon);
               GUI.Tasks.Global_Task.Download
                 (+(API.Constants.Bungie_Root & (+Emblem_Secondary_Overlay)),
                  Gtk_Widget (Image));
            end if;

            User_Callback_Character_Range.Connect
              (Button,
               "clicked",
               User_Callback_Character_Range.To_Marshaller
                 (Handlers.Character_Menu_Button_Clicked_Handler'Access),
               User_Data => Profiles.Character_Range (Count + 1));

            Image.Show;
            Button.Show;

            Character_Grid.Attach (Image, 0, Count);
            Character_Grid.Attach (Button, 1, Count);

            Count := @ + 1;
         end;
      end loop;

      Character_Menu.Set_Relative_To (Emblem_Button);
   end Setup_Character_Menu;

   --  One-time init for "transfer_grid"
   procedure Setup_Transfer_Menu is
      Count : Gint :=
        0; --  Ada containers start at 1 usually, but Gtk stuff starts at 0

      Vault_Icon_Path : constant Unbounded_String :=
        The_Manifest.Destiny_Vendors (Vault_Vendor_Hash).Icon_Path;
      Vault_Image  : Gtk_Image;
      Vault_Button : Gtk_Button;

   begin
      --  A lot of things can change on reload, so best to be prepared
      Base.Clear_Bucket (Transfer_Grid);

      for C of Profile.Characters loop
         declare

            Image  : Gtk_Image;
            Button : Gtk_Button;

            --  Renames
            Emblem_Secondary_Overlay :
              Unbounded_String renames
              The_Manifest.Destiny_Inventory_Items (C.Emblem_Hash)
                .Secondary_Overlay_Path;
         begin
            Gtk_New (Image);
            Gtk_New (Button, Manifest.Tools.Get_Description (The_Manifest, C));

            --  Load emblem
            if Global_Pixbuf_Cache.Contains
                (+(API.Constants.Bungie_Root & (+Emblem_Secondary_Overlay)))
            then
               Image.Set
                 (Global_Pixbuf_Cache.Element
                    (+(API.Constants.Bungie_Root & (+Emblem_Secondary_Overlay))));

            else
               Image.Set (Placeholder_Icon);
               GUI.Tasks.Global_Task.Download
                 (+(API.Constants.Bungie_Root & (+Emblem_Secondary_Overlay)),
                  Gtk_Widget (Image));
            end if;

            Image.Show;
            User_Callback_Character.Connect
              (Button,
               "clicked",
               User_Callback_Character.To_Marshaller
                 (Handlers.Transfer_Handler'Access),
               C);
            Button.Show;

            Transfer_Grid.Attach (Image, 0, Count);
            Transfer_Grid.Attach (Button, 1, Count);

            Count := @ + 1;
         end;
      end loop;

      --  Vault Icon and Button
      Gtk_New (Vault_Image);
      if Global_Pixbuf_Cache.Contains (+(API.Constants.Bungie_Root & (+Vault_Icon_Path)))
      then
         Vault_Image.Set
           (Global_Pixbuf_Cache ((+(API.Constants.Bungie_Root & (+Vault_Icon_Path)))));
      else
         Vault_Image.Set (Placeholder_Icon);
         GUI.Tasks.Global_Task.Download
           (+(API.Constants.Bungie_Root & (+Vault_Icon_Path)), Gtk_Widget (Vault_Image));
      end if;
      Vault_Image.Show;

      Gtk_New (Vault_Button, "Vault");
      Handlers.Widget_Callback.Connect
        (Vault_Button,
         "clicked",
         Handlers.Widget_Callback.To_Marshaller
           (Handlers.Vault_Handler'Access));
      Vault_Button.Show;

      Transfer_Grid.Attach (Vault_Image, 0, Count);
      Transfer_Grid.Attach (Vault_Button, 1, Count);
   end Setup_Transfer_Menu;

   procedure Render_Currencies is
   begin
      Base.Clear_Bucket (Vault_Currency);
      Render_Items (Inventory.Get_Currency, Vault_Currency, 7);
   end Render_Currencies;

   --  Public Subprograms

   --  Status Updates
   --  Global UI Render

   procedure Render is
      --  Renames
      Vault_Inventory :
        Inventories.Item_Description_List_Bucket_Location_Type_Array renames
        Inventory.Get_Sorted;
   begin
      GUI.Tasks.Global_Task.Interrupt;

      Base.Clear_Bucket (Vault_Kinetic);
      Base.Clear_Bucket (Vault_Energy);
      Base.Clear_Bucket (Vault_Power);
      Base.Clear_Bucket (Vault_Shell);
      Render_Items (Vault_Inventory (Kinetic), Vault_Kinetic, 10);
      Render_Items (Vault_Inventory (Energy), Vault_Energy, 10);
      Render_Items (Vault_Inventory (Power), Vault_Power, 10);
      Render_Items (Vault_Inventory (Shell), Vault_Shell, 10);

      Base.Clear_Bucket (Vault_Helmet);
      Base.Clear_Bucket (Vault_Gauntlets);
      Base.Clear_Bucket (Vault_Chest);
      Base.Clear_Bucket (Vault_Leg);
      Base.Clear_Bucket (Vault_Class);
      Render_Items (Vault_Inventory (Helmet), Vault_Helmet, 10);
      Render_Items (Vault_Inventory (Gauntlets), Vault_Gauntlets, 10);
      Render_Items (Vault_Inventory (Chest), Vault_Chest, 10);
      Render_Items (Vault_Inventory (Leg), Vault_Leg, 10);
      Render_Items (Vault_Inventory (Class), Vault_Class, 10);

      Base.Clear_Bucket (Vault_Sparrow);
      Base.Clear_Bucket (Vault_Ship);
      Render_Items (Vault_Inventory (Sparrow), Vault_Sparrow, 10);
      Render_Items (Vault_Inventory (Ship), Vault_Ship, 10);

      Base.Clear_Bucket (Vault_Consumable);
      Base.Clear_Bucket (Vault_Modification);
      Render_Items (Vault_Inventory (Consumable), Vault_Consumable, 10);
      Render_Items (Vault_Inventory (Modification), Vault_Modification, 10);

      --  Theoretically, no items should appear here.
      Base.Clear_Bucket (Vault_Other);
      Render_Items (Vault_Inventory (Unknown), Vault_Other, 10);

      --  Complete downloads queued by Render calls
      GUI.Tasks.Global_Task.Execute (GUI.Base.Image_Callback'Access);
   end Render;

   --  Global Update_Inventory
   procedure Update_GUI is
   begin
      --  Update username
      Set_Label (Name, +Identification.Membership.Bungie_Net_User.Unique_Name);

      --  One-time setup per profile
      Setup_Transfer_Menu;
      Setup_Character_Menu;
      Render_Currencies;
   end Update_GUI;

   --  One-time label creation
   procedure Setup_Descriptions is

      function Make_Label
        (Hash : API.Definitions.Hashes
           .Destiny_Inventory_Bucket_Definition_Manifest_Hash)
         return Gtk_Label
      is

         Result : Gtk_Label;

      begin
         Gtk_New (Result, +The_Manifest.Destiny_Inventory_Buckets (Hash).Name);
         Result.Show;
         return Result;
      end Make_Label;
   begin
      --  Inventory Section
      Subclass_Description.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Subclass'Enum_Rep).Name);

      Weapon_Descriptions.Add (Make_Label (Kinetic'Enum_Rep));
      Weapon_Descriptions.Add (Make_Label (Energy'Enum_Rep));
      Weapon_Descriptions.Add (Make_Label (Power'Enum_Rep));
      Weapon_Descriptions.Add (Make_Label (Shell'Enum_Rep));
      Weapon_Descriptions.Add (Make_Label (Artefact'Enum_Rep));

      Armour_Descriptions.Add (Make_Label (Helmet'Enum_Rep));
      Armour_Descriptions.Add (Make_Label (Gauntlets'Enum_Rep));
      Armour_Descriptions.Add (Make_Label (Chest'Enum_Rep));
      Armour_Descriptions.Add (Make_Label (Leg'Enum_Rep));
      Armour_Descriptions.Add (Make_Label (Class'Enum_Rep));

      --  Equipment (still technically Inventory)
      Emblem_Sparrow_Ship_Descriptions.Add (Make_Label (Emblem'Enum_Rep));
      Emblem_Sparrow_Ship_Descriptions.Add (Make_Label (Sparrow'Enum_Rep));
      Emblem_Sparrow_Ship_Descriptions.Add (Make_Label (Ship'Enum_Rep));

      Finisher_Emote_Descriptions.Add (Make_Label (Finisher'Enum_Rep));
      Finisher_Emote_Descriptions.Add (Make_Label (Emote_Collection'Enum_Rep));
      Blank_Label.Show;
      Finisher_Emote_Descriptions.Add (Blank_Label);

      --  Vault Category Labels
      Vault_Label.Set_Text
        (+The_Manifest.Destiny_Vendors (Vault_Vendor_Hash).Name);

      Vault_Currency_Label.Set_Text
        ((+The_Manifest.Destiny_Inventory_Buckets (Glimmer'Enum_Rep).Name) &
         ", " &
         (+The_Manifest.Destiny_Inventory_Buckets (Legendary_Shards'Enum_Rep)
            .Name) &
         ", " &
         (+The_Manifest.Destiny_Inventory_Buckets (Silver'Enum_Rep).Name) &
         "…");

      Vault_Kinetic_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Kinetic'Enum_Rep).Name);
      Vault_Energy_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Energy'Enum_Rep).Name);
      Vault_Power_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Power'Enum_Rep).Name);
      Vault_Shell_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Shell'Enum_Rep).Name);

      Vault_Helmet_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Helmet'Enum_Rep).Name);
      Vault_Gauntlets_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Gauntlets'Enum_Rep).Name);
      Vault_Chest_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Chest'Enum_Rep).Name);
      Vault_Leg_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Leg'Enum_Rep).Name);
      Vault_Class_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Class'Enum_Rep).Name);

      Vault_Sparrow_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Sparrow'Enum_Rep).Name);
      Vault_Ship_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Ship'Enum_Rep).Name);

      Vault_Consumable_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Consumable'Enum_Rep).Name);
      Vault_Modification_Label.Set_Text
        (+The_Manifest.Destiny_Inventory_Buckets (Modification'Enum_Rep).Name);
   end Setup_Descriptions;

end GUI.Global;
