--  Gtk
with Gtk.Grid;    use Gtk.Grid;
with Gtk.Box;     use Gtk.Box;
with Gtk.Label;   use Gtk.Label;
with Gtk.Popover; use Gtk.Popover;
with Gtk.Button;  use Gtk.Button;

package GUI.Elements.Global is
   --  Character Menu
   Character_Grid : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("character_grid"));

   Emblem_Button : constant Gtk_Button :=
     Gtk_Button (Builder.Get_Object ("emblem_button"));

   Character_Menu : constant Gtk_Popover :=
     Gtk_Popover (Builder.Get_Object ("character_menu"));

   Name : constant Gtk_Label := Gtk_Label (Builder.Get_Object ("name"));

   --  Transfer
   Transfer_Grid : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("transfer_grid"));

   --  Special Buckets
   Vault_Currency : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_currency"));

   --  Buckets
   Vault_Kinetic : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_kinetic"));
   Vault_Energy : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_energy"));
   Vault_Power : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_power"));
   Vault_Shell : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_shell"));
   Vault_Helmet : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_helmet"));
   Vault_Gauntlets : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_gauntlets"));
   Vault_Chest : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_chest"));
   Vault_Leg : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_leg"));
   Vault_Class : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_class"));
   Vault_Sparrow : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_sparrow"));
   Vault_Ship : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_ship"));
   Vault_Consumable : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_consumable"));
   Vault_Modification : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_modification"));
   Vault_Other : constant Gtk_Grid :=
     Gtk_Grid (Builder.Get_Object ("vault_other"));

   --  Inventory Section Labels
   Subclass_Description : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("subclass_description"));

   Weapon_Descriptions : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("weapon_descriptions"));
   Armour_Descriptions : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("armour_descriptions"));

   Emblem_Sparrow_Ship_Descriptions : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("emblem_sparrow_ship_descriptions"));
   Finisher_Emote_Descriptions : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("finisher_emote_descriptions"));

   Blank_Label : constant Gtk_Label := Gtk_Label_New;

   --  Vault Label
   Vault_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_label"));

   --  Expander Labels
   Vault_Currency_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_currency_label"));

   Vault_Kinetic_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_kinetic_label"));
   Vault_Energy_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_energy_label"));
   Vault_Power_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_power_label"));
   Vault_Shell_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_shell_label"));

   Vault_Helmet_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_helmet_label"));
   Vault_Gauntlets_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_gauntlets_label"));
   Vault_Chest_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_chest_label"));
   Vault_Leg_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_leg_label"));
   Vault_Class_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_class_label"));

   Vault_Sparrow_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_sparrow_label"));
   Vault_Ship_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_ship_label"));
   Vault_Consumable_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_consumable_label"));
   Vault_Modification_Label : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("vault_modification_label"));
end GUI.Elements.Global;
