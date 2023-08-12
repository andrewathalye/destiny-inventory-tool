--  Gtk
with Gtk.Search_Entry;   use Gtk.Search_Entry;
with Gtk.Popover;        use Gtk.Popover;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Label;          use Gtk.Label;
with Gtk.Box;            use Gtk.Box;
with Gtk.Widget;         use Gtk.Widget;

package GUI.Elements.Handlers is
   Character_Menu : constant Gtk_Popover :=
     Gtk_Popover (Builder.Get_Object ("character_menu"));
   Search : constant Gtk_Search_Entry :=
     Gtk_Search_Entry (Builder.Get_Object ("search"));
   Error_Dialog : constant Gtk_Message_Dialog :=
     Gtk_Message_Dialog (Builder.Get_Object ("error_dialog"));
   Vault_Button : constant Gtk_Widget :=
     Gtk_Widget (Builder.Get_Object ("vault_button"));
   Item_Details : constant Gtk_Popover :=
     Gtk_Popover (Builder.Get_Object ("item_details"));
   Transfer_Menu : constant Gtk_Popover :=
     Gtk_Popover (Builder.Get_Object ("transfer_menu"));
   Vault_Menu : constant Gtk_Popover :=
     Gtk_Popover (Builder.Get_Object ("vault_menu"));
   Equip_Menu : constant Gtk_Popover :=
     Gtk_Popover (Builder.Get_Object ("equip_menu"));
   Socket_Name_Type_Box : constant Gtk_Box :=
     Gtk_Box (Builder.Get_Object ("socket_name_type_box"));
   Socket_Popover : constant Gtk_Popover :=
     Gtk_Popover (Builder.Get_Object ("socket_popover"));
   Socket_Name : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("socket_name"));
   Socket_Type_And_Tier_Display_Name : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("socket_type_and_tier_display_name"));
   Socket_Description : constant Gtk_Label :=
     Gtk_Label (Builder.Get_Object ("socket_description"));

end GUI.Elements.Handlers;
