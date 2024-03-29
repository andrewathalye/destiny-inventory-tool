--  Gtkada
with Gtkada.Builder; use Gtkada.Builder;

--  Gtk
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget;   use Gtk.Widget;

--  Local Packages
with API.Profiles;
with API.Manifest.Tools; use API;

package GUI.Handlers is
   --  Instantiations
   package Widget_Callback is new Callback (Gtk_Widget_Record);

   --  Global Handlers
   procedure Set_Handlers (Builder : Gtkada_Builder);

   --  Dynamic Handlers
   procedure Character_Menu_Button_Clicked_Handler
     (Button    : access Gtk_Widget_Record'Class;
      User_Data : API.Profiles.Character_Range);
   procedure Transfer_Handler
     (Button : access Gtk_Widget_Record'Class;
      Target : Profiles.Character_Type);
   procedure Vault_Handler (Button : access Gtk_Widget_Record'Class);

   --  Standard item button handler - shows item descriptions
   procedure Item_Button_Handler
     (Widget    : access Gtk_Widget_Record'Class;
      User_Data : Manifest.Tools.Item_Description);

   --  Item button handler which displays only information relevant to
   --  a socket plug.
   procedure Socket_Item_Button_Handler
     (Widget    : access Gtk_Widget_Record'Class;
      User_Data : Manifest.Tools.Item_Description);
end GUI.Handlers;
