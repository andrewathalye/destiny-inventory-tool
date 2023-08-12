--  Child packages are used for various persistent UI elements
with Gtkada.Builder; use Gtkada.Builder;

with GUI.Make_Builder;

package GUI.Elements is
   --  Variables
   Builder : constant Gtkada_Builder;
   --  Left unconnected, run Do_Connect after initialising Gtk
private
   Name    : constant String         := "res/gui.glade";
   Builder : constant Gtkada_Builder := GUI.Make_Builder (Name);
end GUI.Elements;
