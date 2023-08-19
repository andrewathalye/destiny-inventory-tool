--  Child packages are used for various persistent UI elements
with Gtkada.Builder; use Gtkada.Builder;

package GUI.Elements is
   pragma Elaborate_Body (GUI.Elements);
   --  Builder is initialised during elaboration of GUI.Elements.
   --  Errors which occur at this stage will not provide a meaningful
   --  traceback, but messages will be intact.

   function Builder return Gtkada_Builder with
     Inline;
   --  The builder used by the whole application.
   --  Left unconnected, run Do_Connect after initialising Gtk from the GUI task
end GUI.Elements;
