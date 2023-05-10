--  Local Packages
with API.Inventories.Global; use API;

package GUI.Global is
   --  State
   Inventory : Inventories.Global.Global_Inventory_Type;

   --  Subprograms
   --  Status Updates
   procedure Update_GUI; -- Performs a one-time update of the UI labels for a given profile
   procedure Setup_Descriptions; -- Performs a one-time global update of UI translation labels.

   --  Render assumes that the GUI is unlocked and returns it to that state
   --  after its critical section. Use Locked_Wrapper if calling from a locked
   --  context.
   procedure Render;
end GUI.Global;
