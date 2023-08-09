pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

--  GNAT-specific support packages
with GNAT.Exception_Traces;
use all type GNAT.Exception_Traces.Trace_Kind;
with GNAT.Traceback.Symbolic;

--  Local Packages
with GUI.GUI_Tasks; use GUI.GUI_Tasks;

--  Alire
with Destiny_Inventory_Tool_Config;

procedure Inventory_Tool is
begin
   --  Print Welcome Message
   Put_Line
     ("Destiny Inventory Tool v" &
      Destiny_Inventory_Tool_Config.Crate_Version);

   --  Important Note:
   --  By default, exceptions raised in secondary tasks do not
   --  halt program execution or print a traceback. The below
   --  code changes this so that an unhandled exception will always
   --  print a symbolic traceback.
   --
   --  If this behaviour is changed, exception handlers must be added to
   --  all other tasks.
   GNAT.Exception_Traces.Trace_On (Unhandled_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator
     (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);

   GUI_Task.Start;
end Inventory_Tool;
