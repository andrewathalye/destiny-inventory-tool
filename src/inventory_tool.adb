pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

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

   GUI_Task.Start;
end Inventory_Tool;
