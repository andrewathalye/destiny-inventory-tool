package Shared.Config is
   pragma Pure (Shared.Config);

   --  API Config
   Debug_Print     : constant Boolean := True;
   Debug_API       : constant Boolean := True;
   Debug_Downloads : constant Boolean := False;

   --  GUI Config
   Builder_File_Name : constant String := "res/gui.glade";
end Shared.Config;
