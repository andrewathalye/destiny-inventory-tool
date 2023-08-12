with Ada.Text_IO;

with Shared.Config;

package body Shared.Debug is

   procedure Put_Line (Item : String) is
   begin
      if Shared.Config.Debug_Print then
         Ada.Text_IO.Put_Line ("[Debug] " & Item);
      end if;
   end Put_Line;

end Shared.Debug;
