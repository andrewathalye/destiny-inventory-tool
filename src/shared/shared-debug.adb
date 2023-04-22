with Ada.Text_IO;

package body Shared.Debug is

   procedure Put_Line (Item : String) is
   begin
      Ada.Text_IO.Put_Line ("[Debug] " & Item);
   end Put_Line;

end Shared.Debug;
