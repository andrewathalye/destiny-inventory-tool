with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  AWS
with AWS.Response;

--  Gtkada
with Gtk.Widget; use Gtk.Widget;

package Tasks.Download is
   --  Types
   type Download_Callback is
     access procedure
       (File_Name : Unbounded_String;
        Widget    : Gtk_Widget;
        Data      : Stream_Element_Array);
   --  Task

   task type Download_Task is
      entry Download
        (Path       : Unbounded_String;
         Widget     : Gtk_Widget;
         Needs_Auth : Boolean := False);
      entry Execute (Callback : Download_Callback);
      entry Interrupt;

   end Download_Task;
   Contents_Task  : Download_Task;
   Character_Task : Download_Task;
   Global_Task    : Download_Task;

   --  Synchronous versions of the above task. Provided for convenience.
   function Download
     (Path       : Unbounded_String;
      Needs_Auth : Boolean := False;
      Caching    : Boolean := True)
      return Stream_Element_Array;
   function Download --  Note: Uncached
     (Path : Unbounded_String; Needs_Auth : Boolean := False) return String;

   --  Provided for convenience. Download already checks the status of downloads it manages.
   --  Note: Will raise an exception if the status was not successful.
   procedure Check_Status (Data : AWS.Response.Data);
end Tasks.Download;
