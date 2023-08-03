with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

--  AWS
with AWS.Response;

--  Gtkada
with Gtk.Widget; use Gtk.Widget;

--  Local Packages
with Shared.Streams; use Shared.Streams;

package Tasks.Download is
   --  Types
   type Download_Cache_Entry is record
      Path   : Unbounded_String;
      Widget : Gtk_Widget;
      Data   : Stream_Element_Array_Access;
   end record;
   package DCV is new Ada.Containers.Vectors (Natural, Download_Cache_Entry);
   subtype Download_Cache_Type is DCV.Vector;

   --  The callee should free Data / Widgets if needed
   --  as well as clear the cache once it is done.
   --  No guarantees are made about how many items will
   --  be returned in the Cache at a given time, so the
   --  callee should be prepared to handle an arbitrary
   --  number.
   type Download_Callback is
     access procedure (Cache : in out Download_Cache_Type);

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
