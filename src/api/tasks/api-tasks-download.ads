with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  AWS
with AWS.Headers;

--  Local Packages
with Shared.Streams; use Shared.Streams;

generic
   type Opaque_Type (<>) is tagged private;
   type Opaque_Access is access all Opaque_Type'Class;
   with procedure Reference (Item : Opaque_Access);
   --  Reference will be called once upon the Opaque item being received
   --  by the Task, and the item should be unreferenced as necessary by
   --  the download callback
package API.Tasks.Download is
   --------------------------
   -- DOWNLOAD_CACHE_ENTRY --
   --------------------------
   type Download_Cache_Entry is record
      Path   : Unbounded_String;
      Opaque : Opaque_Access;
      Data   : Shared_Stream_Element_Array;
   end record;

   package DCV is new Ada.Containers.Vectors (Natural, Download_Cache_Entry);
   subtype Download_Cache_Type is DCV.Vector;

   -----------------------
   -- DOWNLOAD_CALLBACK --
   -----------------------
   type Download_Callback is
     access procedure (Cache : in out Download_Cache_Type);
   --  Procedure to be called once the download queue is exhausted
   --
   --  The callee should finalise Opaque as necessary
   --  as well as clear the cache once it is done.
   --  No guarantees are made about how many items will
   --  be returned in the Cache at a given time, so the
   --  callee should be prepared to handle an arbitrary
   --  number.

   -------------------
   -- DOWNLOAD_TASK --
   -------------------
   task type Download_Task is
      entry Download
        (Path    : Unbounded_String;
         Opaque  : Opaque_Access;
         Headers : AWS.Headers.List := AWS.Headers.Empty_List);
      entry Execute (Callback : Download_Callback);
      entry Interrupt;
   end Download_Task;
   --  Task responsible for executing downloads
   --  An opaque value must be provided as well.
   --  This will be ignored by the task and returned via the callback
   --  as part of a Download_Cache_Entry.
end API.Tasks.Download;
