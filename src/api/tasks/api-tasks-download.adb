pragma Ada_2022;

with Ada.Containers;
use type Ada.Containers.Count_Type;

--  Local Packages
with API.Tasks.Synchronous_Download;

package body API.Tasks.Download is
   task body Download_Task is
      --  Types
      type Download_Queue_Entry is record
         Path     : Unbounded_String;
         Opaque   : Opaque_Access;
         Headers  : AWS.Headers.List;
         Complete : Boolean := False;
      end record;

      --  Instantiation
      package DQV is new Ada.Containers.Vectors
        (Natural, Download_Queue_Entry);
      subtype Download_Queue_Type is DQV.Vector;

      --  Download cache
      Download_Queue : Download_Queue_Type;
      Download_Cache : Download_Cache_Type;

      --  Local Data
      Callback_L : Download_Callback;
   begin
      loop
         select
            --  Does nothing since there was no process ongoing
            accept Interrupt;
         or
            --  Enqueue download
            accept Download
              (Path    : Unbounded_String;
               Opaque  : Opaque_Access;
               Headers : AWS.Headers.List := AWS.Headers.Empty_List)
            do
               --  Acquire a possibly thread-local reference to Opaque
               --  This reference should be removed by the callback once
               --  the download is complete
               Reference (Opaque_Type (Opaque.all)'Access);

               Download_Queue.Append
                 (Download_Queue_Entry'(Path, Opaque, Headers, others => <>));
            end Download;
         or
            --  Execute download and perform callback
            accept Execute (Callback : Download_Callback) do
               Callback_L := Callback;
            end Execute;

            --  This loop batches the callbacks to reduce the impact on GUI task performance.
            --  Each callback pauses the GUI task to update thread-unsafe data structures.
            Execute_Loop :
               for DQE of Download_Queue loop
                  select
                     --  Interrupt the download
                     accept Interrupt;
                     exit Execute_Loop;
                  else
                     --  Prevents attempting to download the same file multiple times if interrupted
                     if not DQE.Complete then
                        Download_Cache.Append
                          (Download_Cache_Entry'
                             (Path   => DQE.Path,
                              Opaque => DQE.Opaque,
                              Data   =>
                                API.Tasks.Synchronous_Download.Download
                                  (DQE.Path, DQE.Headers)));

                        DQE.Complete := True;
                     end if;
                  end select;

                  --  Batch clear 5 elements from the queue
                  if Download_Cache.Length = 5 then
                     Callback_L (Download_Cache);
                  end if;
               end loop Execute_Loop;
            Download_Queue.Clear;

            Callback_L
              (Download_Cache); --  Handle any remaining items in the download cache
         or
            terminate;
         end select;
      end loop;
   end Download_Task;
end API.Tasks.Download;
