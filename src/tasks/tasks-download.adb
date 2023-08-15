pragma Ada_2022;
with Ada.Containers;
use type Ada.Containers.Count_Type;

--  AWS
with AWS.Client;
with AWS.Client.HTTP_Utils;
with AWS.Headers;
with AWS.Messages; use AWS;

--  Local Packages
with Secrets; use Secrets;

with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared.Debug;
with Shared.Files;
with Shared.Config;

package body Tasks.Download is
   --  Check the status of a request
   --  Raises an exception on failure
   procedure Check_Status (Data : AWS.Response.Data) is
   begin
      if AWS.Response.Status_Code (Data) not in AWS.Messages.Success then
         Put_Line (AWS.Response.Status_Code (Data)'Image);
         AWS.Headers.Debug (True);
         AWS.Headers.Debug_Print (AWS.Response.Header (Data));
         AWS.Headers.Debug (False);
         Put_Line (AWS.Response.Message_Body (Data));

         raise Program_Error with "Request failed.";
      end if;
   end Check_Status;

   procedure Debug_Delay is
   begin
      if Shared.Config.Debug_Downloads then
         delay 0.05;
      end if;
   end Debug_Delay;

   --  Synchronous download functions
   function Download
     (Path       : Unbounded_String;
      Needs_Auth : Boolean := False;
      Caching    : Boolean := True)
      return Stream_Element_Array is
     (Shared_Stream_Element_Array'(Download (Path, Needs_Auth, Caching)).Get);

   function Download
     (Path       : Unbounded_String;
      Needs_Auth : Boolean := False;
      Caching    : Boolean := True)
      return Shared_Stream_Element_Array
   is

      Connection : Client.HTTP_Connection;
      Data       : Response.Data;

      SSEA : Shared_Stream_Element_Array;

   begin
      Debug_Delay;

      --  Note: We avoid using Client.Get because it results in a stack
      --  overflow on musl libc
      if Caching and then Shared.Files.Has_Cached (+Path) then
         return Shared.Files.Get_Cached (+Path);
      end if;
      Put_Line ("Download " & (+Path));
      Client.Create (Connection, +Path);

      if Needs_Auth then
         Client.HTTP_Utils.Send_Request
           (Connection => Connection,
            Kind       => Client.HTTP_Utils.GET,
            Result     => Data,
            URI        => +Path,
            Headers    => Secrets.Headers);

      else
         Client.HTTP_Utils.Send_Request
           (Connection, Client.HTTP_Utils.GET, Data, +Path);
      end if;

      Check_Status (Data);

      if Caching then
         Shared.Files.Cache (+Path, Response.Message_Body (Data));
      end if;

      SSEA.Set (Response.Message_Body (Data));
      return SSEA;
   end Download;

   --  Note: No caching is available for this version
   function Download
     (Path : Unbounded_String; Needs_Auth : Boolean := False) return String
   is
      Connection : Client.HTTP_Connection;
      Data       : Response.Data;
   begin
      Debug_Delay;

      --  Note: We avoid using Client.Get because it results in a stack
      --  overflow on musl libc

      Put_Line ("Download as string " & (+Path));
      Client.Create (Connection, +Path);

      if Needs_Auth then
         Client.HTTP_Utils.Send_Request
           (Connection => Connection,
            Kind       => Client.HTTP_Utils.GET,
            Result     => Data,
            URI        => +Path,
            Headers    => Secrets.Headers);
      else
         Client.HTTP_Utils.Send_Request
           (Connection, Client.HTTP_Utils.GET, Data, +Path);
      end if;
      Check_Status (Data);

      return Response.Message_Body (Data);
   end Download;

   task body Download_Task is
      --  Types
      type Download_Queue_Entry is record
         Path       : Unbounded_String;
         Widget     : Gtk_Widget;
         Needs_Auth : Boolean;
         Complete   : Boolean := False;
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
              (Path       : Unbounded_String;
               Widget     : Gtk_Widget;
               Needs_Auth : Boolean := False)
            do
               Widget.Ref; -- Keep a reference so the Widget is not invalidated
               --  Note this is not automatic because we're in a separate
               --  thread
               Download_Queue.Append
                 (Download_Queue_Entry'
                    (Path, Widget, Needs_Auth, others => <>));
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
                        declare
                           SSEA : Shared_Stream_Element_Array;
                        begin
                           SSEA.Set (Download (DQE.Path, DQE.Needs_Auth));

                           Download_Cache.Append
                             (Download_Cache_Entry'
                                (DQE.Path, DQE.Widget, SSEA));
                        end;

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
end Tasks.Download;
