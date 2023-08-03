pragma Ada_2022;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers;
use type Ada.Containers.Count_Type;

--  AWS
with AWS.Client;
with AWS.Client.HTTP_Utils;
with AWS.Headers;
with AWS.Messages; use AWS;

--  Local Packages
with Secrets;        use Secrets;
with Shared.Strings; use Shared.Strings;
with Shared.Debug;
with Shared.Files;   use Shared;

package body Tasks.Download is
   --  Debugging
   Simulate_Slow : constant Boolean := False;

   --  Check the status of a request
   --  Raises an exception on failure
   procedure Check_Status (Data : AWS.Response.Data) is
   begin
      if AWS.Response.Status_Code (Data) not in AWS.Messages.Success then
         Debug.Put_Line (AWS.Response.Status_Code (Data)'Image);
         AWS.Headers.Debug (True);
         AWS.Headers.Debug_Print (AWS.Response.Header (Data));
         AWS.Headers.Debug (False);
         Debug.Put_Line (AWS.Response.Message_Body (Data));

         raise Program_Error with "Request failed.";
      end if;
   end Check_Status;

   --  Synchronous download functions
   function Download
     (Path       : Unbounded_String;
      Needs_Auth : Boolean := False;
      Caching    : Boolean := True)
      return Stream_Element_Array
   is

      Connection : Client.HTTP_Connection;
      Data       : Response.Data;

   begin
      --  Debug.Put_Line ("Start Download " & (+Path));
      if Simulate_Slow then
         delay 0.05;
      end if;

      --  Note: We avoid using Client.Get because it results in a stack
      --  overflow on musl libc
      if Caching and then Files.Has_Cached (+Path) then
         return Files.Get_Cached (+Path);

      else
         Debug.Put_Line ("Download " & (+Path));
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
            Files.Cache (+Path, Response.Message_Body (Data));
         end if;
         return Response.Message_Body (Data);
      end if;
   end Download;

   --  Note: No caching is available for this version
   function Download
     (Path : Unbounded_String; Needs_Auth : Boolean := False) return String
   is
      Connection : Client.HTTP_Connection;
      Data       : Response.Data;
   begin
      if Simulate_Slow then
         delay 0.05;
      end if;

      --  Note: We avoid using Client.Get because it results in a stack
      --  overflow on musl libc

      Debug.Put_Line ("Download as string " & (+Path));
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
                           SEA_A : constant Stream_Element_Array_Access :=
                             new Stream_Element_Array'
                               (Download (DQE.Path, DQE.Needs_Auth));
                        begin
                           Download_Cache.Append
                             (Download_Cache_Entry'
                                (DQE.Path, DQE.Widget, SEA_A));
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
   exception
      when E : others =>
         Put_Line (Standard_Error, Exception_Information (E));
         Reraise_Occurrence (E);
   end Download_Task;
end Tasks.Download;
