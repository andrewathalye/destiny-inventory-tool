pragma Ada_2022;

--  AWS
with AWS.Client;
with AWS.Client.HTTP_Utils;
with AWS.Messages; use AWS;

--  Local Packages
with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared.Debug;
with Shared.Files;
with Shared.Config;

package body API.Tasks.Synchronous_Download is
   --  INTERNAL
   procedure Debug_Delay is
   begin
      if Shared.Config.Debug_Downloads then
         delay 0.05;
      end if;
   end Debug_Delay;

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

         raise Download_Check_Failed
           with "Request failed: " & AWS.Response.Status_Code (Data)'Image;
      end if;
   end Check_Status;

   --  Synchronous download functions
   function Download
     (Path    : Unbounded_String;
      Headers : AWS.Headers.List := AWS.Headers.Empty_List;
      Caching : Boolean          := True)
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

      Client.HTTP_Utils.Send_Request
        (Connection => Connection,
         Kind       => Client.HTTP_Utils.GET,
         Result     => Data,
         URI        => +Path,
         Headers    => Headers);

      Check_Status (Data);

      if Caching then
         Shared.Files.Cache (+Path, Response.Message_Body (Data));
      end if;

      SSEA.Set (Response.Message_Body (Data));
      return SSEA;
   end Download;
end API.Tasks.Synchronous_Download;
