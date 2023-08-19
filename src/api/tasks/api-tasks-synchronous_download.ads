with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  AWS
with AWS.Response;
with AWS.Headers;

--  Local Packages
with Shared.Streams; use Shared.Streams;

package API.Tasks.Synchronous_Download is
   -----------------------------
   -- Synchronous Subprograms --
   -----------------------------
   function Download
     (Path    : Unbounded_String;
      Headers : AWS.Headers.List := AWS.Headers.Empty_List;
      Caching : Boolean          := True)
      return Shared_Stream_Element_Array;
   --  Download Path using authentication and caching if requested.
   --  Returns a shared reference to a Stream_Element_Array

   ------------------
   -- Verification --
   ------------------
   Download_Check_Failed : exception;
   procedure Check_Status (Data : AWS.Response.Data);
   --  Manually check the result of an AWS download. Not needed for downloads completed through
   --  this package.
   --  Note: Raises Download_Check_Failed if the download result was not 200 OK

end API.Tasks.Synchronous_Download;
