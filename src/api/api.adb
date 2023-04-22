pragma Ada_2022;

--  AWS
with AWS.Messages;

--  Local Packages
with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared;
with Secrets;

package body API is

   function Create_Headers
     (Auth_Data : Auth_Storage_Type) return Auth_Header_Type
   is

      List : Headers.List;

   begin
      List.Add ("Authorization", "Bearer " & (+Auth_Data.Access_Token));
      List.Add ("X-API-Key", Secrets.API_Key);
      return List;
   end Create_Headers;

   function Query_Status (Data : Response.Data) return Boolean is

      use AWS.Messages;

   begin
      return AWS.Response.Status_Code (Data) = AWS.Messages.S200;
   end Query_Status;

   procedure Check_Status (Data : Response.Data) is
   begin
      if not Query_Status (Data) then
         Debug.Put_Line (AWS.Response.Status_Code (Data)'Image);
         Headers.Debug (True);
         Headers.Debug_Print (AWS.Response.Header (Data));
         Headers.Debug (False);
         Debug.Put_Line (AWS.Response.Message_Body (Data));
         raise Program_Error with "Request failed.";
      end if;
   end Check_Status;

end API;
