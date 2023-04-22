private with Ada.Strings.Unbounded;

-- AWS
with AWS.Response;
with AWS.Headers; use AWS;

package API is
   -- Constants
   Bungie_Root : constant String := "https://www.bungie.net";

   -- Types
   type Auth_Storage_Type is private;

   subtype Auth_Header_Type is AWS.Headers.List;

   -- Subprograms
   function Create_Headers
     (Auth_Data : Auth_Storage_Type) return Auth_Header_Type;
   function Query_Status (Data : Response.Data) return Boolean;

   -- Note: Will raise an exception if the status was not successful
   -- Use Query_Status if you wish to see if the Response was successful
   -- and handle the error state yourself
   procedure Check_Status (Data : Response.Data);
private
   -- Use cached versions of API data files to reduce API calls
   -- This does not currently apply to API.Authorise
   Debug_Caching : Boolean := True;

   use Ada.Strings.Unbounded;

   API_Root : constant String := "/Platform";

   type Auth_Storage_Type is record
      Access_Token  : Unbounded_String;
      Refresh_Token : Unbounded_String;
      Membership_ID : Unbounded_String;
   end record;

   Client_Secret : constant String :=
     "bybYT7bJTHfk-IHXNcfSUn54fk8aQXA3tXYaSL71.zs";
   API_Key : constant String := "ba586a1fd8f94cccb485ed6ad880fefc";
end API;
