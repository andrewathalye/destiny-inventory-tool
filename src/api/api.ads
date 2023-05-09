private with Ada.Strings.Unbounded;

--  AWS
with AWS.Headers; use AWS;

package API is
   --  Constants
   Bungie_Root : constant String := "https://www.bungie.net";

   --  Types
   type Auth_Storage_Type is private;
   subtype Auth_Header_Type is AWS.Headers.List;

   --  Subprograms
   function Create_Headers
     (Auth_Data : Auth_Storage_Type) return Auth_Header_Type;

private
   use Ada.Strings.Unbounded;
   API_Root : constant String := Bungie_Root & "/Platform";

   type Auth_Storage_Type is record
      Access_Token  : Unbounded_String;
      Refresh_Token : Unbounded_String;
      Membership_ID : Unbounded_String;
   end record;
end API;
