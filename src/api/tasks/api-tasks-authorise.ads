with AWS.Headers;

private with API.Constants;

package API.Tasks.Authorise is
   function Do_Authorise
     (State : String; Client_Secret : String; API_Key : String; Client_ID : String) return AWS.Headers.List;
   --  Returns Authorization headers after performing OAuth
   --  authentication with Bungie.Net servers. To complete the
   --  process, the auth URL as per Bungie docs should be displayed
   --  to the user before calling Do_Authorise.
private
   use API.Constants;

   OAuth_Authorise_Endpoint : constant String :=
     Bungie_Root & "/en/oauth/authorize";

   OAuth_Token_Endpoint : constant String :=
     Bungie_Root & "/platform/app/oauth/token/";
end API.Tasks.Authorise;
