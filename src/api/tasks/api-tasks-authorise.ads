with AWS.Headers;

package API.Tasks.Authorise is
   function Do_Authorise (State : String) return AWS.Headers.List;
   --  Returns Authorization headers after performing OAuth
   --  authentication with Bungie.Net servers. To complete the
   --  process, the auth URL as per Bungie docs should be displayed
   --  to the user before calling Do_Authorise.


   function Get_URL (State : String) return String;
   --  Returns a Bungie.NET authorisation URL for use
   --  in a browser.
end API.Tasks.Authorise;
