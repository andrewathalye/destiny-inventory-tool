--  AWS
with AWS.Headers;

--  Local Packages
with API.Memberships; use API.Memberships;

package API.Tasks.Memberships is
   function Get
     (Headers : AWS.Headers.List) return Membership_Type;
   --  Get a Membership_Type using authentication headers from Headers
   --  In most cases, the next step is to combine Headers and Membership using
   --  API.Identification to obtain an Auth_Type record, which is accepted
   --  by all other endpoints for identification

end API.Tasks.Memberships;
