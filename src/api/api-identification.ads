--  AWS
with AWS.Headers;

--  Local
with API.Memberships;

package API.Identification is
   type Auth_Type is record
      Headers : AWS.Headers.List;
      Membership : API.Memberships.Membership_Type;
   end record;

   function Get_Identification (Headers : AWS.Headers.List; Membership : API.Memberships.Membership_Type) return Auth_Type is ((Headers, Membership));
   --  Returns an object suitable for calling any Bungie API endpoint (except for Authorise)
end API.Identification;
