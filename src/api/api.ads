package API is
   pragma Pure (API);
   --  Usage Order:
   --  API.Tasks.Authorise -> AWS.Headers.List
   --  API.Tasks.Memberships -> Membership_Type
   --  API.Identification -> Auth_Type
   --  API.Tasks.Manifest -> Manifest_Type
   --  API.Tasks.Profiles -> Profile_Type
   --  [any other packages as desired]
end API;
