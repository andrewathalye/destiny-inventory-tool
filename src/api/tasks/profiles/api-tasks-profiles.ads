with API.Profiles; use API.Profiles;
with API.Identification;

package API.Tasks.Profiles is
   function Get_Profile
     (Auth : API.Identification.Auth_Type) return Profile_Type;
end API.Tasks.Profiles;

