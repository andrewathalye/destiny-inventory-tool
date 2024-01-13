--  Local
with API.Identification;
with API.Manifest; use API.Manifest;

package API.Tasks.Manifest is
   function Get (Identification : API.Identification.Auth_Type) return Manifest_Type;
private
end API.Tasks.Manifest;
