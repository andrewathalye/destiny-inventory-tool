--  AWS
with AWS.Headers;

--  Local
with API.Manifest; use API.Manifest;

package API.Tasks.Manifest is
   function Get (Headers : AWS.Headers.List) return Manifest_Type;
private
   Current_Manifest_Format_Version : constant := 7;
end API.Tasks.Manifest;
