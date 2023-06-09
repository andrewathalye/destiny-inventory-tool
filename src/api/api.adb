pragma Ada_2022;

--  Local Packages
with Shared.Strings; use Shared.Strings;
with Constant_Secrets;

package body API is

   function Create_Headers
     (Auth_Data : Auth_Storage_Type) return Auth_Header_Type
   is

      List : Headers.List;

   begin
      List.Add ("Authorization", "Bearer " & (+Auth_Data.Access_Token));
      List.Add ("X-API-Key", Constant_Secrets.API_Key);
      return List;
   end Create_Headers;
end API;
