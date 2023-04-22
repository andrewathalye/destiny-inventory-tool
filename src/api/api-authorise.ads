package API.Authorise is
   --  Constants
   OAuth_Authorise_Endpoint : constant String :=
     "https://www.bungie.net/en/oauth/authorize";

   --  Subprograms
   function Do_Authorise (State : String) return Auth_Storage_Type;
private
   --  Constants
   OAuth_Token_Endpoint : constant String :=
     "https://www.bungie.net/platform/app/oauth/token/";
end API.Authorise;
