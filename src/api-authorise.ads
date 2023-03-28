package API.Authorise is
	-- Subprograms
	function Do_Authorise return Auth_Storage_Type;
private
	-- Constants
	OAuth_Authorise_Endpoint : constant String := "https://www.bungie.net/en/oauth/authorize";
	OAuth_Token_Endpoint : constant String := "https://www.bungie.net/platform/app/oauth/token/";
	Client_ID : constant String := "43557";
	Client_Secret : constant String := "bybYT7bJTHfk-IHXNcfSUn54fk8aQXA3tXYaSL71.zs";
end API.Authorise;
