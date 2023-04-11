private with Ada.Strings.Unbounded;

-- AWS
with AWS.Response;
with AWS.Headers;
use AWS;

package API is
	-- Constants
	Bungie_Root : constant String := "https://www.bungie.net";

	-- Types
	type Auth_Storage_Type is private;
	subtype Auth_Header_Type is AWS.Headers.List;

	-- Subprograms
	function Create_Headers (Auth_Data : Auth_Storage_Type) return Auth_Header_Type;
	function Query_Status (Data : Response.Data) return Boolean;

	-- Note: Will raise an exception if the status was not successful
	-- Use Query_Status if you wish to see if the Response was successful
	-- and handle the error state yourself
	procedure Check_Status (Data : Response.Data);
private
	use Ada.Strings.Unbounded;

	API_Root : constant String := Bungie_Root & "/Platform";
	API_Key : constant String := "ba586a1fd8f94cccb485ed6ad880fefc";

	type Auth_Storage_Type is record
		Access_Token : Unbounded_String;
		Refresh_Token : Unbounded_String;
		Membership_ID : Unbounded_String;
	end record;
end API;
