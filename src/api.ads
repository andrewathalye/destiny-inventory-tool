private with Ada.Strings.Unbounded;

with Interfaces; use Interfaces;

-- AWS
private with AWS.Response;
with AWS.Headers;

package API is
	-- Constants
	Bungie_Root : constant String := "https://www.bungie.net";

	-- Types
	type Auth_Storage_Type is private;
	subtype Auth_Header_Type is AWS.Headers.List;

	-- Nullable Types
	type Nullable_Integer_64 (Empty : Boolean := True) is record
		case Empty is
			when True =>
				null;
			when False =>
				Value : Integer_64;
		end case;
	end record;

	-- Subprograms
	function Create_Headers (Auth_Data : Auth_Storage_Type) return Auth_Header_Type;
private
	use Ada.Strings.Unbounded;
	use AWS;

	procedure Check_Status (Data : Response.Data);

	API_Root : constant String := Bungie_Root & "/Platform";
	API_Key : constant String := "ba586a1fd8f94cccb485ed6ad880fefc";

	type Auth_Storage_Type is record
		Access_Token : Unbounded_String;
		Refresh_Token : Unbounded_String;
		Membership_ID : Unbounded_String;
	end record;
end API;
