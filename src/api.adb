pragma Ada_2022;

-- AWS
with AWS.Messages;
use AWS;

-- Local Packages
with Shared; use Shared;

package body API is
	function Create_Headers (Auth_Data : Auth_Storage_Type) return Auth_Header_Type is
		List : Headers.List;
	begin
		List.Add ("Authorization", "Bearer " & (+Auth_Data.Access_Token));
		List.Add ("X-API-Key", API_Key);

		return List;
	end Create_Headers;

	procedure Check_Status (Data : Response.Data) is
		use AWS.Messages;	
	begin
		if AWS.Response.Status_Code (Data) /= AWS.Messages.S200 then
			Put_Debug (AWS.Response.Status_Code (Data)'Image);
			Headers.Debug (True);
			Headers.Debug_Print (AWS.Response.Header (Data));
			Put_Debug (AWS.Response.Message_Body (Data));
			raise Program_Error with "Request failed.";
		end if;
	end Check_Status;
end API;
