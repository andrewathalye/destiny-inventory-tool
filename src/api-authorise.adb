pragma Ada_2022;

with Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Numerics.Discrete_Random;
with Ada.Directories; use Ada.Directories;
use Ada;

with Interfaces; use Interfaces;

-- AWS
with AWS.Client;
with AWS.Server;
with AWS.Response;
with AWS.Status; use AWS.Status;
with AWS.Messages;

-- Local Packages
with JSON; use JSON;
with Shared; use Shared;

package body API.Authorise is
	
	-- Global Variables
	function Generate_State return String is
		package Unsigned_64_Random is new Ada.Numerics.Discrete_Random (Unsigned_64);
		G : Unsigned_64_Random.Generator;
	begin
		Unsigned_64_Random.Reset (G);

		declare
			Output : constant String := Unsigned_64_Random.Random (G)'Image;
		begin
			return Output (Output'First + 1 .. Output'Last);
		end;
	end Generate_State;

	State : constant String := Generate_State;

	-- Subprograms
	protected Authorise_Object is
		procedure Set_Code (Input : String);
		entry Get_Code (Result : out Unbounded_String);
		procedure Reset;
	private
		Code : Unbounded_String;
		Code_Received : Boolean;
	end Authorise_Object;

	protected body Authorise_Object is
		procedure Set_Code (Input : String) is
		begin
			Code := To_Unbounded_String (Input);
			Code_Received := True;
		end Set_Code;

		entry Get_Code (Result : out Unbounded_String)
			when Code_Received
		is begin
			Result := Code;
		end Get_Code;

		procedure Reset is
		begin
			Code_Received := False;
		end Reset;
	end Authorise_Object;

	function Code_Callback (Request : Status.Data) return Response.Data
	is begin
		if Status.Method (Request) = Status.GET then
			-- Reject invalid states
			if Parameter (
				Request,
				Name => "state") /= State
			then
				return Response.Acknowledge (Messages.S400, "Invalid state received. Please try again.");
			end if;

			Authorise_Object.Set_Code (
				Parameter (
					Request,
					Name => "code"));

			return Response.Acknowledge (Messages.S200, "Code received, you may close this tab.");
		else
			return Response.Acknowledge (Messages.S400, "Unsupported method.");
		end if;
	end Code_Callback;

	function Get_Code return Unbounded_String
	is
		WS : Server.HTTP;
		Code : Unbounded_String;
	begin
		Server.Start (
			WS,
			Name => "Code Callback",
			Callback => Code_Callback'Access,
			Max_Connection => 1,
			Port => 8888,
			Security => True); -- Enable HTTPS

		Server.Set_Security (WS, "dat/cert.pem");

		Authorise_Object.Get_Code (Code);
		delay 1.0; -- Wait for response to be served
		Server.Shutdown (WS);

		Authorise_Object.Reset;

		return Code;
	end Get_Code;

	-- Parse JSON
	function Parse_JSON (Message_Body : Unbounded_String) return Auth_Storage_Type is
		List : constant Unbounded_String_List := Get_Strings (
			Message_Body,
			[To_Unbounded_String ("access_token"),
			To_Unbounded_String ("refresh_token"),
			To_Unbounded_String ("membership_id")]);
	begin
		return (
			List.Element (To_Unbounded_String ("access_token")),
			List.Element (To_Unbounded_String ("refresh_token")),
			List.Element (To_Unbounded_String ("membership_id")));
	end Parse_JSON;

	function Get_Token (Refresh_Token : Unbounded_String) return Auth_Storage_Type
	is
		Data : Response.Data;
	begin
		Put_Debug ("Refresh token");
		Data := Client.Post (
			URL => OAuth_Token_Endpoint,
			Data =>
				"grant_type=refresh_token"
				& "&refresh_token=" & To_String (Refresh_Token)
				& "&client_id=" & Client_ID
				& "&client_secret=" & Client_Secret,
			Content_Type => "application/x-www-form-urlencoded");
--		Put_Debug (Response.Message_Body (Data));

		Check_Status (Data);

		return Parse_JSON (Response.Message_Body (Data));
	end Get_Token;

	function Get_Initial_Token (Code : Unbounded_String) return Auth_Storage_Type
	is
		Data : Response.Data;
	begin
		Put_Debug ("Initial token");
		Data := Client.Post (
			URL => OAuth_Token_Endpoint,
			Data =>
				"grant_type=authorization_code"
				& "&code=" & To_String (Code)
				& "&client_id=" & Client_ID
				& "&client_secret=" & Client_Secret,
			Content_Type => "application/x-www-form-urlencoded");
--		Put_Debug (Response.Message_Body (Data));

		Check_Status (Data);

		return Parse_JSON (Response.Message_Body (Data));
	end Get_Initial_Token;

	function Do_Authorise return Auth_Storage_Type is
		Auth_Storage : Auth_Storage_Type;
	begin
		if Exists ("dat/refresh.dat") then -- Load token
			Put_Debug ("Load token");
			-- Load refresh token
			declare
				SF : Stream_IO.File_Type;
				S : Stream_IO.Stream_Access;
				Refresh_Token : Unbounded_String;
			begin
				Stream_IO.Open (
					SF,
					Mode => Stream_IO.In_File,
					Name => "dat/refresh.dat");
				S := Stream_IO.Stream (SF);
				Unbounded_String'Read (S, Refresh_Token);
				Stream_IO.Close (SF);

				-- Refresh both tokens
				Auth_Storage := Get_Token (Refresh_Token);
			end;
		else -- Fetch token
			Put_Debug ("Fetch token");
			-- Get Authorisation Code
			Text_IO.Put_Line ("Open the following URL in a browser: "
				& OAuth_Authorise_Endpoint
				& "?client_id=" & Client_ID
				& "&response_type=code"
				& "&state=" & State);
			Text_IO.Put_Line ("Please note that you may need to accept an invalid certificate for localhost");

			-- Get Access Token and Refresh Token
			Auth_Storage := Get_Initial_Token (Get_Code);
		end if;

		-- Save refresh token
		Put_Debug ("Save refresh token");
		Save_Refresh_Token : declare
			SF : Stream_IO.File_Type;
			S : Stream_IO.Stream_Access;
		begin
			if Exists ("dat/refresh.dat") then
				Stream_IO.Open (
					SF,
					Mode => Stream_IO.Out_File,
					Name => "dat/refresh.dat");
			else
				Stream_IO.Create (
					SF,
					Name => "dat/refresh.dat");
			end if;

			S := Stream_IO.Stream (SF);
			Unbounded_String'Write (S, Auth_Storage.Refresh_Token);
			Stream_IO.Close (SF);
		end Save_Refresh_Token;

		return Auth_Storage;
	end Do_Authorise;
end API.Authorise;
