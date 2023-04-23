pragma Ada_2022;
with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
use Ada;

--  AWS
with AWS.Client;
with AWS.Server;
with AWS.Response;
with AWS.Status; use AWS.Status;
with AWS.Messages;
with AWS.Net;

--  GNAT
with GNAT.OS_Lib;

--  GNATCOLL
with GNATCOLL.JSON; use GNATCOLL.JSON;

--  Local Packages
with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared;
with Secrets;

package body API.Authorise is

   Local_State : Unbounded_String;

   protected Authorise_Object is
      procedure Set_Code (Input : String);
      entry Get_Code (Result : out Unbounded_String);
      procedure Reset;

   private
      Code          : Unbounded_String;
      Code_Received : Boolean;

   end Authorise_Object;

   protected body Authorise_Object is

      procedure Set_Code (Input : String) is
      begin
         Code          := To_Unbounded_String (Input);
         Code_Received := True;
      end Set_Code;

      entry Get_Code (Result : out Unbounded_String) when Code_Received is
      begin
         Result := Code;
      end Get_Code;

      procedure Reset is
      begin
         Code_Received := False;
      end Reset;

   end Authorise_Object;
   --  Subprograms

   function Code_Callback (Request : Status.Data) return Response.Data is
   begin
      if Status.Method (Request) = Status.GET then
         --  Reject invalid states
         if Parameter (Request, Name => "state") /= Local_State then
            return
              Response.Acknowledge
                (Messages.S400, "Invalid state received. Please try again.");
         end if;
         Authorise_Object.Set_Code (Parameter (Request, Name => "code"));
         return Response.Acknowledge (Messages.S200, "");

      else
         return Response.Acknowledge (Messages.S400, "Unsupported method.");
      end if;
   end Code_Callback;

   function Get_Code return Unbounded_String is

      WS   : Server.HTTP;
      Code : Unbounded_String;

   begin
      Server.Set_Security (WS, "dat/cert.pem");
      Server.Start
        (WS,
         Name           => "Code Callback",
         Callback       => Code_Callback'Access,
         Max_Connection => 1,
         Port           => 8_888,
         Security       => True); -- Enable HTTPS

      Authorise_Object.Get_Code (Code);
      Server.Shutdown (WS);
      Authorise_Object.Reset;
      return Code;
   exception
      when AWS.Net.Socket_Error =>
         Put_Line
           (Standard_Error,
            "[API.Authorise] Failed to bind port 8888. Cannot authenticate.");
         GNAT.OS_Lib.OS_Exit (-1);
   end Get_Code;
   --  Parse JSON

   function Parse_JSON
     (Message_Body : Unbounded_String) return Auth_Storage_Type
   is
      Data : constant JSON_Value := Read (Message_Body, "<auth_data>");
   begin
      return
        (Data.Get ("access_token"),
         Data.Get ("refresh_token"),
         Data.Get ("membership_id"));
   end Parse_JSON;

   function Get_Token
     (Refresh_Token : Unbounded_String) return Auth_Storage_Type
   is

      Data : Response.Data;

   begin
      Debug.Put_Line ("Refresh token");
      Data :=
        Client.Post
          (URL  => OAuth_Token_Endpoint,
           Data =>
             "grant_type=refresh_token" & "&refresh_token=" &
             To_String (Refresh_Token) & "&client_id=" & Secrets.Client_ID &
             "&client_secret=" & Secrets.Client_Secret,
           Content_Type => "application/x-www-form-urlencoded");
--              Debug.Put_Line (Response.Message_Body (Data));

      Check_Status (Data);
      return Parse_JSON (Response.Message_Body (Data));
   end Get_Token;

   function Get_Initial_Token
     (Code : Unbounded_String) return Auth_Storage_Type
   is

      Data : Response.Data;

   begin
      Debug.Put_Line ("Initial token");
      Data :=
        Client.Post
          (URL  => OAuth_Token_Endpoint,
           Data =>
             "grant_type=authorization_code" & "&code=" & To_String (Code) &
             "&client_id=" & Secrets.Client_ID & "&client_secret=" &
             Secrets.Client_Secret,
           Content_Type => "application/x-www-form-urlencoded");
--              Debug.Put_Line (Response.Message_Body (Data));

      Check_Status (Data);
      return Parse_JSON (Response.Message_Body (Data));
   end Get_Initial_Token;

   function Do_Authorise (State : String) return Auth_Storage_Type is

      Auth_Storage : Auth_Storage_Type;

   begin
      Local_State := +State;

      if Exists ("dat/refresh.dat") then -- Load token
         Debug.Put_Line ("Load token");
         --  Load refresh token

         declare

            SF            : Stream_IO.File_Type;
            S             : Stream_IO.Stream_Access;
            Refresh_Token : Unbounded_String;

         begin
            Stream_IO.Open
              (SF, Mode => Stream_IO.In_File, Name => "dat/refresh.dat");
            S := Stream_IO.Stream (SF);
            Unbounded_String'Read (S, Refresh_Token);
            Stream_IO.Close (SF);
            --  Refresh both tokens
            Auth_Storage := Get_Token (Refresh_Token);
         end;

      else -- Fetch token
         Debug.Put_Line ("Fetch token");
         --  Get Authorisation Code
         --  Get Access Token and Refresh Token
         Auth_Storage := Get_Initial_Token (Get_Code);
      end if;
      --  Save refresh token
      Debug.Put_Line ("Save refresh token");
      Save_Refresh_Token :
      declare

         SF : Stream_IO.File_Type;
         S  : Stream_IO.Stream_Access;

      begin
         if Exists ("dat/refresh.dat") then
            Stream_IO.Open
              (SF, Mode => Stream_IO.Out_File, Name => "dat/refresh.dat");

         else
            Stream_IO.Create (SF, Name => "dat/refresh.dat");
         end if;
         S := Stream_IO.Stream (SF);
         Unbounded_String'Write (S, Auth_Storage.Refresh_Token);
         Stream_IO.Close (SF);
      end Save_Refresh_Token;
      return Auth_Storage;
   end Do_Authorise;

end API.Authorise;
