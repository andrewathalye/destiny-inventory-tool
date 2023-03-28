pragma Ada_2022;

-- AWS
with AWS.Client;
with AWS.Response;
use AWS;

-- VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with VSS.Text_Streams; use VSS.Text_Streams;
with VSS.Text_Streams.Memory_UTF8_Input; use VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Stream_Element_Vectors.Conversions; use VSS.Stream_Element_Vectors.Conversions;
use VSS.JSON.Pull_Readers;
use VSS.JSON;

-- Local Packages
with JSON; use JSON;
with Shared; use Shared;

package body API.Manifest is
	-- Note Seek to "DestinyGenderDefinition"
	procedure Read_Genders (
			Reader : in out JSON_Simple_Pull_Reader;
			Genders : out Destiny_Gender_Map)
	is
		Gender : Destiny_Gender_Definition;
	begin
		Read_Next (Reader); -- START_OBJECT
		while Event_Kind (Reader) /= End_Object loop
			Wait_Until_Key (Reader, "genderType");
			Read_Next (Reader);
			Gender.Gender_Type := Destiny_Gender_Type'Enum_Val (
				As_Integer (Number_Value (Reader)));

			Wait_Until_Key (Reader, "name");
			Read_Next (Reader);
			Gender.Gender_Name := VS2UB (String_Value (Reader));

			Wait_Until_Key (Reader, "hash");
			Read_Next (Reader);
			Genders.Insert (
				Manifest_Hash (
					As_Integer (
						Number_Value (Reader))),
				Gender);

			Wait_Until_Event (Reader, End_Object);
			Read_Next (Reader);
		end loop;
	end Read_Genders;

	function Get_Manifest (
		Headers : Auth_Header_Type;
		M : Memberships.Membership_Type) return Manifest_Type
	is
		-- Storage
		Localised_Manifest_Path : Unbounded_String;

		-- Requests
		Data : Response.Data;
		Stream : Memory_UTF8_Input_Stream_Access;

		-- Parsing
		Reader : JSON_Simple_Pull_Reader;
		Reader2 : JSON_Simple_Pull_Reader;
		Result : Manifest_Type;

	begin
		Put_Debug ("Get manifest");
--		Data := Client.Get (
--			API_Root & "/Destiny2/Manifest/");
--		Check_Status (Data);
--		Put_Debug (Response.Message_Body (Data));

--		Stream := Get_Stream (Response.Message_Body (Data));
		Stream := Get_Stream (+Read_File ("json/manifest.json"));

		Set_Stream (Reader, Input_Text_Stream_Access (Stream));

		Wait_Until_Key (Reader, "jsonWorldContentPaths");
		Wait_Until_Key (Reader, +M.Bungie_Net_User.Locale); -- TODO Needs to change
		Read_Next (Reader); -- STRING_VALUE

		Localised_Manifest_Path := VS2UB (String_Value (Reader));

		if Has_Cached (+Localised_Manifest_Path) then
			Put_Debug ("Loaded cached localised manifest");
			Set_Data (Stream.all, To_Stream_Element_Vector (
				Get_Cached (+Localised_Manifest_Path)));
		else
			Put_Debug ("Get localised manifest");
			Data := Client.Get (
				"https://www.bungie.net" & (+Localised_Manifest_Path));
			Check_Status (Data);
			Cache (+Localised_Manifest_Path, Response.Message_Body (Data));
			Set_Data (Stream.all, To_Stream_Element_Vector (
				Response.Message_Body (Data)));
		end if;

		Set_Stream (Reader2, Input_Text_Stream_Access (Stream));
		
		Wait_Until_Key (Reader2, "DestinyGenderDefinition");
		Read_Genders (Reader2, Result.Destiny_Genders);

		Free (Stream);
		Put_Debug (Result'Image);
		return Result;
	end Get_Manifest;
end API.Manifest;
