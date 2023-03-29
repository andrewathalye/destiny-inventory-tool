pragma Ada_2022;

with Ada.Streams.Stream_IO;
with Ada.Directories; use Ada.Directories;

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

	-- Note Seek to "DestinyRaceDefinition"
	procedure Read_Races (
			Reader : in out JSON_Simple_Pull_Reader;
			Races : out Destiny_Race_Map)
	is
		Race : Destiny_Race_Name;
	begin
		Read_Next (Reader); -- START_OBJECT
		while Event_Kind (Reader) /= End_Object loop
			Wait_Until_Key (Reader, "genderedRaceNames");
			Read_Next (Reader); -- START_OBJECT

			Read_Next (Reader); -- "Male"
			Read_Next (Reader);
			Race (Male) := VS2UB (String_Value (Reader));

			Read_Next (Reader); -- "Female"
			Read_Next (Reader);
			Race (Female) := VS2UB (String_Value (Reader));

			Wait_Until_Key (Reader, "hash");
			Read_Next (Reader);
			Races.Insert (
				Manifest_Hash (
					As_Integer (
						Number_Value (Reader))),
				Race);

			Wait_Until_Event (Reader, End_Object);
			Read_Next (Reader);
		end loop;
	end Read_Races;

	-- Note Seek to "DestinyClassDefinition"
	procedure Read_Classes (
			Reader : in out JSON_Simple_Pull_Reader;
			Classes : out Destiny_Class_Map)
	is
		Class : Destiny_Class_Name;
	begin
		Read_Next (Reader); -- START_OBJECT
		while Event_Kind (Reader) /= End_Object loop
			Wait_Until_Key (Reader, "genderedClassNames");
			Read_Next (Reader); -- START_OBJECT

			Read_Next (Reader); -- "Male"
			Read_Next (Reader);
			Class (Male) := VS2UB (String_Value (Reader));

			Read_Next (Reader); -- "Female"
			Read_Next (Reader);
			Class (Female) := VS2UB (String_Value (Reader));

			Wait_Until_Key (Reader, "hash");
			Read_Next (Reader);
			Classes.Insert (
				Manifest_Hash (
					As_Integer (
						Number_Value (Reader))),
				Class);

			Wait_Until_Event (Reader, End_Object);
			Read_Next (Reader);
		end loop;
	end Read_Classes;

	-- Note Seek to "DestinyRecordDefinition"
	procedure Read_Titles (
		Reader : in out JSON_Simple_Pull_Reader;
		Titles : out Destiny_Title_Map)
	is
		Title : Destiny_Title_Name;
	begin
		Read_Next (Reader); -- START_OBJECT
		while Event_Kind (Reader) /= End_Object loop
			Wait_Until_Key (Reader, "titleInfo");
			Read_Next (Reader); -- START_OBJECT

			Read_Next (Reader); -- "hasTitle"
			Read_Next (Reader);
			if Boolean_Value (Reader) then
				Read_Next (Reader); -- "titlesByGender"
				Read_Next (Reader); -- START_OBJECT

				Read_Next (Reader); -- "Male"
				Read_Next (Reader);
				Title (Male) := VS2UB (String_Value (Reader));
				Read_Next (Reader); -- "Female"
				Read_Next (Reader);
				Title (Female) := VS2UB (String_Value (Reader));

				Wait_Until_Key (Reader, "hash");
				Read_Next (Reader);
				Titles.Insert (
					Manifest_Hash (
						As_Integer (
							Number_Value (Reader))),
					Title);
			end if;

			Wait_Until_Key (Reader, "blacklisted");
			Wait_Until_Event (Reader, End_Object);
			Read_Next (Reader);
		end loop;
	end Read_Titles;

	-- Note Seek to "DestinyInventoryBucketDefinition"
	procedure Read_Inventory_Buckets (
		Reader : in out JSON_Simple_Pull_Reader;
		Buckets : out Destiny_Inventory_Bucket_Map)
	is
		Bucket : Destiny_Inventory_Bucket_Definition;
	begin
		Read_Next (Reader); -- START_OBJECT
		while Event_Kind (Reader) /= End_Object loop
			Bucket.Description := Null_Unbounded_String;
			Bucket.Name := Null_Unbounded_String;

			Wait_Until_Key (Reader, "displayProperties");
			Read_Next (Reader); -- START_OBJECT
			Read_Next (Reader); -- "description" or "hasIcon"

			-- If the bucket has a name and description, add them
			if VS2S (Key_Name (Reader)) = "description" then
				Read_Next (Reader);
				Bucket.Description := VS2UB (String_Value (Reader));

				Read_Next (Reader); -- "name"
				Read_Next (Reader);
				Bucket.Name := VS2UB (String_Value (Reader));
			end if;

			Wait_Until_Key (Reader, "category");
			Read_Next (Reader);
			Bucket.Category := Destiny_Inventory_Bucket_Category'Enum_Val (
				As_Integer (
					Number_Value (Reader)));

			Read_Next (Reader); -- "bucketOrder"
			Read_Next (Reader);
			Bucket.Bucket_Order := Integer_32 (
				As_Integer (
					Number_Value (Reader)));

			Read_Next (Reader); -- "itemCount"
			Read_Next (Reader);
			Bucket.Item_Count := Integer_32 (
				As_Integer (
					Number_Value (Reader)));

			Wait_Until_Key (Reader, "fifo");
			Read_Next (Reader);
			Bucket.FIFO := Boolean_Value (Reader);

			Wait_Until_Key (Reader, "hash");
			Read_Next (Reader);
			Buckets.Insert (
				Manifest_Hash (
					As_Integer (
						Number_Value (Reader))),
				Bucket);

			Wait_Until_Event (Reader, End_Object);
			Read_Next (Reader);
		end loop;
	end Read_Inventory_Buckets;

	-- Note Seek to "DestinyDamageTypeDefinition"
	procedure Read_Damage_Types (
		Reader : in out JSON_Simple_Pull_Reader;
		Damage_Types : out Destiny_Damage_Type_Map)
	is
		Damage_Type : Destiny_Damage_Type_Definition;
	begin
		Read_Next (Reader); -- START_OBJECT
		while Event_Kind (Reader) /= End_Object loop
			Damage_Type.Icon_Path := Null_Unbounded_String;

			Wait_Until_Key (Reader, "displayProperties");
			Read_Next (Reader); -- START_OBJECT

			Read_Next (Reader); -- "description"
			Read_Next (Reader);
			Damage_Type.Description := VS2UB (String_Value (Reader));

			Read_Next (Reader); -- "name"
			Read_Next (Reader);
			Damage_Type.Name := VS2UB (String_Value (Reader));

			Read_Next (Reader); -- "icon" or "hasIcon"
			if VS2S (Key_Name (Reader)) = "icon" then
				Read_Next (Reader);
				Damage_Type.Icon_Path := VS2UB (String_Value (Reader));
			end if;

			Wait_Until_Key (Reader, "showIcon");
			Read_Next (Reader);
			Damage_Type.Show_Icon := Boolean_Value (Reader);

			Wait_Until_Key (Reader, "hash");
			Read_Next (Reader);
			Damage_Types.Insert (
				Manifest_Hash (
					As_Integer (
						Number_Value (Reader))),
				Damage_Type);

			Wait_Until_Event (Reader, End_Object);
			Read_Next (Reader);
		end loop;
	end Read_Damage_Types;

	-- Note Seek to "DestinyInventoryItemDefinition"
	procedure Read_Inventory_Items (
		Reader : in out JSON_Simple_Pull_Reader;
		Items : out Destiny_Inventory_Item_Map)
	is
		Item : Destiny_Inventory_Item_Definition;
	begin
		Item.Icon_Path := Null_Unbounded_String;
		Item.Watermark_Path := Null_Unbounded_String;
		Item.Shelved_Watermark_Path := Null_Unbounded_String;
		Item.Bucket_Type_Hash := 0;

		Read_Next (Reader); -- START_OBJECT
		while Event_Kind (Reader) /= End_Object loop
			Wait_Until_Key (Reader, "displayProperties");
			Read_Next (Reader); -- START_OBJECT

			Read_Next (Reader); -- "description"
			Read_Next (Reader);
			Item.Description := VS2UB (String_Value (Reader));

			Read_Next (Reader); -- "name"
			Read_Next (Reader);
			Item.Name := VS2UB (String_Value (Reader));

			Read_Next (Reader); -- "icon" or "hasIcon"
			if VS2S (Key_Name (Reader)) = "icon" then
				Read_Next (Reader);
				Item.Icon_Path := VS2UB (String_Value (Reader));
			end if;

			-- Read optional fields with variable locations
			Read_Variable_Fields : loop
				case Event_Kind (Reader) is
					when Key_Name =>
						if VS2S (Key_Name (Reader)) = "itemTypeAndTierDisplayName" then
							Read_Next (Reader);
							Item.Item_Type_And_Tier_Display_Name := VS2UB (String_Value (Reader));
						elsif VS2S (Key_Name (Reader)) = "bucketTypeHash" then
							Read_Next (Reader);
							Item.Bucket_Type_Hash := Manifest_Hash (
								As_Integer (
									Number_Value (Reader)));
						elsif VS2S (Key_Name (Reader)) = "tierType" then
							Read_Next (Reader);
							Item.Tier_Type := Destiny_Tier_Type'Enum_Val (
								As_Integer (
									Number_Value (Reader)));

						elsif VS2S (Key_Name (Reader)) = "defaultDamageTypeHash" then
							Read_Next (Reader);
							Item.Default_Damage_Type_Hash := Manifest_Hash (
								As_Integer (
									Number_Value (Reader)));
						elsif VS2S (Key_Name (Reader)) = "collectibleHash" then
							Read_Next (Reader); -- value ignored TODO
							Read_Next (Reader); -- "iconWatermark", "iconWatermarkShelved", etc.
						elsif VS2S (Key_Name (Reader)) = "iconWatermark" then
							Read_Next (Reader);
							Item.Watermark_Path := VS2UB (String_Value (Reader));
							Read_Next (Reader); -- "iconWatermarkShelved", etc.
						elsif VS2S (Key_Name (Reader)) = "iconWatermarkShelved" then
							Read_Next (Reader);
							Item.Shelved_Watermark_Path := VS2UB (String_Value (Reader));
							Read_Next (Reader); -- etc.
						elsif VS2S (Key_Name (Reader)) = "hash" then
							exit Read_Variable_Fields;
						else
							Read_Next (Reader);
						end if;

					when others => null;

					Read_Next (Reader);
				end case;
			end loop Read_Variable_Fields;
			
			-- At "hash"
			Read_Next (Reader);
			Items.Insert (
				Manifest_Hash (
					As_Integer (
						Number_Value (Reader))),
				Item);

			Wait_Until_Event (Reader, End_Object);
			Read_Next (Reader);
		end loop;
	end Read_Inventory_Items;

	function Fetch_Manifest (Localised_Manifest_Path : Unbounded_String) return Manifest_Type is
		Data : Response.Data;

		Result : Manifest_Type;

		Stream : Memory_UTF8_Input_Stream_Access := new Memory_UTF8_Input_Stream;
		Reader : JSON_Simple_Pull_Reader;
	begin
		Put_Debug ("Fetch and parse manifest");
		if Has_Cached (+Localised_Manifest_Path) then
			Put_Debug ("Loaded cached localised manifest");
			Set_Data (Stream.all, To_Stream_Element_Vector (
				Get_Cached (+Localised_Manifest_Path)));
		else
			Put_Debug ("Get localised manifest");
			Data := Client.Get (
				Bungie_Root & (+Localised_Manifest_Path));
			Check_Status (Data);
			Cache (+Localised_Manifest_Path, Response.Message_Body (Data));
			Set_Data (Stream.all, To_Stream_Element_Vector (
				Response.Message_Body (Data)));
		end if;

		-- Read Fields from Manifest
		Set_Stream (Reader, Input_Text_Stream_Access (Stream));
		
		Wait_Until_Key (Reader, "DestinyClassDefinition");
		Read_Classes (Reader, Result.Destiny_Classes);

		Put_Debug ("classes read");

		Wait_Until_Key (Reader, "DestinyGenderDefinition");
		Read_Genders (Reader, Result.Destiny_Genders);

		Put_Debug ("genders read");

		Wait_Until_Key (Reader, "DestinyInventoryBucketDefinition");
		Read_Inventory_Buckets (Reader, Result.Destiny_Inventory_Buckets);

		Put_Debug ("buckets read");

		Wait_Until_Key (Reader, "DestinyRaceDefinition");
		Read_Races (Reader, Result.Destiny_Races);

		Put_Debug ("races read");

		Wait_Until_Key (Reader, "DestinyDamageTypeDefinition");
		Read_Damage_Types (Reader, Result.Destiny_Damage_Types);

		Put_Debug ("damage types read");

		Wait_Until_Key (Reader, "DestinyInventoryItemDefinition");
		Read_Inventory_Items (Reader, Result.Destiny_Inventory_Items);

		Put_Debug ("items read");

		Wait_Until_Key (Reader, "DestinyRecordDefinition");
		Read_Titles (Reader, Result.Destiny_Titles);

		Put_Debug ("records read");

		Free (Stream);
--		Put_Debug (Result'Image);
--		We can't use 'Image for Debug output here because the UB Strings are UTF-8 encoded
		declare
			use Ada.Streams.Stream_IO;

			SF : File_Type;
			S : Stream_Access;
		begin
			Create (SF, Out_File, "manifest.dat");
			S := Ada.Streams.Stream_IO.Stream (SF);
			Unbounded_String'Write (S, Localised_Manifest_Path);
			Manifest_Type'Write (S, Result);
			Close (SF);
		end;

		return Result;
	end Fetch_Manifest;

	function Get_Manifest (M : Memberships.Membership_Type) return Manifest_Type
	is
		-- Storage
		Localised_Manifest_Path : Unbounded_String;

		-- Requests
		Data : Response.Data;
		Stream : Memory_UTF8_Input_Stream_Access;

		-- Parsing
		Reader : JSON_Simple_Pull_Reader;
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
		Wait_Until_Key (Reader, "en"); -- +M.Bungie_Net_User.Locale); -- TODO Needs to change
		Read_Next (Reader); -- STRING_VALUE

		Localised_Manifest_Path := VS2UB (String_Value (Reader));

		Free (Stream);
		
		if Exists ("manifest.dat") then
			Put_Debug ("Load preparsed manifest");
			declare
				use Ada.Streams.Stream_IO;

				SF : File_Type;
				S : Stream_Access;

				Manifest_Version : Unbounded_String;
			begin
				Open (SF, In_File, "manifest.dat");
				S := Ada.Streams.Stream_IO.Stream (SF);
				Unbounded_String'Read (S, Manifest_Version);
				Manifest_Type'Read (S, Result);
				Close (SF);

				if Manifest_Version /= Localised_Manifest_Path then
					Put_Debug ("Update prepared manifest");
					Delete_File ("manifest.dat");
					return Fetch_Manifest (Localised_Manifest_Path);
				end if;

				return Result;
			end;
		else
			return Fetch_Manifest (Localised_Manifest_Path);
		end if;
	end Get_Manifest;
end API.Manifest;
