pragma Ada_2022;

-- AWS
with AWS.Client;
with AWS.Response;
use AWS;

-- VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with VSS.Text_Streams; use VSS.Text_Streams;
use VSS.JSON.Pull_Readers;
use VSS.JSON;

-- Local Packages
with JSON; use JSON;

package body API.Profiles is
	-- Note: Position Reader before "data"	
	procedure Read_Characters (
		Reader : in out JSON_Simple_Pull_Reader;
		List : out Character_List)
	is begin
		Wait_Until_Key (Reader, "data");
		Read_Next (Reader); -- START_OBJECT
		loop
			Read_Next (Reader);

			declare
				Character : Character_Type;
				Stat_Temp : Manifest_Hash;
			begin
				case Event_Kind (Reader) is
					when Key_Name => -- Character ID
						Read_Next (Reader); -- START_OBJECT
						Read_Character : loop
							Read_Next (Reader);
							case Event_Kind (Reader) is
								when Key_Name =>
									if VS2S (Key_Name (Reader)) = "characterId" then
										Read_Next (Reader);
										Character.Character_ID := VS2UB (String_Value (Reader));
									elsif VS2S (Key_Name (Reader)) = "dateLastPlayed" then
										Read_Next (Reader);
										Character.Date_Last_Played := VS2UB (String_Value (Reader));
									elsif VS2S (Key_Name (Reader)) = "light" then
										Read_Next (Reader);
										Character.Light := Integer_32 (
											As_Integer (Number_Value (Reader)));
									elsif VS2S (Key_Name (Reader)) = "stats" then
										Read_Next (Reader); -- START_OBJECT
										Read_Next (Reader); -- KEY_NAME
										while Event_Kind (Reader) /= End_Object loop
											Stat_Temp := Manifest_Hash'Value (
												VS2S (Key_Name (Reader))); -- Stat Name Hash
											Read_Next (Reader); -- NUMBER_VALUE
											Character.Stats.Insert (
												Stat_Temp, 
												Integer_32 (
													As_Integer (Number_Value (Reader))));
											Read_Next (Reader); -- KEY_NAME or END_OBJECT
										end loop;
									elsif VS2S (Key_Name (Reader)) = "raceHash" then
										Read_Next (Reader);
										Character.Race_Hash := Manifest_Hash (
											As_Integer (Number_Value (Reader)));
									elsif VS2S (Key_Name (Reader)) = "genderHash" then
										Read_Next (Reader);
										Character.Gender_Hash := Manifest_Hash (
											As_Integer (Number_Value (Reader)));
									elsif VS2S (Key_Name (Reader)) = "classHash" then
										Read_Next (Reader);
										Character.Class_Hash := Manifest_Hash (
											As_Integer (Number_Value (Reader)));
									elsif VS2S (Key_Name (Reader)) = "emblemPath" then
										Read_Next (Reader);
										Character.Emblem_Path := VS2UB (String_Value (Reader));
									elsif VS2S (Key_Name (Reader)) = "emblemBackgroundPath" then
										Read_Next (Reader);
										Character.Emblem_Background_Path := VS2UB (String_Value (Reader));
									elsif VS2S (Key_Name (Reader)) = "titleRecordHash" then
										Read_Next (Reader);

										-- Can be Null_Value
										if Event_Kind (Reader) = Number_Value then
											Character.Title_Record_Hash := Unsigned_32 (
												As_Integer (Number_Value (Reader)));
										end if;
									elsif VS2S (Key_Name (Reader)) = "emblemColor" then
										-- not processed
										Read_Next (Reader); -- START_OBJECT
										Wait_Until_Event (Reader, End_Object);
									elsif VS2S (Key_Name (Reader)) = "levelProgression" then
										-- not processed
										Read_Next (Reader); -- START_OBJECT
										Wait_Until_Event (Reader, End_Object);
									else
										Read_Next (Reader);
									end if;
								when End_Object => exit Read_Character;
								when others => raise Program_Error;
							end case;
						end loop Read_Character;

						-- Submit Character
						List.Append (Character);
					when End_Object => exit;
					when others => raise Program_Error;
				end case;
			end;
		end loop;
	end Read_Characters;

	-- Note: Position Reader at START_OBJECT
	function Read_Item (
		Reader : in out JSON_Simple_Pull_Reader) return Item_Type
	is
		Item : Item_Type;
	begin
		loop
			Read_Next (Reader);
			case Event_Kind (Reader) is
				when Key_Name =>
					if VS2S (Key_Name (Reader)) = "itemHash" then
						Read_Next (Reader);
						Item.Item_Hash := Manifest_Hash (
							As_Integer (
								Number_Value (Reader)));
					elsif VS2S (Key_Name (Reader)) = "itemInstanceId" then
						Read_Next (Reader);
						Item.Item_Instance_ID := VS2UB (String_Value (Reader));
					elsif VS2S (Key_Name (Reader)) = "quantity" then
						Read_Next (Reader);
						Item.Quantity := Integer_32 (
							As_Integer (
								Number_Value (Reader)));
					elsif VS2S (Key_Name (Reader)) = "bindStatus" then
						Read_Next (Reader);
						Item.Bind_Status :=
							Bind_Status_Type'Enum_Val (
								As_Integer (Number_Value (Reader)));
					elsif VS2S (Key_Name (Reader)) = "location" then
						Read_Next (Reader);
						Item.Location :=
							Item_Location_Type'Enum_Val (
								As_Integer (Number_Value (Reader)));
					elsif VS2S (Key_Name (Reader)) = "bucketHash" then
						Read_Next (Reader);
						Item.Bucket_Hash := Manifest_Hash (
							As_Integer (
								Number_Value (Reader)));
					elsif VS2S (Key_Name (Reader)) = "transferStatus" then
						Read_Next (Reader);
						Item.Transfer_Status :=
							Transfer_Status_Type'Enum_Val (
								As_Integer (Number_Value (Reader)));
					elsif VS2S (Key_Name (Reader)) = "lockable" then
						Read_Next (Reader);
						Item.Lockable := Boolean_Value (Reader);
					elsif VS2S (Key_Name (Reader)) = "state" then
						Read_Next (Reader);
						declare
							Temp : constant Unsigned_64 :=
								Unsigned_64 (As_Integer (Number_Value (Reader)));
						begin
							Item.State := (
								Locked => (Temp and 2#1#) > 0,
								Tracked => (Temp and 2#10#) > 0,
								Masterwork => (Temp and 2#100#) > 0,
								Crafted => (Temp and 2#1000#) > 0,
								Highlighted_Objective => (Temp and 2#10000#) > 0);
						end;
					elsif VS2S (Key_Name (Reader)) = "overrideStyleItemHash" then
						Read_Next (Reader);
						Item.Override_Style_Item_Hash := Manifest_Hash (
							As_Integer (Number_Value (Reader)));
					elsif VS2S (Key_Name (Reader)) = "expirationDate" then
						Read_Next (Reader);
						Item.Expiration_Date := VS2UB (String_Value (Reader));
					elsif VS2S (Key_Name (Reader)) = "tooltipNotificationIndexes" then
						-- TODO Not processed currently
						Read_Next (Reader); -- START_ARRAY
						Wait_Until_Event (Reader, End_Array);	
					elsif VS2S (Key_Name (Reader)) = "itemValueVisibility" then
						-- TODO Not processed currently
						Read_Next (Reader); -- START_ARRAY
						Wait_Until_Event (Reader, End_Array);	
					elsif VS2S (Key_Name (Reader)) = "metricObjective" then
						-- TODO Not processed currently
						Read_Next (Reader); -- START_OBJECT
						Wait_Until_Event (Reader, End_Object);
					elsif VS2S (Key_Name (Reader)) = "versionNumber" then
						Read_Next (Reader);
						Item.Version_Number := Integer_32 (
							As_Integer (Number_Value (Reader)));
					else
						Read_Next (Reader);
					end if;
				when End_Object => exit;
				when others => raise Program_Error;
			end case;
		end loop;
		return Item;
	end Read_Item;

	--Note: Position Reader before "items"
	procedure Read_Inventory (
		Reader : in out JSON_Simple_Pull_Reader;
		Inventory : out Item_List)
	is begin
		Wait_Until_Key (Reader, "items");
		Read_Next (Reader);
		case Event_Kind (Reader) is
			when Start_Array => null;
			when others => raise Program_Error;
		end case;

		Read_Items : loop
			Read_Next (Reader);
			case Event_Kind (Reader) is
				when Start_Object =>
					Inventory.Append (Read_Item (Reader));
				when End_Array => exit Read_Items;
				when others => raise Program_Error;
			end case;
		end loop Read_Items;
	end Read_Inventory;

	
	--Note: Position Reader at START_OBJECT
	procedure Read_Loadout (
		Reader : in out JSON_Simple_Pull_Reader;
		Loadout : out Loadout_Type)
	is
		--Note: Position Reader at START_OBJECT
		function Read_Loadout_Item (Reader : in out JSON_Simple_Pull_Reader)
			return Loadout_Item_Type
		is 
			Result : Loadout_Item_Type;
		begin
			Read_Next (Reader); -- "itemInstanceId"
			Read_Next (Reader);
			Result.Instance_ID := VS2UB (String_Value (Reader));

			Read_Next (Reader); -- "plugItemHashes";
			Read_Next (Reader); -- START_ARRAY
			Read_Next (Reader); -- Number_Value / End_Array

			while Event_Kind (Reader) /= End_Array loop
				Result.Plug_Item_Hashes.Append (Manifest_Hash (
					As_Integer (Number_Value (Reader))));
				Read_Next (Reader); -- End_Array / Number_Value
			end loop;

			Read_Next (Reader); -- End_Object

			return Result;
		end Read_Loadout_Item;
	begin
		loop
			Read_Next (Reader);
			case Event_Kind (Reader) is
				when Key_Name =>
					if VS2S (Key_Name (Reader)) = "colorHash" then
						Read_Next (Reader);
						Loadout.Colour_Hash := Manifest_Hash (
							As_Integer (Number_Value (Reader)));
					elsif VS2S (Key_Name (Reader)) = "iconHash" then
						Read_Next (Reader);
						Loadout.Icon_Hash := Manifest_Hash (
							As_Integer (Number_Value (Reader)));

					elsif VS2S (Key_Name (Reader)) = "nameHash" then
						Read_Next (Reader);
						Loadout.Name_Hash := Manifest_Hash (
							As_Integer (Number_Value (Reader)));

					elsif VS2S (Key_Name (Reader)) = "items" then
						Read_Next (Reader); -- START_ARRAY
						Read_Next (Reader); -- START_OBJECT
						while Event_Kind (Reader) /= End_Array loop
							Loadout.Items.Append (Read_Loadout_Item (Reader));
							Read_Next (Reader); -- START_OBJECT / END_ARRAY
						end loop;
					else
						raise Program_Error;
					end if;
				when End_Object => exit;
				when others => raise Program_Error;
			end case;
		end loop;
	end Read_Loadout;

	function Get_Profile (
		Headers : Auth_Header_Type;
		M : Membership_Type) return Profile_Type
	is
		Result : Profile_Type;

		Data : Response.Data;
		Stream : Memory_UTF8_Input_Stream_Access;
		Reader : JSON_Simple_Pull_Reader;
	begin
		Put_Debug ("Get profiles");
--		Data := Client.Get (
--			API_Root & "/Destiny2/"
--				& Memberships.Find_Default_Platform_ID (M)
--				& "/Profile/" & (+M.Primary_Membership_ID)
--				& "/" & "?components=ProfileInventories,ProfileCurrencies,PlatformSilver,Characters,CharacterInventories,CharacterProgressions,CharacterEquipment,CharacterLoadouts",
--			Headers => Headers);
--		Check_Status (Data);
--		Put_Debug (Response.Message_Body (Data));

--		Stream := Get_Stream (Response.Message_Body (Data));
		Stream := Get_Stream (+Read_File ("json/profiles.json"));
		Set_Stream (Reader, Input_Text_Stream_Access (Stream));

		Wait_Until_Key (Reader, "profileInventory");
		Read_Inventory (Reader, Result.Profile_Inventory);

		Wait_Until_Key (Reader, "profileCurrencies");
		Read_Inventory (Reader, Result.Profile_Currencies);

		Wait_Until_Key (Reader, "platformSilver");
		Read_Next (Reader); -- START_OBJECT
		while Event_Kind (Reader) /= End_Object loop
			Read_Next (Reader); -- "TigerPsn"
			Read_Next (Reader); -- START_OBJECT
			Result.Platform_Silver.PSN := Read_Item (Reader);
			Read_Next (Reader); -- "TigerXbox"
			Read_Next (Reader); -- START_OBJECT
			Result.Platform_Silver.XBOX := Read_Item (Reader);
			Read_Next (Reader); -- "TigerBlizzard"
			Read_Next (Reader); -- START_OBJECT
			Result.Platform_Silver.Blizzard := Read_Item (Reader);
			Read_Next (Reader); -- "TigerStadia"
			Read_Next (Reader); -- START_OBJECT
			Result.Platform_Silver.Stadia := Read_Item (Reader);
			Read_Next (Reader); -- "TigerSteam"
			Read_Next (Reader); -- START_OBJECT
			Result.Platform_Silver.Steam := Read_Item (Reader);
			Read_Next (Reader); -- "BungieNext"
			Read_Next (Reader); -- START_OBJECT
			Result.Platform_Silver.Bungie_Next := Read_Item (Reader);
			Read_Next (Reader); -- "TigerEgs"
			Read_Next (Reader); -- START_OBJECT
			Result.Platform_Silver.EGS := Read_Item (Reader);
		end loop;

		Wait_Until_Key (Reader, "characters");
		Read_Characters (Reader, Result.Characters);

		Wait_Until_Key (Reader, "characterInventories");
		Wait_Until_Key (Reader, "data");
		Read_Next (Reader); -- START_OBJECT

		while Event_Kind (Reader) /= End_Object loop
			declare
				Character_ID : Unbounded_String;
				Inventory : Item_List;
			begin
				Read_Next (Reader); -- KEY_NAME
				Character_ID := VS2UB (Key_Name (Reader));	
				Read_Inventory (Reader, Inventory);
				Result.Character_Inventories.Insert (Character_ID, Inventory);
				Read_Next (Reader); -- END_OBJECT
				Read_Next (Reader); -- START_OBJECT / END_OBJECT
			end;
		end loop;

		Wait_Until_Key (Reader, "characterLoadouts");
		Wait_Until_Key (Reader, "data");
		Read_Next (Reader); -- START_OBJECT
		Read_Next (Reader); -- Character ID / END_OBJECT
		
		while Event_Kind (Reader) /= End_Object loop
			declare
				Character_ID : Unbounded_String;
				Loadouts : Loadout_List;
				Loadout : Loadout_Type;
			begin
				Character_ID := VS2UB (String_Value (Reader));

				Read_Next (Reader); -- START_OBJECT
				Read_Next (Reader); -- "loadouts"
				Read_Next (Reader); -- START_ARRAY
				Read_Next (Reader); -- START_OBJECT

				while Event_Kind (Reader) /= End_Array loop
					Read_Loadout (Reader, Loadout);
					Loadouts.Append (Loadout);
					Read_Next (Reader); -- START_OBJECT / END_ARRAY
				end loop;

				Result.Character_Loadouts.Insert (Character_ID, Loadouts);

				Read_Next (Reader); -- Character ID / END_OBJECT
			end;
		end loop;

		Wait_Until_Key (Reader, "characterEquipment");
		Wait_Until_Key (Reader, "data");
		Read_Next (Reader); -- START_OBJECT

		while Event_Kind (Reader) /= End_Object loop
			declare
				Character_ID : Unbounded_String;
				Inventory : Item_List;
			begin
				Read_Next (Reader); -- KEY_NAME
				Character_ID := VS2UB (Key_Name (Reader));	
				Read_Inventory (Reader, Inventory);
				Result.Character_Equipment.Insert (Character_ID, Inventory);
				Read_Next (Reader); -- START_OBJECT / END_OBJECT
			end;
		end loop;

		Free (Stream);
		return Result;
	end Get_Profile;
end API.Profiles;
