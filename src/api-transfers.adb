pragma Ada_2022;

with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

-- AWS
with AWS.Client;
with AWS.Response;
use AWS;

-- VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
use VSS.JSON.Pull_Readers;
use VSS.JSON;
with VSS.Text_Streams; use VSS.Text_Streams;

-- Local Packages
with API.Memberships;
with API.Profiles; use API.Profiles; -- only for enums
with API.Manifest.Tools; use API.Manifest.Tools; -- only for enums
with API.Error_Codes; use API.Error_Codes; -- only for enums

with GUI.Character;
with GUI.Global;
with GUI.Base;

with JSON; use JSON;

with Shared; use Shared;

package body API.Transfers is
	-- Server Check
	procedure Server_Check (Data : AWS.Response.Data)
	is
		Stream : Memory_UTF8_Input_Stream_Access := Get_Stream (
			Response.Message_Body (Data));
		Reader : JSON_Simple_Pull_Reader;

		Error_Code : Error_Codes.Error_Code_Type;
	begin
		Set_Stream (Reader, Input_Text_Stream_Access (Stream));

		-- Check passes if S200 returned
		if Query_Status (Data) then
			Free (Stream);
			return;
		end if;

		Wait_Until_Key (Reader, "ErrorCode");
		Read_Next (Reader); -- NUMBER_VALUE
		Error_Code := Error_Code_Type'Enum_Val (
			As_Integer (
				Number_Value (Reader)));
		Free (Stream);

		Put_Line (Standard_Error,
			"[Error] API.Transfers got "
			& Error_Code'Image
			& " after passing all local checks");

		-- Wipe profile data and reload
		GUI.Lock_Object.Unlock;
		GUI.Base.Reload_Profile_Data;
		GUI.Lock_Object.Lock;
				
		case Error_Code is
			when DestinyNoRoomInDestination =>
				raise Out_Of_Space;
			when DestinyItemActionForbidden =>
				raise Actions_Disallowed;
			when DestinyItemNotFound =>
				raise Item_Not_Found;
			when DestinyItemNotTransferrable =>
				raise Cannot_Transfer;
			when others =>
				raise Unknown_Error;
		end case;
	end Server_Check;

	-- Local Checks
	-- These return no value and raise an exception if the check fails
	procedure Check_Character_Has_Room (
		Character : Profiles.Character_Type;
		D : Manifest.Tools.Item_Description)
	is
		Bucket_Item_Count : Natural renames GUI.Character.Item_Count (Character, D.Default_Bucket_Location);
		Max_Item_Count : Integer_32 renames GUI.The_Manifest.Destiny_Inventory_Buckets (D.Default_Bucket_Hash).Item_Count;
	begin
		-- +2 because space is needed for the equipped item and the new item
		if Integer_32 (Bucket_Item_Count + 2) > Max_Item_Count then
			raise Out_Of_Space;
		end if;
	end Check_Character_Has_Room;

	procedure Check_Vault_Has_Room (D : Manifest.Tools.Item_Description)
	is
		Bucket_Item_Count : Natural renames GUI.Global.Item_Count (D.Bucket_Location);
		Max_Item_Count : Integer_32 renames GUI.The_Manifest.Destiny_Inventory_Buckets (General'Enum_Rep).Item_Count;
	begin
		-- +1 because space is needed for the new item
		if Integer_32 (Bucket_Item_Count + 1) > Max_Item_Count then
			raise Out_Of_Space;
		end if;

		-- Check if adding this item would overflow a stack in the vault
		-- This only occurs for non-transferrable items
		if D.Transfer_Status /= Can_Transfer then
			begin
				if D.Quantity + GUI.Global.Get_Item_Stack (D.Item_Hash).Quantity > D.Max_Stack_Size then
					raise Out_Of_Space;
				end if;
			exception
				when GUI.Global.Item_Not_Found => null;
			end;
		end if;
	end Check_Vault_Has_Room;

	procedure Check_Actions_Permitted (D : Manifest.Tools.Item_Description)
	is begin
		if not D.Allow_Actions then
			raise Actions_Disallowed;
		end if;
	end Check_Actions_Permitted;

	-- The below subprograms perform both local and remote checks
	-- See the specification for more information
	
	procedure Vault (
		D : Manifest.Tools.Item_Description;
		Source : Profiles.Character_Type;
		Vault : Boolean := True)
	is
		Data : Response.Data;
	begin
		Put_Debug ("(Un)Vault item");

		Put_Debug ("{"
					& '"' & "itemReferenceHash" & '"' & ':'
						& D.Item_Hash'Image & ','
					& '"' & "stackSize" & '"' & ':'
						& D.Quantity'Image & ','
					& '"' & "transferToVault" & '"' & ':'
						& ' ' & (if Vault then "true" else "false") & ','
					& '"' & "itemId" & '"' & ':'
						& ' ' & (+D.Item_Instance_ID) & ','
					& '"' & "characterId" & '"' & ':'
						& ' ' & (+Source.Character_ID) & ','
					& '"' & "membershipType" & '"' & ':'
						& Memberships.Find_Default_Platform_ID (GUI.Membership)
				& "}");

		-- Local Check
		-- An exception will be raised if any of these fail

		Check_Actions_Permitted (D);
		-- Check_Item_Belongs_Elsewhere
		case D.Bucket_Location is
			when Consumable | Modification =>
				raise Already_Here;
			when others => null;
		end case;

		if Vault then
			Check_Vault_Has_Room (D);

			-- Check_Item_Not_Vaulted
			case D.Location is
				when Manifest.Vault => raise Already_Here;
				when others => null;
			end case;

			-- Check_Item_Can_Transfer
			case D.Transfer_Status is
				when Can_Transfer => null;
				when others =>
					case D.Bucket_Location is
						when Postmaster => null;
						when others => raise Cannot_Transfer;
					end case;
			end case;
		else
			Check_Character_Has_Room (Source, D);
		end if;

		Data := Client.Post (
			URL => Bungie_Root & API_Root & "/Destiny2/Actions/Items/TransferItem/",
			Data =>
				"{"
					& '"' & "itemReferenceHash" & '"' & ':'
						& D.Item_Hash'Image & ','
					& '"' & "stackSize" & '"' & ':'
						& D.Quantity'Image & ','
					& '"' & "transferToVault" & '"' & ':'
						& ' ' & (if Vault then "true" else "false") & ','
					& '"' & "itemId" & '"' & ':'
						& ' ' & (+D.Item_Instance_ID) & ','
					& '"' & "characterId" & '"' & ':'
						& ' ' & (+Source.Character_ID) & ','
					& '"' & "membershipType" & '"' & ':'
						& Memberships.Find_Default_Platform_ID (GUI.Membership)
				& "}",
			Headers => GUI.Headers);
		Server_Check (Data);
	end Vault;
	
	procedure Unvault (
		D : Manifest.Tools.Item_Description;
		Target : Profiles.Character_Type)
	is begin
		-- Local Check
		-- (No Additional Checks)
		Vault (D, Target, Vault => False);
	end Unvault;

	procedure Transfer (
		D : Manifest.Tools.Item_Description;
		Source, Target : Profiles.Character_Type)
	is begin
		-- Local Check
		-- (No Additonal Checks)

		Vault (D, Source);
		delay 0.1; -- Throttle timer
		Unvault (D, Target);
	end Transfer;

	procedure Postmaster_Pull (
		D : Manifest.Tools.Item_Description;
		Source : Profiles.Character_Type)
	is
		Data : Response.Data;
	begin
		Put_Debug ("Pull from Postmaster");

		-- Local Check
		-- An exception will be raised if any of these fail
		Check_Character_Has_Room (Source, D);
		Check_Actions_Permitted (D);

		-- The item will end up in the vault in this case
		if D.Transfer_Status /= Can_Transfer then
			Check_Vault_Has_Room (D);
		end if;

		Data := Client.Post (
			URL => Bungie_Root & API_Root & "/Destiny2/Actions/Items/PullFromPostmaster/",
			Data =>
				"{"
					& '"' & "itemReferenceHash" & '"' & ':'
						& D.Item_Hash'Image & ','
					& '"' & "stackSize" & '"' & ':'
						& D.Quantity'Image & ','
					& '"' & "itemId" & '"' & ':'
						& ' ' & (+D.Item_Instance_ID) & ','
					& '"' & "characterId" & '"' & ':'
						& ' ' & (+Source.Character_ID) & ','
					& '"' & "membershipType" & '"' & ':'
						& Memberships.Find_Default_Platform_ID (GUI.Membership)
				& "}",
			Headers => GUI.Headers);
		Server_Check (Data);
	end Postmaster_Pull;
	
	procedure Equip (
		D : Manifest.Tools.Item_Description;
		Source : Profiles.Character_Type)
	is
		Data : Response.Data;
	begin
		Put_Debug ("Equip item");

		-- Local Check
		-- An exception will be raised if any of these fail
		Check_Actions_Permitted (D);

		Data := Client.Post (
			URL => Bungie_Root & API_Root & "/Destiny2/Actions/Items/EquipItem/",
			Data =>
				"{"
					& '"' & "itemId" & '"' & ':'
						& ' ' & (+D.Item_Instance_ID) & ','
					& '"' & "characterId" & '"' & ':'
						& ' ' & (+Source.Character_ID) & ','
					& '"' & "membershipType" & '"' & ':'
						& Memberships.Find_Default_Platform_ID (GUI.Membership)
				& "}",
			Headers => GUI.Headers);
		Server_Check (Data);
	end Equip;
end API.Transfers;
