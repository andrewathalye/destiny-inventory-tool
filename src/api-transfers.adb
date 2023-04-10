-- AWS
with AWS.Client;
with AWS.Response;
use AWS;

-- Local Packages
with API.Memberships;
with GUI;
with Shared; use Shared;

package body API.Transfers is
	-- TODO: Use VSS
	procedure Vault (
		D : Manifest.Tools.Item_Description;
		Source : Profiles.Character_Type;
		Vault : Boolean := True)
	is
		Data : Response.Data;
	begin
		Put_Debug ("Vault item");
		Data := Client.Post (
			URL => API_Root & "/Destiny2/Actions/Items/TransferItem/",
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
		Check_Status (Data);
	end Vault;
	
	procedure Unvault (
		D : Manifest.Tools.Item_Description;
		Target : Profiles.Character_Type)
	is begin
		Vault (D, Target, Vault => False);
	end Unvault;

	procedure Transfer (
		D : Manifest.Tools.Item_Description;
		Source, Target : Profiles.Character_Type)
	is begin
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
		Data := Client.Post (
			URL => API_Root & "/Destiny2/Actions/Items/PullFromPostmaster/",
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
		Check_Status (Data);
	end Postmaster_Pull;
	
	procedure Equip (
		D : Manifest.Tools.Item_Description;
		Source : Profiles.Character_Type)
	is
		Data : Response.Data;
	begin
		Put_Debug ("Equip item");
		Data := Client.Post (
			URL => API_Root & "/Destiny2/Actions/Items/EquipItem/",
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
		Check_Status (Data);
	end Equip;
end API.Transfers;
