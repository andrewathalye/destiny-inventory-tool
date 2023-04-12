-- Local Packages
with API.Manifest.Tools;
with API.Profiles;

package API.Transfers is
	-- Exceptions
	Out_Of_Space : exception; -- DestinyNoRoomInDestination
	Already_Here : exception;
	Cannot_Transfer : exception; -- DestinyItemNotTransferrable
	Actions_Disallowed : exception; -- DestinyItemActionForbidden
	Item_Not_Found : exception; -- DestinyItemNotFound

	Unknown_Error : exception;

	-- Note: The below subprograms perform local checks
	-- for consistency and also interpret server responses.
	--
	-- If a local check passed but a serverside check failed,
	-- then an exception will be raised and the state will be
	-- synchronised (e.g. Profile redownloaded, inventories
	-- cleared, etc.)
	
	procedure Vault (
		D : Manifest.Tools.Item_Description;
		Source : Profiles.Character_Type;
		Vault : Boolean := True);
	
	-- Provided for convenience
	procedure Unvault (
		D : Manifest.Tools.Item_Description;
		Target : Profiles.Character_Type);

	-- Provided for convenience
	procedure Transfer (
		D : Manifest.Tools.Item_Description;
		Source,Target : Profiles.Character_Type);

	procedure Postmaster_Pull (
		D : Manifest.Tools.Item_Description;
		Source : Profiles.Character_Type);
	
	procedure Equip (
		D : Manifest.Tools.Item_Description;
		Source : Profiles.Character_Type);
end API.Transfers;
