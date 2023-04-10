-- Local Packages
with API.Manifest.Tools;
with API.Profiles;

package API.Transfers is
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
