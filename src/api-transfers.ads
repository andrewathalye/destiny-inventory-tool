with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Local Packages
with API.Manifest.Tools;

package API.Transfers is
	procedure Transfer (
		D : Manifest.Tools.Item_Description;
		Source : Unbounded_String;
		Target : Unbounded_String)
	
	procedure Vault (
		D : Manifest.Tools.Item_Description;
		Source : Unbounded_String);
	
	procedure Unvault (
		D : Manifest.Tools.Item_Description;
		Source : Unbounded_String);
	
	procedure Equip (
		D : Manifest.Tools.Item_Description;
		Source : Unbounded_String);
end API.Transfers;
