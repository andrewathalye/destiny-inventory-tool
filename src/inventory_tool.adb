pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Local Packages
with API.Authorise;
with API.Memberships;
with API.Profiles;
with API; use API;
with Shared; use Shared;

procedure Inventory_Tool is
	-- Constants
--	Auth_Data : constant Auth_Storage_Type := Authorise.Do_Authorise;
	Auth_Data : Auth_Storage_Type;
	Headers : constant Auth_Header_Type := Create_Headers (Auth_Data);

	M : constant Memberships.Membership_Type := Memberships.Get_Memberships (Headers);
	P : Profiles.Profile_Type := Profiles.Get_Profiles (Headers, M);
begin
	-- Print Welcome Message
	Put_Line ("Destiny Inventory Tool v0.1");
	Put_Line ("Welcome back " & (+M.Bungie_Net_User.Unique_Name) & "!");
	Put_Line ("Your default platform appears to be "
		& Memberships.Find_Default_Platform (M)'Image & ".");

	-- Print Current Loadout on Active Character
	
	-- Command Line Input
end Inventory_Tool;
