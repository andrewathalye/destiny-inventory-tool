pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Local Packages
with API.Authorise;
with API.Memberships;
with API.Manifest;
with API.Manifest.Tools;
with API.Profiles;
with API; use API;
with Shared; use Shared;

procedure Inventory_Tool is
	-- Constants
--	Auth_Data : constant Auth_Storage_Type := Authorise.Do_Authorise;
	Auth_Data : Auth_Storage_Type;
	Headers : constant Auth_Header_Type := Create_Headers (Auth_Data);

	Membership : constant Memberships.Membership_Type := Memberships.Get_Memberships (Headers);
	Profile : Profiles.Profile_Type := Profiles.Get_Profile (Headers, Membership);
	M : constant Manifest.Manifest_Type := Manifest.Get_Manifest (Membership);
begin
	-- Print Welcome Message
	Put_Line ("Destiny Inventory Tool v0.1");
	Put_Line ("Welcome back " & (+Membership.Bungie_Net_User.Unique_Name) & "!");
	Put_Line ("Your default platform appears to be "
		& Memberships.Find_Default_Platform (Membership)'Image & ".");
	Put_Line ("Global Inventory:");
	declare
		D : Manifest.Tools.Item_Description;
	begin
		for I of Profile.Profile_Currencies loop
			D := Manifest.Tools.Get_Description (M, I);
			Put_Line (
				D.Quantity'Image & "x "
				& (+D.Name)
				& " | " & (+D.Item_Type_And_Tier_Display_Name));

		end loop;
	end;

	Put_Line ("Active Characters:");

	for C of Profile.Characters loop
		Put_Line (Manifest.Tools.Get_Description (M, C) & " -" & C.Light'Image);
		declare
			D : Manifest.Tools.Item_Description;
		begin
			for I of Profile.Character_Inventories (C.Character_ID) loop
				D := Manifest.Tools.Get_Description (M, I);
				Put_Line (
					D.Quantity'Image & "x "
					& (+D.Name)
					& " | " & (+D.Item_Type_And_Tier_Display_Name));
			end loop;
			raise Program_Error;
		end;
	end loop;

	-- Print Current Loadout on Active Character
	
	-- Command Line Input
end Inventory_Tool;
