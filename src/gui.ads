-- Gtkada
with Gtkada.Builder; use Gtkada.Builder;

-- Local Packages
with API.Authorise;
with API.Profiles;
with API.Manifest;
with API.Memberships;
use API;

package GUI is
	-- Variables
--	Auth_Data : constant Auth_Storage_Type := Authorise.Do_Authorise;
	Auth_Data : Auth_Storage_Type;
	Headers : constant Auth_Header_Type := Create_Headers (Auth_Data);

	Membership : constant Memberships.Membership_Type := Memberships.Get_Memberships (Headers);
	Profile : Profiles.Profile_Type := Profiles.Get_Profile (Headers, Membership);
	The_Manifest : constant Manifest.Manifest_Type := Manifest.Get_Manifest (Membership);

	Builder : Gtkada_Builder; -- Left unitialised

	-- Subprograms
	procedure Window_Close_Handler (Builder : access Gtkada_Builder_Record'Class);
	procedure Switch_Button_Clicked_Handler (Builder : access Gtkada_Builder_Record'Class);

	procedure Update_For_Character (Character : Profiles.Character_Type);
end GUI;
