pragma Ada_2022;

with Ada.Streams; use Ada.Streams;
with Ada.Containers.Vectors;
with System;

-- Gtkada
with Gtk.Main; use Gtk.Main;
with Gtk.Image; use Gtk.Image;
with Gtk.Label; use Gtk.Label;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Button; use Gtk.Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Container; use Gtk.Container;

with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib.Error; use Glib.Error;
use Glib;

-- AWS
with AWS.Client;
with AWS.Response;
use AWS;

-- Local Packages
with API.Manifest.Tools;
use API.Manifest;
use API;
with Shared; use Shared;

package body GUI is
	-- State Data
	Character_Index : Natural := 0;

	function "=" (L, R : Manifest.Tools.Item_Description) return Boolean is (False);

	package IDV is new Ada.Containers.Vectors (Natural, Manifest.Tools.Item_Description);
	subtype Item_Description_List is IDV.Vector;
	Bucket_Items : Item_Description_List;

	type Bucket_Location is (
		Kinetic,
		Energy,
		Power,
		Helmet,
		Gauntlets,
		Chest,
		Leg,
		Class,
		Unknown); -- TODO Add a lot more :)

	for Bucket_Location use (
		Kinetic => 20,
		Energy => 30,
		Power => 40,
		Helmet => 50,
		Gauntlets => 60,
		Chest => 70,
		Leg => 80,
		Class => 90,
		Unknown => 9999);

	Character_Items : array (Bucket_Location) of Item_Description_List;

	-- Private Subprograms
	-- Exclusively for JPEG / PNG format images
	function Load_Image (File_Name : String; Data : Stream_Element_Array) return Gdk_Pixbuf
	is
		-- Imported Subprograms
		type Pixbuf_Loader is null record;

		function Gdk_Pixbuf_Loader_New_With_Type (
			Image_Type : String;
			Error : access GError) return access Pixbuf_Loader
		with
			Import => True,
			Convention => C;

		function Gdk_Pixbuf_Loader_Write (
			Loader : access Pixbuf_Loader;
			Buffer : System.Address;
			Count : GSize;
			Error : access GError) return GBoolean
		with
			Import => True,
			Convention => C;

		function Gdk_Pixbuf_Loader_Get_Pixbuf (
			Loader : access Pixbuf_Loader) return System.Address
		with
			Import => True,
			Convention => C;

		function Gdk_Pixbuf_Loader_Close (
			Loader : access Pixbuf_Loader;
			Error : access GError) return GBoolean
		with
			Import => True,
			Convention => C;

		-- Constants
		FType : constant String := (
			if File_Name (File_Name'Last -2 .. File_Name'Last) = "png" then
				"png"
			else
				"jpeg");

		-- Variables
		Loader : access Pixbuf_Loader;
		Pixbuf : System.Address;
		Discard : GBoolean;
	begin
		Loader := Gdk_Pixbuf_Loader_New_With_Type (FType & ASCII.NUL, null);

		Discard := Gdk_Pixbuf_Loader_Write (Loader, Data (Data'First)'Address, Data'Length, null);

		Pixbuf := Gdk_Pixbuf_Loader_Get_Pixbuf (Loader);

		if Gdk_Pixbuf'(Convert (Pixbuf)) = null then
			raise Program_Error with "Image could not be processed";
		end if;

		Discard := Gdk_Pixbuf_Loader_Close (Loader, null);
		
		return Convert (Pixbuf);
	end Load_Image;

	procedure Remove_Callback (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class; Grid : Gtk_Grid)
	is begin
		Grid.Remove (Widget);
	end Remove_Callback;

	procedure Clear_Bucket (G : Gtk_Grid)
	is 
		package FUD_Grid is new Gtk.Container.Foreach_User_Data (Gtk_Grid);
	begin
		FUD_Grid.Foreach (G, Remove_Callback'Access, G);
	end Clear_Bucket;

	-- Public Subprograms

	procedure Window_Close_Handler (Builder : access Gtkada_Builder_Record'Class) is
	begin
		GTK.Main.Main_Quit;
	end Window_Close_Handler;

	procedure Switch_Button_Clicked_Handler (Builder : access Gtkada_Builder_Record'Class) is
	begin
		Character_Index := @ + 1;

		if Character_Index >= Natural (Profile.Characters.Length) then
			Character_Index := 0;
		end if;

		Update_For_Character (Profile.Characters.Element (Character_Index));
	end Switch_Button_Clicked_Handler;

	procedure Render_Items (List : Item_Description_List; Bucket : Gtk_Grid) is
		Left : Gint := 0;
		Top : Gint := 0;
	begin
		for D of List loop
			declare
				Button : Gtk_Button;
				Image : Gtk_Image;
			begin
				Gtk_New (Button);
				Gtk_New (Image);
				
				if Has_Cached (+D.Icon_Path) then
--					Put_Debug ("Load cached icon");
					Set (Image, Load_Image (
						+D.Icon_Path,
						Get_Cached (+D.Icon_Path)));
				else
					declare
						Data : Response.Data;
					begin
						Put_Debug ("Get icon");
						Data := Client.Get (Bungie_Root & (+D.Icon_Path));
						Cache (+D.Icon_Path, Response.Message_Body (Data));
						Set (Image, Load_Image (
							+D.Icon_Path,
							Response.Message_Body (Data)));
					end;
				end if;

				Set_Image (Button, Image);

				Show (Button);
				Bucket.Attach (Button, Left, Top);

				Left := @ + 1;

				if Left = 3 then
					Left := 0;
					Top := @ + 1;
				end if;
			end;
		end loop;

		Show (Bucket);
	end Render_Items;

	procedure Update_For_Character (Character : Profiles.Character_Type) is
		-- Labels and Images to be updated for each character
		Title : Gtk_Label := Gtk_Label (Builder.Get_Object ("title"));
		Light : Gtk_Label := Gtk_Label (Builder.Get_Object ("light"));
		Emblem : Gtk_Image := Gtk_Image (Builder.Get_Object ("emblem"));

		-- Buckets (Grids) that need to be updated
		Kinetic_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("kinetic"));
		Energy_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("energy"));
		Power_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("power"));

		Helmet_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("helmet"));
		Gauntlets_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("gauntlets"));
		Chest_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("chest"));
		Leg_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("leg"));
		Class_Bucket : Gtk_Grid := Gtk_Grid (Builder.Get_Object ("class"));
	begin
		-- Update Labels
		Set_Label (Title, +Manifest.Tools.Get_Title (The_Manifest, Character));
		Set_Label (Light, Character.Light'Image);

		-- Update Emblem
		if Has_Cached (+Character.Emblem_Background_Path) then
			Put_Debug ("Load cached emblem");
			Set (Emblem, Load_Image (
				+Character.Emblem_Background_Path,
				Get_Cached (+Character.Emblem_Background_Path)));
		else
			declare
				Data : Response.Data;
			begin
				Put_Debug ("Get emblem");
				Data := Client.Get (Bungie_Root & (+Character.Emblem_Background_Path));
				Cache (+Character.Emblem_Background_Path, Response.Message_Body (Data));
				Set (Emblem, Load_Image (
					+Character.Emblem_Background_Path,
					Response.Message_Body (Data)));
			end;
		end if;

		-- Update Items
		for IDL of Character_Items loop
			IDL.Clear;
		end loop;

		-- Inventory Items (not equipped)
		for I of Profile.Character_Inventories (Character.Character_ID) loop
			declare
				D : Manifest.Tools.Item_Description := Manifest.Tools.Get_Description (The_Manifest, I);
			begin
				if D.Category = Manifest.Equippable then
					Character_Items (Bucket_Location'Enum_Val (D.Bucket_Order)).Append (D);	
				end if;
			exception
				when Constraint_Error =>
					Character_Items (Unknown).Append (D);
			end;
		end loop;

		Clear_Bucket (Kinetic_Bucket);
		Render_Items (Character_Items (Kinetic), Kinetic_Bucket);

		Clear_Bucket (Energy_Bucket);
		Render_Items (Character_Items (Energy), Energy_Bucket);

		Clear_Bucket (Power_Bucket);
		Render_Items (Character_Items (Power), Power_Bucket);

		Clear_Bucket (Helmet_Bucket);
		Render_Items (Character_Items (Helmet), Helmet_Bucket);

		Clear_Bucket (Gauntlets_Bucket);
		Render_Items (Character_Items (Gauntlets), Gauntlets_Bucket);

		Clear_Bucket (Chest_Bucket);
		Render_Items (Character_Items (Chest), Chest_Bucket);

		Clear_Bucket (Leg_Bucket);
		Render_Items (Character_Items (Leg), Leg_Bucket);

		Clear_Bucket (Class_Bucket);
		Render_Items (Character_Items (Class), Class_Bucket);

	end Update_For_Character;
end GUI;
