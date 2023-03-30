pragma Ada_2022;

with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Ada.Directories; use Ada.Directories;

-- AWS
with AWS.URL;
use AWS;

package body Shared is
	-- Debug Utilities
	procedure Put_Debug (Item : String) is
		use Ada.Text_IO;
	begin
		Put_Line ("[DEBUG] " & Item);
	end Put_Debug;

	function Read_File (Name : String) return String is
		use Ada.Streams.Stream_IO;

		SF : File_Type;
		S : Stream_Access;
		Text : String (1 .. Natural (Size (Name)));
	begin
		Open (SF, In_File, Name);
		S := Stream (SF);
		
		String'Read (S, Text);
		Close (SF);

		return Text;
	end Read_File;
	pragma Obsolescent (Read_File);

	-- Cache Utilities
	function Has_Cached (Name : String) return Boolean
	is (Exists ("cache/" & URL.Encode (Name)));

	function Get_Data (Name : String) return Stream_Element_Array
	is
		use Ada.Streams.Stream_IO;

		SF : File_Type;
		S : Stream_Access;
		SEA : Stream_Element_Array (1 .. Stream_Element_Offset (Size (Name)));
	begin
		Open (SF, In_File, Name);
		S := Stream (SF);

		Stream_Element_Array'Read (S, SEA);
		Close (SF);

		return SEA;
	end Get_Data;

	function Get_Cache_Path (Name : String) return String
	is ("cache/" & URL.Encode (Name));

	function Get_Cached (Name : String) return Stream_Element_Array
	is (Get_Data (Get_Cache_Path (Name)));
	
	procedure Cache (Name : String; Content : Stream_Element_Array)
	is
		use Ada.Streams.Stream_IO;

		SF : File_Type;
		S : Stream_Access;
	begin
		if Has_Cached (Name) then
			raise Program_Error;
		end if;

		if not Exists ("cache/") then
			Create_Directory ("cache");
		end if;

		-- Write to disk cache
		Create (SF, Out_File, "cache/" & URL.Encode (Name));
		S := Stream (SF);
		Stream_Element_Array'Write (S, Content);
		Close (SF);
	end Cache;
end Shared;
