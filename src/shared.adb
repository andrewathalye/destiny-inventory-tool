with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Ada.Directories; use Ada.Directories;

package body Shared is
	procedure Put_Debug (Item : String) is
		use Ada.Text_IO;
	begin
		Put_Line ("[DEBUG] " & Item);
	end Put_Debug;

	function Read_File (Name : String) return String is
		use Ada.Streams.Stream_IO;
		use Ada.Streams;

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
end Shared;
