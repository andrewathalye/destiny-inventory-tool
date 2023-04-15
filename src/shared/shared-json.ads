with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;

with Unchecked_Deallocation;

-- VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
use VSS.JSON.Pull_Readers;
with VSS.Text_Streams.Memory_UTF8_Input; use VSS.Text_Streams.Memory_UTF8_Input;

-- Local Packages
with Shared.Strings;

package Shared.JSON is
	-- Types
	type Memory_UTF8_Input_Stream_Access is access Memory_UTF8_Input_Stream;
	type Unbounded_String_Array is array (Natural range <>) of Unbounded_String;

	-- Instantiated Subprograms
	procedure Free is new Unchecked_Deallocation (
		Memory_UTF8_Input_Stream,
		Memory_UTF8_Input_Stream_Access);

	package USL is new Ada.Containers.Hashed_Maps (
		Key_Type => Unbounded_String,
		Element_Type => Unbounded_String,
		Hash => Shared.Strings.Hash,
		Equivalent_Keys => Shared.Strings.Equivalent_Keys);
	pragma Obsolescent (USL);
	
	subtype Unbounded_String_List is USL.Map;
	pragma Obsolescent (Unbounded_String_List);

	-- Subprograms
	-- Note: Returned Stream_Access must be Freed
	function Get_Stream (JSON_Data : Unbounded_String) return Memory_UTF8_Input_Stream_Access;

	function Get_Strings (
		JSON_Data : Unbounded_String;
		Keys : Unbounded_String_Array) return Unbounded_String_List;
	pragma Obsolescent (Get_Strings);

	procedure Wait_Until_Key (Reader : in out JSON_Simple_Pull_Reader; Key : String);
	procedure Wait_Until_Event (Reader : in out JSON_Simple_Pull_Reader; Event : JSON_Event_Kind);
end Shared.JSON;
