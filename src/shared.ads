with Ada.Containers;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;

-- VSS
with VSS.Strings.Conversions; use VSS.Strings.Conversions;
use VSS.Strings;

package Shared is
	-- Hashed Map Functions
	function Hash (Key : Unbounded_String) return Ada.Containers.Hash_Type is
		(Ada.Strings.Hash (To_String (Key)));
	
	function Equivalent_Key (L, R : Unbounded_String) return Boolean is
		(L = R);

	-- Debug Utilities
	procedure Put_Debug (Item : String);
	function Read_File (Name : String) return String;

	-- Cache Utilities
	function Get_Cached (Name : String) return Stream_Element_Array;
	function Has_Cached (Name : String) return Boolean;
	procedure Cache (Name : String; Content : Stream_Element_Array);


	-- Unbounded / Bounded conversion
	function "+" (Item : String) return Unbounded_String renames To_Unbounded_String;
	function "+" (Item : Unbounded_String) return String renames To_String;

	-- VSS conversion functions
	function VS2UB (Item : Virtual_String'Class) return Unbounded_String renames To_Unbounded_UTF_8_String ;
	function VS2S (Item : Virtual_String'Class) return String renames To_UTF_8_String ;
end Shared;
