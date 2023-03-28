with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- VSS
with VSS.Strings.Conversions; use VSS.Strings.Conversions;
use VSS.Strings;

package Shared is
	procedure Put_Debug (Item : String);
	function Read_File (Name : String) return String;

	-- Unbounded / Bounded conversion
	function "+" (Item : String) return Unbounded_String renames To_Unbounded_String;
	function "+" (Item : Unbounded_String) return String renames To_String;

	-- VSS conversion functions
	function VS2UB (Item : Virtual_String'Class) return Unbounded_String renames To_Unbounded_UTF_8_String ;
	function VS2S (Item : Virtual_String'Class) return String renames To_UTF_8_String ;
end Shared;
