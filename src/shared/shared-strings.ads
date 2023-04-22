with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;

-- VSS
with VSS.Strings;
with VSS.Strings.Conversions;

package Shared.Strings is
   -- Hashed Map Functions
   function Hash (Key : Unbounded_String) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (To_String (Key)));

   function Equivalent_Keys (L, R : Unbounded_String) return Boolean is
     (L = R);

   -- Unbounded / Bounded conversion
   function "+" (Item : String) return Unbounded_String renames
     To_Unbounded_String;
   function "+" (Item : Unbounded_String) return String renames To_String;

   -- VSS conversion functions
   -- These are used very frequently by the JSON parsers, so
   -- their names are heavily abbreviated
   function VS2UB
     (Item : VSS.Strings.Virtual_String'Class) return Unbounded_String renames
     VSS.Strings.Conversions.To_Unbounded_UTF_8_String;
   function VS2S
     (Item : VSS.Strings.Virtual_String'Class) return String renames
     VSS.Strings.Conversions.To_UTF_8_String;
end Shared.Strings;
