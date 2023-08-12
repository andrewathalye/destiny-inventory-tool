with Interfaces; use Interfaces;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

pragma Warnings (Off, "never instantiated");
with Ada.Containers.Ordered_Maps;
pragma Unreferenced (Ada.Containers.Ordered_Maps);

package API.Definitions is
   -----------
   -- BASIC --
   -----------
   type Quantity_Type is range -1 .. Integer_32'Last;

   package USL is new Ada.Containers.Vectors (Natural, Unbounded_String);
   subtype Unbounded_String_List is USL.Vector;
end API.Definitions;
