with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function API.Manifest.Fetch
  (Localised_Manifest_Path : Unbounded_String) return Manifest_Type;
