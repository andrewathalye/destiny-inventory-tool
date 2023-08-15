limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes;       use API.Definitions.Hashes;

package API.Definitions.Destiny_Place is
   ----------------------------
   -- DestinyPlaceDefinition --
   ----------------------------
   type Destiny_Place_Definition is record
      Description : Unbounded_String;
      Name        : Unbounded_String;
   end record;

   package DPDM is new Ada.Containers.Ordered_Maps
     (Destiny_Place_Definition_Manifest_Hash, Destiny_Place_Definition);
   subtype Destiny_Place_Map is DPDM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Place;
