limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes;         use API.Definitions.Hashes;
with API.Definitions.Destiny_Gender; use API.Definitions.Destiny_Gender;

package API.Definitions.Destiny_Race is
   ---------------------------
   -- DestinyRaceDefinition --
   ---------------------------
   type Destiny_Race_Name is array (Destiny_Gender_Type) of Unbounded_String;
   package DRNM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Race_Definition_Manifest_Hash,
      Element_Type => Destiny_Race_Name);
   subtype Destiny_Race_Map is DRNM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Race;
