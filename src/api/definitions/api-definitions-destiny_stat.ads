limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes;       use API.Definitions.Hashes;

package API.Definitions.Destiny_Stat is
   ---------------------------
   -- DestinyStatDefinition --
   ---------------------------
   --  At the moment, only the name of the stat is computed / stored
   package Destiny_Stat_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Stat_Definition_Manifest_Hash,
      Element_Type => Unbounded_String);
   subtype Destiny_Stat_Map is Destiny_Stat_Maps.Map;

   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Stat;
