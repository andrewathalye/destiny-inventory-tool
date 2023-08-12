limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes; use API.Definitions.Hashes;

package API.Definitions.Destiny_Faction is
------------------------------
   -- DestinyFactionDefinition --
   ------------------------------
   type Destiny_Faction_Definition is record
      Description      : Unbounded_String;
      Name             : Unbounded_String;
      Icon_Path        : Unbounded_String; --  Nullable
      Progression_Hash : Destiny_Progression_Definition_Manifest_Hash;
   end record;

   package DFDM is new Ada.Containers.Ordered_Maps
     (Destiny_Faction_Definition_Manifest_Hash, Destiny_Faction_Definition);
   subtype Destiny_Faction_Map is DFDM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Faction;
