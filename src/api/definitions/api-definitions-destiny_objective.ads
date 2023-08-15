limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes;       use API.Definitions.Hashes;

package API.Definitions.Destiny_Objective is
   --------------------------------
   -- DestinyObjectiveDefinition --
   --------------------------------
   type Destiny_Objective_Definition is record
      Icon_Path            : Unbounded_String; --  Nullable
      Progress_Description : Unbounded_String; --  Nullable
   end record;

   package DODM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Objective_Definition_Manifest_Hash,
      Element_Type => Destiny_Objective_Definition);
   subtype Destiny_Objective_Map is DODM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Objective;
