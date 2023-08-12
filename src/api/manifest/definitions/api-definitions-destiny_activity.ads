limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes; use API.Definitions.Hashes;

package API.Definitions.Destiny_Activity is
-------------------------------
   -- DestinyActivityDefinition --
   -------------------------------
   type Destiny_Activity_Definition is record
      Description : Unbounded_String;
      Name        : Unbounded_String;
      --  Many Fields Omitted
      Destination_Hash : Destiny_Destination_Definition_Manifest_Hash;
      Place_Hash       : Destiny_Place_Definition_Manifest_Hash;
   end record;

   package DADM is new Ada.Containers.Ordered_Maps
     (Destiny_Activity_Definition_Manifest_Hash, Destiny_Activity_Definition);
   subtype Destiny_Activity_Map is DADM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Activity;
