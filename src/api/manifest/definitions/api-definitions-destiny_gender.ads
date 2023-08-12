limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes; use API.Definitions.Hashes;

package API.Definitions.Destiny_Gender is
   -----------------------------
   -- DestinyGenderDefinition --
   -----------------------------
   type Destiny_Gender_Type is (Male, Female);

   type Destiny_Gender_Definition is record
      Gender_Type : Destiny_Gender_Type;
      Gender_Name : Unbounded_String;
   end record;

   package DGDM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Gender_Definition_Manifest_Hash,
      Element_Type => Destiny_Gender_Definition);
   subtype Destiny_Gender_Map is DGDM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Gender;
