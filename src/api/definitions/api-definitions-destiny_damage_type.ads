limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes;       use API.Definitions.Hashes;

package API.Definitions.Destiny_Damage_Type is
   ---------------------------------
   -- DestinyDamageTypeDefinition --
   ---------------------------------
   type Destiny_Damage_Type_Definition is record
      Description : Unbounded_String;
      Name        : Unbounded_String;
      Icon_Path   : Unbounded_String; --  Nullable
      Show_Icon   : Boolean;
   end record;

   package DDTDM is new Ada.Containers.Ordered_Maps
     (Key_Type     => Destiny_Damage_Type_Definition_Manifest_Hash,
      Element_Type => Destiny_Damage_Type_Definition);
   subtype Destiny_Damage_Type_Map is DDTDM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Damage_Type;
