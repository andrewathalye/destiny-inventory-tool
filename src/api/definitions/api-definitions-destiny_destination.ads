limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes;       use API.Definitions.Hashes;

package API.Definitions.Destiny_Destination is
   ----------------------------------
   -- DestinyDestinationDefinition --
   ----------------------------------
   type Destiny_Bubble_Hash_Type is new Unsigned_32;

   type Destiny_Bubble_Definition is record
      Hash        : Destiny_Bubble_Hash_Type;
      Description : Unbounded_String;
      Name        : Unbounded_String;
   end record;

   package DBDL is new Ada.Containers.Vectors
     (Natural, Destiny_Bubble_Definition);
   subtype Destiny_Bubble_List is DBDL.Vector;

   type Destiny_Destination_Definition is record
      Description                    : Unbounded_String;
      Name                           : Unbounded_String;
      Place_Hash                     : Destiny_Place_Definition_Manifest_Hash;
      Default_Freeroam_Activity_Hash : Destiny_Activity_Definition_Manifest_Hash;
      --  activityGraphEntries?
      Bubbles : Destiny_Bubble_List;
   end record;

   package DDDM is new Ada.Containers.Ordered_Maps
     (Destiny_Destination_Definition_Manifest_Hash,
      Destiny_Destination_Definition);
   subtype Destiny_Destination_Map is DDDM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Destination;
