with API.Definitions.Destiny_Activity; use API.Definitions.Destiny_Activity;
with API.Definitions.Destiny_Class;    use API.Definitions.Destiny_Class;
with API.Definitions.Destiny_Damage_Type;
use API.Definitions.Destiny_Damage_Type;
with API.Definitions.Destiny_Destination;
use API.Definitions.Destiny_Destination;
with API.Definitions.Destiny_Faction; use API.Definitions.Destiny_Faction;
with API.Definitions.Destiny_Gender;  use API.Definitions.Destiny_Gender;
with API.Definitions.Destiny_Inventory_Bucket;
use API.Definitions.Destiny_Inventory_Bucket;
with API.Definitions.Destiny_Inventory_Item;
use API.Definitions.Destiny_Inventory_Item;
with API.Definitions.Destiny_Objective; use API.Definitions.Destiny_Objective;
with API.Definitions.Destiny_Place;     use API.Definitions.Destiny_Place;
with API.Definitions.Destiny_Race;      use API.Definitions.Destiny_Race;
with API.Definitions.Destiny_Record;    use API.Definitions.Destiny_Record;
with API.Definitions.Destiny_Stat;      use API.Definitions.Destiny_Stat;
with API.Definitions.Destiny_Vendor;    use API.Definitions.Destiny_Vendor;
with API.Definitions.Destiny_Vendor_Group;
use API.Definitions.Destiny_Vendor_Group;

package API.Manifest is
   -------------
   -- CENTRAL --
   -------------
   --  Available Manifest fields (now sorted alphabetically)
   --  Most data types do not include all available information
   --  See individual definitions for more information

   type Manifest_Type is record
      Destiny_Activities        : Destiny_Activity_Map;
      Destiny_Classes           : Destiny_Class_Map;
      Destiny_Damage_Types      : Destiny_Damage_Type_Map;
      Destiny_Destinations      : Destiny_Destination_Map;
      Destiny_Factions          : Destiny_Faction_Map;
      Destiny_Genders           : Destiny_Gender_Map;
      Destiny_Inventory_Buckets : Destiny_Inventory_Bucket_Map;
      Destiny_Inventory_Items   : Destiny_Inventory_Item_Map;
      Destiny_Objectives        : Destiny_Objective_Map;
      Destiny_Places            : Destiny_Place_Map;
      Destiny_Races             : Destiny_Race_Map;
      Destiny_Records           : Destiny_Record_Map;
      Destiny_Stats             : Destiny_Stat_Map;
      Destiny_Vendors           : Destiny_Vendor_Map;
      Destiny_Vendor_Groups     : Destiny_Vendor_Group_Map;

      --  TODO:
      --  DestinySocketTypeDefinition
      --  DestinyProgressionDefinition
   end record;

   --  Subprograms
   function Get_Manifest return Manifest_Type;

private
   Current_Manifest_Format_Version : constant := 6;
end API.Manifest;
