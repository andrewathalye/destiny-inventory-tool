limited with API.Manifest;
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
with API.Definitions.Hashes;       use API.Definitions.Hashes;
with API.Definitions.Destiny_Inventory_Item;
use API.Definitions.Destiny_Inventory_Item;

package API.Definitions.Destiny_Vendor is
   -----------------------------
   -- DestinyVendorDefinition --
   -----------------------------
   type Vendor_Item_Index_Type is new Natural;
   type Failure_Index_Type is new Natural;
   type Display_Category_Index_Type is new Integer range -1 .. Integer'Last;

   package Failure_Index_Lists is new Ada.Containers.Vectors
     (Natural, Failure_Index_Type);
   subtype Failure_Index_List is Failure_Index_Lists.Vector;

   package Failure_String_Lists is new Ada.Containers.Vectors
     (Failure_Index_Type, Unbounded_String);
   subtype Failure_String_List is Failure_String_Lists.Vector;

   --  At this time, no other information is needed
   --  but that might change.
   type Destiny_Display_Category_Definition is record
      Name : Unbounded_String;
   end record;

   package DDCDM is new Ada.Containers.Ordered_Maps
     (Display_Category_Index_Type, Destiny_Display_Category_Definition);
   subtype Destiny_Display_Category_Map is DDCDM.Map;

   type Destiny_Vendor_Item_Socket_Override_Type is record
      Single_Item_Hash : Destiny_Inventory_Item_Definition_Manifest_Hash :=
        0; --  Nullable
      Randomized_Options_Count : Quantity_Type;
      Socket_Type_Hash         : Destiny_Socket_Type_Definition_Manifest_Hash;
   end record;

   package DVISOL is new Ada.Containers.Vectors
     (Natural, Destiny_Vendor_Item_Socket_Override_Type);
   subtype Destiny_Vendor_Item_Socket_Override_List is DVISOL.Vector;

   type Destiny_Vendor_Item_Definition is record
      Item_Hash       : Destiny_Inventory_Item_Definition_Manifest_Hash;
      Quantity        : Quantity_Type;
      Failure_Indexes : Failure_Index_List;
      --  Currencies? IMO best to avoid because live data is better here
      Display_Category_Index : Display_Category_Index_Type;
      --  Points to a Destiny_Display_Category_Definition within Display_Categories
      --  Category_Index?
      Socket_Overrides : Destiny_Vendor_Item_Socket_Override_List;
   end record;

   package DVIDM is new Ada.Containers.Ordered_Maps
     (Vendor_Item_Index_Type, Destiny_Vendor_Item_Definition);
   subtype Destiny_Vendor_Item_Map is DVIDM.Map;

   type Destiny_Vendor_Location_Definition is record
      Destination_Hash      : Destiny_Destination_Definition_Manifest_Hash;
      Background_Image_Path : Unbounded_String;
   end record;

   package DVLL is new Ada.Containers.Vectors
     (Natural, Destiny_Vendor_Location_Definition);
   subtype Destiny_Vendor_Location_List is DVLL.Vector;

   type Destiny_Vendor_Definition is record
      Large_Icon_Path : Unbounded_String; --  Nullable
      Subtitle        : Unbounded_String;
      --  largeTransparentIcon?
      Description       : Unbounded_String;
      Name              : Unbounded_String;
      Icon_Path         : Unbounded_String; --  Nullable
      Display_Item_Hash : Destiny_Inventory_Item_Definition_Manifest_Hash; --  Nullable
      Inhibit_Buying    : Boolean;
      Inhibit_Selling   : Boolean;
      Faction_Hash      : Destiny_Faction_Definition_Manifest_Hash :=
        0; --  Nullable
      Failure_Strings : Failure_String_List;
      --  Vendor_Portrait? Vendor_Banner?
      Enabled : Boolean;
      Visible : Boolean;
      --  Categories?
      Display_Categories : Destiny_Display_Category_Map;
      --  Indexed by Hash
      Items : Destiny_Vendor_Item_Map;
      --  Indexed by Vendor Item Index (may be ignored if not needed)
      Locations : Destiny_Vendor_Location_List; --  Nullable
      Group : Destiny_Vendor_Group_Definition_Manifest_Hash := 0; --  Nullable
      --  Note: Theoretically there can be multiple, but according to the API spec
      --  only one group may be attached to a vendor at a time.
      Ignore_Sale_Hashes : Destiny_Inventory_Item_Definition_Manifest_Hash_List;
   end record;

   package DVDM is new Ada.Containers.Ordered_Maps
     (Destiny_Vendor_Definition_Manifest_Hash, Destiny_Vendor_Definition);
   subtype Destiny_Vendor_Map is DVDM.Map;
   procedure Read
     (Hash         :        Base_Manifest_Hash;
      Reader       : in out JSON_Simple_Pull_Reader;
      The_Manifest :    out Manifest.Manifest_Type);
end API.Definitions.Destiny_Vendor;
