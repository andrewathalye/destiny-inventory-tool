with Interfaces; use Interfaces;

with VSS.JSON.Pull_Readers.Simple;
use VSS.JSON.Pull_Readers.Simple;
use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.Strings; use Shared.Strings;

with API.Definitions.Hashes;
with API.Definitions.Destiny_Inventory_Bucket;
   use API.Definitions.Destiny_Inventory_Bucket;
   use API.Definitions.Hashes;
   use API.Definitions;

function API.Tasks.Profiles.Read_Item (Reader : in out JSON_Simple_Pull_Reader) return Item_Type
is
   Item : Item_Type;
begin
   Read_Next (Reader); -- "itemHash"
   Read_Next (Reader);
   Item.Item_Hash :=
     Destiny_Inventory_Item_Definition_Manifest_Hash
       (As_Integer (Number_Value (Reader)));

   --  Optional field: itemInstanceId
   Read_Next (Reader); -- "itemInstanceId" or "quantity"
   if VS2S (Key_Name (Reader)) = "itemInstanceId" then
      Read_Next (Reader);
      Item.Item_Instance_ID :=
        Item_Instance_ID_Type'Value (VS2S (String_Value (Reader)));
      Read_Next (Reader); -- "quantity"
   end if;

   Read_Next (Reader);
   Item.Quantity := Quantity_Type (As_Integer (Number_Value (Reader)));

   Read_Next (Reader); -- "bindStatus"
   Read_Next (Reader);
   Item.Bind_Status :=
     Bind_Status_Type'Enum_Val (As_Integer (Number_Value (Reader)));

   Read_Next (Reader); -- "location"
   Read_Next (Reader);
   Item.Location :=
     Item_Location_Type'Enum_Val (As_Integer (Number_Value (Reader)));

   Read_Next (Reader); -- "bucketHash"
   Read_Next (Reader);
   Item.Bucket_Hash :=
     Destiny_Inventory_Bucket_Definition_Manifest_Hash
       (As_Integer (Number_Value (Reader)));

   Read_Next (Reader); -- "transferStatus"
   Read_Next (Reader);
   Item.Transfer_Status :=
     Transfer_Status_Type'Enum_Val (As_Integer (Number_Value (Reader)));

   Read_Next (Reader); -- "lockable"
   Read_Next (Reader);
   Item.Lockable := Boolean_Value (Reader);

   Read_Next (Reader); -- "state"
   Read_Next (Reader);

   declare

      Temp : constant Unsigned_64 :=
        Unsigned_64 (As_Integer (Number_Value (Reader)));

   begin
      Item.State :=
        (Locked                => (Temp and 2#1#) > 0,
         Tracked               => (Temp and 2#10#) > 0,
         Masterwork            => (Temp and 2#100#) > 0,
         Crafted               => (Temp and 2#1000#) > 0,
         Highlighted_Objective => (Temp and 2#1_0000#) > 0);
   end;

   --  TODO UNDOCUMENTED Field dismantlePermission
   Read_Next (Reader); -- "dismantlePermission"
   Read_Next (Reader);

   --  Optional Field overrideStyleItemHash
   Read_Next
     (Reader); -- "overrideStyleItemHash" or "expirationDate" or "isWrapper"
   if VS2S (Key_Name (Reader)) = "overrideStyleItemHash" then
      Read_Next (Reader);
      Item.Override_Style_Item_Hash :=
        Destiny_Inventory_Item_Definition_Manifest_Hash
          (As_Integer (Number_Value (Reader)));
      Read_Next (Reader); -- "expirationDate" or "isWrapper"
   end if;

   --  Optional Field expirationDate
   if VS2S (Key_Name (Reader)) = "expirationDate" then
      Read_Next (Reader);
      Item.Expiration_Date := VS2UB (String_Value (Reader));
      Read_Next (Reader); -- "isWrapper"
   end if;

   Read_Next (Reader);
   Read_Next
     (Reader); -- tooltipNotificationIndexes or metricHash or metricObjective or versionNumber or itemValueVisibility

   --  Skip several optional fields we don’t need
   if VS2S (Key_Name (Reader)) = "tooltipNotificationIndexes" then
      Read_Next (Reader); -- START_ARRAY
      Skip_Current_Array
        (Reader); -- metricHash or metricObjective or versionNumber or itemValueVisibility
   end if;

   if VS2S (Key_Name (Reader)) = "metricHash" then
      Read_Next (Reader);
      Read_Next
        (Reader); -- metricObjective or versionNumber or itemValueVisibility
   end if;

   if VS2S (Key_Name (Reader)) = "metricObjective" then
      Read_Next (Reader);
      Skip_Current_Object (Reader); -- versionNumber or itemValueVisibility
   end if;

   --  Optional field versionNumber
   if VS2S (Key_Name (Reader)) = "versionNumber" then
      Read_Next (Reader);
      Item.Version_Number := Integer_32 (As_Integer (Number_Value (Reader)));
      Read_Next (Reader); -- "itemValueVisibility" possibly
   end if;

   if VS2S (Key_Name (Reader)) = "itemValueVisibility" then
      Read_Next (Reader);
      Skip_Current_Array (Reader);
   end if;

   return Item;
end API.Tasks.Profiles.Read_Item;
