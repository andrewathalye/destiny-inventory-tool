with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

procedure API.Manifest.Inventory_Bucket_Callback
  (Hash         :        Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Bucket : Destiny_Inventory_Bucket_Definition;
begin
   Wait_Until_Key (Reader, "displayProperties");
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "description" or "hasIcon"

   --  If the bucket has a name and description, add them
   if VS2S (Key_Name (Reader)) = "description" then
      Read_Next (Reader);
      Bucket.Description := VS2UB (String_Value (Reader));
      Read_Next (Reader); -- "name"
      Read_Next (Reader);
      Bucket.Name := VS2UB (String_Value (Reader));
   end if;

   Wait_Until_Key (Reader, "category");
   Read_Next (Reader);
   Bucket.Category :=
     Destiny_Inventory_Bucket_Category'Enum_Val
       (As_Integer (Number_Value (Reader)));

   Read_Next (Reader); -- "bucketOrder"
   Read_Next (Reader);
   Bucket.Bucket_Order := Integer_32 (As_Integer (Number_Value (Reader)));

   Read_Next (Reader); -- "itemCount"
   Read_Next (Reader);
   Bucket.Item_Count := Quantity_Type (As_Integer (Number_Value (Reader)));

   Read_Next (Reader); -- "location"
   Read_Next (Reader);
   Bucket.Location :=
     Item_Location_Type'Enum_Val (As_Integer (Number_Value (Reader)));

   Wait_Until_Key (Reader, "fifo");
   Read_Next (Reader);
   Bucket.FIFO := Boolean_Value (Reader);

   The_Manifest.Destiny_Inventory_Buckets.Insert (Hash, Bucket);
end API.Manifest.Inventory_Bucket_Callback;
