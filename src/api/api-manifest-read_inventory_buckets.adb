separate (API.Manifest)
procedure Read_Inventory_Buckets
  (Reader  : in out JSON_Simple_Pull_Reader;
   Buckets :    out Destiny_Inventory_Bucket_Map)
is

   Bucket : Destiny_Inventory_Bucket_Definition;

begin
   Read_Next (Reader); -- START_OBJECT

   while Event_Kind (Reader) /= End_Object loop
      Bucket.Description := Null_Unbounded_String;
      Bucket.Name        := Null_Unbounded_String;
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
      Bucket.Item_Count := Integer_32 (As_Integer (Number_Value (Reader)));
      Read_Next (Reader); -- "location"
      Read_Next (Reader);
      Bucket.Location :=
        Item_Location_Type'Enum_Val (As_Integer (Number_Value (Reader)));
      Wait_Until_Key (Reader, "fifo");
      Read_Next (Reader);
      Bucket.FIFO := Boolean_Value (Reader);
      Wait_Until_Key (Reader, "hash");
      Read_Next (Reader);
      Buckets.Insert
        (Manifest_Hash (As_Integer (Number_Value (Reader))), Bucket);
      Wait_Until_Event (Reader, End_Object);
      Read_Next (Reader);
   end loop;
end Read_Inventory_Buckets;
