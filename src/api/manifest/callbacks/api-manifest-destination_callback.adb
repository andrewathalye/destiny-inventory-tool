with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

procedure API.Manifest.Destination_Callback
  (Hash         :        Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Destination : Destiny_Destination_Definition;
   Bubble      : Destiny_Bubble_Definition;
begin
   Wait_Until_Key (Reader, "description");
   Read_Next (Reader);
   Destination.Description := VS2UB (String_Value (Reader));

   Read_Next (Reader); --  "name"
   Read_Next (Reader);
   Destination.Name := VS2UB (String_Value (Reader));

   Wait_Until_Key (Reader, "placeHash");
   Read_Next (Reader);
   Destination.Place_Hash :=
     Manifest_Hash (As_Integer (Number_Value (Reader)));

   Read_Next (Reader); --  "defaultFreeroamActivityHash"
   Read_Next (Reader);
   Destination.Default_Freeroam_Activity_Hash :=
     Manifest_Hash (As_Integer (Number_Value (Reader)));

   Wait_Until_Key (Reader, "bubbles");
   Read_Next (Reader); --  START_ARRAY
   Read_Next (Reader); --  START_OBJECT

   while Event_Kind (Reader) /= End_Array loop
      Read_Next (Reader); --  "hash"
      Read_Next (Reader);
      Bubble.Hash :=
        Destiny_Bubble_Hash_Type (As_Integer (Number_Value (Reader)));

      Wait_Until_Key (Reader, "description");
      Read_Next (Reader);
      Bubble.Description := VS2UB (String_Value (Reader));

      Read_Next (Reader); --  "name"
      Read_Next (Reader);
      Bubble.Name := VS2UB (String_Value (Reader));

      Wait_Until_Event (Reader, End_Object);
      Read_Next (Reader); --  END_OBJECT

      Destination.Bubbles.Append (Bubble);
      Read_Next (Reader); --  START_OBJECT or END_ARRAY
   end loop;

   The_Manifest.Destiny_Destinations.Insert (Hash, Destination);
end API.Manifest.Destination_Callback;
