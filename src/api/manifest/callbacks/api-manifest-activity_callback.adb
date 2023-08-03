with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

procedure API.Manifest.Activity_Callback
  (Hash         :        Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Activity : Destiny_Activity_Definition;
begin
   Wait_Until_Key (Reader, "description");
   Read_Next (Reader);
   Activity.Description := VS2UB (String_Value (Reader));

   Read_Next (Reader); --  "name"
   Read_Next (Reader);
   Activity.Name := VS2UB (String_Value (Reader));

   Wait_Until_Key (Reader, "destinationHash");
   Read_Next (Reader);
   Activity.Destination_Hash :=
     Manifest_Hash (As_Integer (Number_Value (Reader)));

   Wait_Until_Key (Reader, "placeHash");
   Read_Next (Reader);
   Activity.Place_Hash := Manifest_Hash (As_Integer (Number_Value (Reader)));

   The_Manifest.Destiny_Activities.Insert (Hash, Activity);
end API.Manifest.Activity_Callback;
