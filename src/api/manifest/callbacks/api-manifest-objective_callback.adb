with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;

procedure API.Manifest.Objective_Callback
  (Hash         :        Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Objective : Destiny_Objective_Definition;
begin
   Read_Next (Reader); --  START_OBJECT

   --  "icon" is a nullable field. Parse it manually.
   Read_Next (Reader); --  "displayProperties"
   Read_Next (Reader); --  START_OBJECT
   Read_Next (Reader); --  "description"
   Read_Next (Reader);
   Read_Next (Reader); --  "name"
   Read_Next (Reader);
   Read_Next (Reader); --  "icon" or "hasIcon"

   if VS2S (Key_Name (Reader)) = "icon" then
      Read_Next (Reader);
      Objective.Icon_Path := VS2UB (String_Value (Reader));
   end if;

   Wait_Until_Key (Reader, "progressDescription");
   Read_Next (Reader);
   Objective.Progress_Description := VS2UB (String_Value (Reader));

   The_Manifest.Destiny_Objectives.Insert (Hash, Objective);
end API.Manifest.Objective_Callback;