pragma Ada_2022;

with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;
--  with Shared.Debug; use Shared.Debug;

procedure API.Manifest.Faction_Callback
  (Hash         :        Base_Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Faction : Destiny_Faction_Definition;
begin
   Wait_Until_Key (Reader, "displayProperties");
   Read_Next (Reader); --  START_OBJECT

   Read_Next (Reader); -- "description" / "icon"

   if VS2S (Key_Name (Reader)) = "description" then
      Read_Next (Reader);
      Faction.Description := VS2UB (String_Value (Reader));

      Read_Next (Reader); -- "name"
      Read_Next (Reader);
      Faction.Name := VS2UB (String_Value (Reader));

      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "icon" then
      Read_Next (Reader);
      Faction.Icon_Path := VS2UB (String_Value (Reader));
   end if;

   if VS2S (Key_Name (Reader)) /= "hasIcon" then
      Wait_Until_Key (Reader, "hasIcon");
   end if;

   Read_Next (Reader); --  BOOLEAN_VALUE

   Read_Next (Reader); -- END_OBJECT

   Read_Next (Reader); -- "progressionHash"
   Read_Next (Reader);

   Faction.Progression_Hash :=
     Destiny_Progression_Definition_Manifest_Hash
       (As_Integer (Number_Value (Reader)));

   The_Manifest.Destiny_Factions.Insert
     (Destiny_Faction_Definition_Manifest_Hash (Hash), Faction);
end API.Manifest.Faction_Callback;
