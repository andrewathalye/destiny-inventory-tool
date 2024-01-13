with VSS.JSON.Pull_Readers.Simple;
   use VSS.JSON.Pull_Readers.Simple;
   use VSS.JSON.Pull_Readers;

with API.Memberships; use API.Memberships;
with API.Tasks.Profiles.Read_Item;

procedure API.Tasks.Profiles.Read_Silver
  (Reader : in out JSON_Simple_Pull_Reader; Result : out Profile_Type)
is
begin
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "data"
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "platformSilver"

   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "TigerPsn"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (PSN) := API.Tasks.Profiles.Read_Item (Reader);
   Read_Next (Reader); -- "TigerXbox"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Xbox) := API.Tasks.Profiles.Read_Item (Reader);
   Read_Next (Reader); -- "TigerBlizzard"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Blizzard) := API.Tasks.Profiles.Read_Item (Reader);
   Read_Next (Reader); -- "TigerStadia"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Stadia) := API.Tasks.Profiles.Read_Item (Reader);
   Read_Next (Reader); -- "TigerSteam"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Steam) := API.Tasks.Profiles.Read_Item (Reader);
   Read_Next (Reader); -- "BungieNext"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Next) := API.Tasks.Profiles.Read_Item (Reader);
   Read_Next (Reader); -- "TigerEgs"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (EGS) := API.Tasks.Profiles.Read_Item (Reader);
end API.Tasks.Profiles.Read_Silver;
