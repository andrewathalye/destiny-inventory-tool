separate (API.Profiles)
procedure Read_Silver
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
   Result.Platform_Silver (PSN) := Read_Item (Reader);
   Read_Next (Reader); -- "TigerXbox"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Xbox) := Read_Item (Reader);
   Read_Next (Reader); -- "TigerBlizzard"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Blizzard) := Read_Item (Reader);
   Read_Next (Reader); -- "TigerStadia"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Stadia) := Read_Item (Reader);
   Read_Next (Reader); -- "TigerSteam"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Steam) := Read_Item (Reader);
   Read_Next (Reader); -- "BungieNext"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (Next) := Read_Item (Reader);
   Read_Next (Reader); -- "TigerEgs"
   Read_Next (Reader); -- START_OBJECT
   Result.Platform_Silver (EGS) := Read_Item (Reader);
end Read_Silver;
