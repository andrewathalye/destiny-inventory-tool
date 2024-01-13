with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;

--  Note: Position Reader at "itemComponents"
procedure API.Tasks.Profiles.Read_Item_Components
  (Reader : in out JSON_Simple_Pull_Reader;
   Components : out Item_Components_Type);
