with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;

procedure API.Manifest.Place_Callback
  (Hash : Base_Manifest_Hash;
   Reader : in out JSON_Simple_Pull_Reader;
   The_Manifest : out Manifest_Type);
