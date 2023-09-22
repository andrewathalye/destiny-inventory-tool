pragma Ada_2022;

--  VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers;
use VSS.JSON;

--  Local Packages
with Shared.Strings; use Shared.Strings;
with Shared.JSON;    use Shared.JSON;
with Shared.Debug;   use Shared;

procedure API.Profiles.Read_Item_Components
  (Reader     : in out JSON_Simple_Pull_Reader;
   Components :    out Item_Components_Type)
is
   Item_Instance_ID : Item_Instance_ID_Type;
begin
   -------------------------
   --  ADD_INSTANCE_DATA  --
   -------------------------

   Wait_Until_Key (Reader, "instances");
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "data"
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- key_name = item_instance_id
   Add_Instances :
      while Event_Kind (Reader) /= End_Object loop
         declare
            Item_Level : Quantity_Type;
            Quality    : Quantity_Type;

            --  Nullable
            Energy_Capacity : Quantity_Type := -1;
            Energy_Used     : Quantity_Type := -1;

         begin
            Item_Instance_ID :=
              Item_Instance_ID_Type'Value (VS2S (Key_Name (Reader)));

            --  Many fields omitted

            --  Note: Read Light Level
            --  The below is a bodge - "primaryStat" is not present in
            --  a number of "defective" 0-light items, so we would misalign
            --  if trying to read it. Instead, calculate it via "Item_Level * 10 + Quality"
            Wait_Until_Key (Reader, "itemLevel");
            Read_Next (Reader);
            Item_Level := Quantity_Type (As_Integer (Number_Value (Reader)));

            Read_Next (Reader); -- "quality"
            Read_Next (Reader);
            Quality := Quantity_Type (As_Integer (Number_Value (Reader)));

            --  A lot of nullable fields follow, but we only need "energy"
            Read_Next (Reader);
            Finish_Up :
               loop
                  case Event_Kind (Reader) is
                     when End_Object =>
                        --  Only possible to trigger if there is no "energy" object
                        exit Finish_Up;
                     when Key_Name => --  Read the key and value
                        if VS2S (Key_Name (Reader)) = "energy" then
                           Read_Next (Reader); -- START_OBJECT

                           Read_Next (Reader); -- energyTypeHash
                           Read_Next (Reader);

                           Read_Next (Reader); -- energyType
                           Read_Next (Reader);

                           Read_Next (Reader); -- energyCapacity
                           Read_Next (Reader);
                           Energy_Capacity :=
                             Quantity_Type
                               (As_Integer (Number_Value (Reader)));

                           Read_Next (Reader); -- energyUsed
                           Read_Next (Reader);
                           Energy_Used :=
                             Quantity_Type
                               (As_Integer (Number_Value (Reader)));

                           Read_Next (Reader); -- energyUnused
                           Read_Next (Reader);

                           Read_Next
                             (Reader); -- END_OBJECT (just for "energy")
                        else
                           Read_Next (Reader);
                           case Event_Kind (Reader) is
                              when Start_Object =>
                                 Wait_Until_Event (Reader, End_Object);
                              when Start_Array =>
                                 Wait_Until_Event (Reader, End_Array);
                              when String_Value |
                                Boolean_Value   |
                                Number_Value    |
                                Null_Value      =>
                                 null;
                              when others =>
                                 raise Program_Error;
                           end case;
                        end if;
                     when others =>
                        raise Program_Error;
                  end case;

                  Read_Next (Reader);
               end loop Finish_Up;

               --  END_OBJECT reached for the instance data segment

            Components.Instances.Insert
              (Item_Instance_ID,
               (Item_Level * 10 + Quality, Energy_Capacity, Energy_Used));

            Read_Next (Reader); -- Key_Name for Instance_ID or END_OBJECT
         end;
      end loop Add_Instances;

   Debug.Put_Line ("Instance data read");

   -----------------
   --  ADD_STATS  --
   -----------------

   Wait_Until_Key (Reader, "stats");
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "data"
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- key_name = item_instance_id or END_OBJECT

   Add_Stats :
      while Event_Kind (Reader) /= End_Object loop
         declare
            Stats     : Stats_Map;
            Stat_Hash : Destiny_Stat_Definition_Manifest_Hash;
         begin
            Item_Instance_ID :=
              Item_Instance_ID_Type'Value (VS2S (Key_Name (Reader)));

            Read_Next (Reader); -- START_OBJECT
            Read_Next (Reader); -- "stats"
            Read_Next (Reader); -- START_OBJECT

            Read_Next (Reader); -- statHash (as a key)
            Read_Stats :
               while Event_Kind (Reader) /= End_Object loop
                  Read_Next (Reader); -- START_OBJECT
                  Read_Next (Reader); -- "statHash"

                  Read_Next (Reader);
                  Stat_Hash :=
                    Destiny_Stat_Definition_Manifest_Hash
                      (As_Integer (Number_Value (Reader)));

                  Read_Next (Reader); -- "value"
                  Read_Next (Reader);
                  Stats.Insert
                    (Stat_Hash,
                     Stat_Type (As_Integer (Number_Value (Reader))));

                  Read_Next (Reader); -- END_OBJECT
                  Read_Next (Reader); -- statHash as key or END_OBJECT
               end loop Read_Stats;

            Components.Stats.Insert (Item_Instance_ID, Stats);
            Read_Next (Reader); -- END_OBJECT (for "stats")
            Read_Next (Reader); -- instance_id as key_name or end_object
         end;
      end loop Add_Stats;

   Debug.Put_Line ("Stats read");

   -------------------
   --  ADD_SOCKETS  --
   -------------------

   Wait_Until_Key (Reader, "sockets");
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "data"
   Read_Next (Reader); -- START_OBJECT

   Read_Next (Reader); -- key_name = item_instance_id or End_Object
   Add_Sockets :
      while Event_Kind (Reader) /= End_Object loop
         declare
            Sockets : Socket_List;
            Socket  : Socket_Type;
         begin
            Item_Instance_ID :=
              Item_Instance_ID_Type'Value (VS2S (Key_Name (Reader)));
            Read_Next (Reader); -- START_OBJECT
            Read_Next (Reader); -- "sockets"
            Read_Next (Reader); -- Start_Array
            Read_Next (Reader); -- START_OBJECT or END_ARRAY

            Add_Socket :
               while Event_Kind (Reader) /= End_Array loop
                  Read_Next (Reader); -- "plugHash" or "isEnabled"
                  if VS2S (Key_Name (Reader)) = "plugHash" then
                     Read_Next (Reader);
                     Socket.Plug_Hash :=
                       Destiny_Inventory_Item_Definition_Manifest_Hash
                         (As_Integer (Number_Value (Reader)));
                     Read_Next (Reader); -- "isEnabled"
                  else
                     Socket.Plug_Hash := 0;
                  end if;

                  Read_Next (Reader);
                  Socket.Is_Enabled := Boolean_Value (Reader);

                  Read_Next (Reader); -- "isVisible"

                  Read_Next (Reader);
                  Socket.Is_Visible := Boolean_Value (Reader);

                  Read_Next (Reader); -- "enableFailIndexes" or END_OBJECT
                  if Event_Kind (Reader) = Key_Name then
                     Read_Next (Reader); -- START_ARRAY
                     Skip_Current_Array (Reader);
                  end if;

                  Read_Next (Reader); -- END_ARRAY or START_OBJECT
                  Sockets.Append (Socket);
               end loop Add_Socket;
            Components.Sockets.Insert (Item_Instance_ID, Sockets);
            Read_Next (Reader); -- End_Object
            Read_Next (Reader); -- instance_id as key_name or end_object
         end;
      end loop Add_Sockets;

   Debug.Put_Line ("Sockets read");

   ---------------------------
   --  ADD_PLUG_OBJECTIVES  --
   ---------------------------
   Wait_Until_Key (Reader, "plugObjectives");
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "data"
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- instanceID as key_name or END_OBJECT

   --  Note: Layout is _very_ complex
   --  plugObjectives {
   --     "<instance_id>": {
   --        "objectivesPerPlug": {
   --           "<plugHash>": [
   --              {
   --                 <data_fields>
   --              }
   --           ]
   --        }
   --     }
   --  }
   Add_Objective_Maps :
      while Event_Kind (Reader) /= End_Object loop
         declare
            --  Temp Variables used for insertion
            Map       : Plug_Objective_Map;
            Plug_Hash : Destiny_Inventory_Item_Definition_Manifest_Hash;
         begin
            Item_Instance_ID :=
              Item_Instance_ID_Type'Value (VS2S (Key_Name (Reader)));
            Read_Next (Reader); -- START_OBJECT
            Read_Next (Reader); -- "objectivesPerPlug"
            Read_Next (Reader); -- START_OBJECT
            Read_Next (Reader); -- plugHash as key_name or END_OBJECT

            Add_Plugs :
               while Event_Kind (Reader) /= End_Object loop
                  declare
                     List : Plug_Objective_List;
                  begin
                     Plug_Hash :=
                       Destiny_Inventory_Item_Definition_Manifest_Hash'Value
                         (VS2S (Key_Name (Reader)));

                     Read_Next (Reader); -- Start_Array
                     Read_Next (Reader); -- START_OBJECT or END_ARRAY

                     Add_Objectives :
                        while Event_Kind (Reader) /= End_Array loop
                           declare
                              Objective : Plug_Objective_Type;
                           begin
                              Read_Next (Reader); -- "objectiveHash"
                              Read_Next (Reader);
                              Objective.Objective_Hash :=
                                Destiny_Objective_Definition_Manifest_Hash
                                  (As_Integer (Number_Value (Reader)));

                              Read_Next
                                (Reader); -- "destinationHash" or "activityHash" or "progress" or "completionValue"

                              --  Neither of these two fields are used at the moment
                              if VS2S (Key_Name (Reader)) = "destinationHash"
                              then
                                 Read_Next (Reader);
                                 Read_Next
                                   (Reader); -- "activityHash" or "progress" or "completionValue"
                              end if;

                              if VS2S (Key_Name (Reader)) = "activityHash" then
                                 Read_Next (Reader);
                                 Read_Next
                                   (Reader); -- "progress" or "completionValue"
                              end if;

                              if VS2S (Key_Name (Reader)) = "progress" then
                                 Read_Next (Reader);
                                 Objective.Progress :=
                                   Quantity_Type
                                     (As_Integer (Number_Value (Reader)));
                                 Read_Next (Reader); -- "completionValue"
                              end if;

                              Read_Next (Reader);
                              Objective.Completion_Value :=
                                Quantity_Type
                                  (As_Integer (Number_Value (Reader)));

                              Read_Next (Reader); -- "complete"
                              Read_Next (Reader);
                              Objective.Complete := Boolean_Value (Reader);

                              Read_Next (Reader); -- "visible"
                              Read_Next (Reader);
                              Objective.Visible := Boolean_Value (Reader);

                              List.Append (Objective);
                           end;

                           Read_Next (Reader); -- END_OBJECT
                           Read_Next (Reader); -- Start_Object or End_Array
                        end loop Add_Objectives;

                     Map.Insert (Plug_Hash, List);

                     Read_Next (Reader); -- plugHash as key_name or END_OBJECT
                  end;
               end loop Add_Plugs;

            Components.Plug_Objectives.Insert (Item_Instance_ID, Map);
            Read_Next (Reader); -- END_OBJECT
            Read_Next (Reader); -- plugHash as key_name or END_OBJECT
         end;
      end loop Add_Objective_Maps;
   Debug.Put_Line ("Plug Objectives read");

   -----------------
   --  ADD_PERKS  --
   -----------------

   Wait_Until_Key (Reader, "perks");
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "data"
   Read_Next (Reader); -- START_OBJECT

   Read_Next (Reader); -- key_name = item_instance_id or END_OBJECT
   Add_Perks :
      while Event_Kind (Reader) /= End_Object loop
         declare
            Perks : Perk_List;
            Perk  : Perk_Type;
         begin
            Item_Instance_ID :=
              Item_Instance_ID_Type'Value (VS2S (Key_Name (Reader)));
            Read_Next (Reader); -- START_OBJECT
            Read_Next (Reader); -- "perks"
            Read_Next (Reader); -- START_ARRAY
            Read_Next (Reader); -- START_OBJECT or END_ARRAY

            Add_Perk :
               while Event_Kind (Reader) /= End_Array loop
                  Read_Next (Reader); -- "perkHash"
                  Read_Next (Reader);
                  Perk.Perk_Hash :=
                    Destiny_Sandbox_Perk_Definition_Manifest_Hash
                      (As_Integer (Number_Value (Reader)));

                  Read_Next (Reader); -- "iconPath"
                  Read_Next (Reader);
                  Perk.Icon_Path := VS2UB (String_Value (Reader));

                  Read_Next (Reader); -- "isActive"
                  Read_Next (Reader);
                  Perk.Is_Active := Boolean_Value (Reader);

                  Read_Next (Reader); -- "visible"
                  Read_Next (Reader);
                  Perk.Visible := Boolean_Value (Reader);

                  Read_Next (Reader); -- END_OBJECT
                  Read_Next (Reader); -- START_OBJECT or END_ARRAY

                  Perks.Append (Perk);
               end loop Add_Perk;
            Components.Perks.Insert (Item_Instance_ID, Perks);

            Read_Next (Reader); -- End_Object
            Read_Next (Reader); -- key_name is item_instance_id or END_OBJECT
         end;
      end loop Add_Perks;

   Debug.Put_Line ("Perks read");
end API.Profiles.Read_Item_Components;
