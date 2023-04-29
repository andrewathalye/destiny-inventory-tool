pragma Ada_2022;

separate (API.Profiles)
procedure Read_Item_Components
  (Reader     : in out JSON_Simple_Pull_Reader;
   Components :    out Item_Components_Type)
is
   Item_Instance_ID : Item_Instance_ID_Type;
begin
   Wait_Until_Key (Reader, "instances");
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "data"
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- key_name = item_instance_id
   Add_Instances :
      while Event_Kind (Reader) /= End_Object loop
         declare
            Primary_Stat_Hash  : Manifest_Hash;
            Primary_Stat_Value : Integer_32;
            Item_Level         : Integer_32;

            --  Nullable
            Energy_Capacity : Integer_32 := -1;
            Energy_Used     : Integer_32 := -1;

            Objects_Remaining : Natural := 1;

         begin
            Item_Instance_ID :=
              Item_Instance_ID_Type'Value (VS2S (Key_Name (Reader)));
            --  Many fields omitted
            Wait_Until_Key (Reader, "primaryStat");
            Read_Next (Reader); -- START_OBJECT

            Read_Next (Reader); -- "statHash"
            Read_Next (Reader);
            Primary_Stat_Hash :=
              Manifest_Hash (As_Integer (Number_Value (Reader)));

            Read_Next (Reader); -- "value"
            Read_Next (Reader);
            Primary_Stat_Value :=
              Integer_32 (As_Integer (Number_Value (Reader)));

            Wait_Until_Key (Reader, "itemLevel");
            Read_Next (Reader);
            Item_Level := Integer_32 (As_Integer (Number_Value (Reader)));

            --  A lot of nullable fields follow, so just loop through them
            Finish_Up :
               loop
                  case Event_Kind (Reader) is
                     when Start_Object =>
                        Objects_Remaining := @ + 1;
                     when End_Object =>
                        Objects_Remaining := @ - 1;
                     when Key_Name =>
                        if VS2S (Key_Name (Reader)) = "energy" then
                           Read_Next (Reader); -- START_OBJECT

                           Read_Next (Reader); -- energyTypeHash
                           Read_Next (Reader);

                           Read_Next (Reader); -- energyType
                           Read_Next (Reader);

                           Read_Next (Reader); -- energyCapacity
                           Read_Next (Reader);
                           Energy_Capacity :=
                             Integer_32 (As_Integer (Number_Value (Reader)));

                           Read_Next (Reader); -- energyUsed
                           Read_Next (Reader);
                           Energy_Used :=
                             Integer_32 (As_Integer (Number_Value (Reader)));

                           Read_Next (Reader); -- energyUnused
                           Read_Next (Reader);

                           Read_Next (Reader); -- END_OBJECT
                        end if;
                     when others =>
                        null;
                  end case;

                  if Objects_Remaining = 0 then
                     exit Finish_Up;
                  end if;

                  Read_Next (Reader);
               end loop Finish_Up;

            Components.Instances.Insert
              (Item_Instance_ID,
              (Primary_Stat_Hash,
                Primary_Stat_Value,
                Item_Level,
                Energy_Capacity,
                Energy_Used));

            Read_Next (Reader); -- Key_Name for Instance_ID or END_OBJECT

         end;
      end loop Add_Instances;

   Debug.Put_Line ("Instance data read");

   Wait_Until_Key (Reader, "stats");
   Read_Next (Reader); -- START_OBJECT
   Read_Next (Reader); -- "data"
   Read_Next (Reader); -- START_OBJECT
   Add_Stats :
      while Event_Kind (Reader) /= End_Object loop
         declare
            Stats     : Stats_Map;
            Stat_Hash : Manifest_Hash;
         begin
            Read_Next (Reader); -- key_name = item_instance_id
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
                    Manifest_Hash (As_Integer (Number_Value (Reader)));

                  Read_Next (Reader); -- "value"
                  Read_Next (Reader);
                  Stats.Insert
                    (Stat_Hash,
                     Integer_32 (As_Integer (Number_Value (Reader))));

                  Read_Next (Reader); -- END_OBJECT
                  Read_Next (Reader); -- statHash as key or END_OBJECT
               end loop Read_Stats;

            Components.Stats.Insert (Item_Instance_ID, Stats);
            Read_Next (Reader); -- START_OBJECT or END_OBJECT
         end;
      end loop Add_Stats;

   Debug.Put_Line ("Stats read");

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
                       Manifest_Hash (As_Integer (Number_Value (Reader)));
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
                    Manifest_Hash (As_Integer (Number_Value (Reader)));

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
end Read_Item_Components;
