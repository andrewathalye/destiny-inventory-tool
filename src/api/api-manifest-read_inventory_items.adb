separate (API.Manifest)
procedure Read_Inventory_Items
  (Reader : in out JSON_Simple_Pull_Reader;
   Items  :    out Destiny_Inventory_Item_Map)
is
begin
   Read_Next (Reader); -- START_OBJECT

   while Event_Kind (Reader) /= End_Object loop
      declare
         Item : Destiny_Inventory_Item_Definition;
      begin
         Wait_Until_Key (Reader, "displayProperties");
         Read_Next (Reader); -- START_OBJECT

         Read_Next (Reader); -- "description"
         Read_Next (Reader);
         Item.Description := VS2UB (String_Value (Reader));
         Read_Next (Reader); -- "name"
         Read_Next (Reader);
         Item.Name := VS2UB (String_Value (Reader));
         Read_Next (Reader); -- "icon" or "hasIcon"

         if VS2S (Key_Name (Reader)) = "icon" then
            Read_Next (Reader);
            Item.Icon_Path := VS2UB (String_Value (Reader));
         end if;
         --  Read optional fields with variable locations
         Read_Variable_Fields :
            loop
               case Event_Kind (Reader) is
                  when Key_Name =>
                     if VS2S (Key_Name (Reader)) = "itemTypeAndTierDisplayName"
                     then
                        Read_Next (Reader);
                        Item.Item_Type_And_Tier_Display_Name :=
                          VS2UB (String_Value (Reader));

                     elsif VS2S (Key_Name (Reader)) = "maxStackSize" then
                        Read_Next (Reader);
                        Item.Max_Stack_Size :=
                          Integer_32 (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) = "bucketTypeHash" then
                        Read_Next (Reader);
                        Item.Bucket_Type_Hash :=
                          Manifest_Hash (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) = "tierType" then
                        Read_Next (Reader);
                        Item.Tier_Type :=
                          Destiny_Tier_Type'Enum_Val
                            (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) =
                       "doesPostmasterPullHaveSideEffects"
                     then
                        Read_Next (Reader);
                        Item.Postmaster_Pull_Has_Side_Effects :=
                          Boolean_Value (Reader);

                     elsif VS2S (Key_Name (Reader)) = "itemType" then
                        Read_Next (Reader);
                        Item.Item_Type :=
                          Destiny_Item_Type'Enum_Val
                            (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) = "allowActions" then
                        Read_Next (Reader);
                        Item.Allow_Actions := Boolean_Value (Reader);

                     elsif VS2S (Key_Name (Reader)) = "defaultDamageTypeHash"
                     then
                        Read_Next (Reader);
                        Item.Default_Damage_Type_Hash :=
                          Manifest_Hash (As_Integer (Number_Value (Reader)));

                     elsif VS2S (Key_Name (Reader)) = "collectibleHash" then
                        Read_Next (Reader);
                        Read_Next
                          (Reader); -- "iconWatermark", "iconWatermarkShelved", etc.

                     elsif VS2S (Key_Name (Reader)) = "iconWatermark" then
                        Read_Next (Reader);
                        Item.Watermark_Path := VS2UB (String_Value (Reader));
                        Read_Next (Reader); -- "iconWatermarkShelved", etc.

                     elsif VS2S (Key_Name (Reader)) = "iconWatermarkShelved"
                     then
                        Read_Next (Reader);
                        Item.Shelved_Watermark_Path :=
                          VS2UB (String_Value (Reader));
                        Read_Next (Reader); -- etc.

                     elsif VS2S (Key_Name (Reader)) =
                       "displayVersionWatermarkIcons"
                     then
                        Read_Next (Reader); -- START_ARRAY
                        Read_Next (Reader);

                        while Event_Kind (Reader) /= End_Array loop
                           Item.Display_Version_Watermark_Icons.Append
                             (VS2UB (String_Value (Reader)));
                           Read_Next (Reader); -- STRING_VALUE or END_ARRAY
                        end loop;

                     elsif VS2S (Key_Name (Reader)) = "hash" then
                        exit Read_Variable_Fields;

                     else
                        Read_Next (Reader);
                     end if;

                  when others =>
                     null;
                     Read_Next (Reader);
               end case;
            end loop Read_Variable_Fields;

            --  At "hash"
         Read_Next (Reader);
         Items.Insert
           (Manifest_Hash (As_Integer (Number_Value (Reader))), Item);
         Wait_Until_Event (Reader, End_Object);
         Read_Next (Reader);
      end;
   end loop;
end Read_Inventory_Items;
