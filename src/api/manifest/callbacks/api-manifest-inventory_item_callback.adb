pragma Ada_2022;

with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;
--  with Shared.Debug;   use Shared.Debug;

procedure API.Manifest.Inventory_Item_Callback
  (Hash         :        Base_Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Item : Destiny_Inventory_Item_Definition;
begin
   --  Start DestinyDisplayPropertiesDefinition
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
      Wait_Until_Key (Reader, "hasIcon");
   end if;

   Read_Next (Reader); --  BOOLEAN_VALUE
   Read_Next (Reader); --  END_OBJECT
   --  End DestinyDisplayPropertiesDefinition

   Read_Next
     (Reader); -- tooltipNotifications / collectibleHash / iconWatermark / secondaryIcon / secondaryOverlay / secondarySpecial / backgroundColor / screenshot

   if VS2S (Key_Name (Reader)) = "tooltipNotifications" then
      Read_Next (Reader); --  START_ARRAY
      Skip_Current_Array (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "collectibleHash" then
      Read_Next (Reader); -- NUMBER_VALUE
      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "iconWatermark" then
      Read_Next (Reader);
      Item.Watermark_Path := VS2UB (String_Value (Reader));

      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "iconWatermarkShelved" then
      Read_Next (Reader);
      Item.Shelved_Watermark_Path := VS2UB (String_Value (Reader));

      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "secondaryIcon" then
      Read_Next (Reader); --  STRING_VALUE
      Item.Secondary_Icon_Path := VS2UB (String_Value (Reader));
      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "secondaryOverlay" then
      Read_Next (Reader); --  STRING_VALUE
      Item.Secondary_Overlay_Path := VS2UB (String_Value (Reader));
      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "secondarySpecial" then
      Read_Next (Reader); --  STRING_VALUE
      Item.Secondary_Special_Path := VS2UB (String_Value (Reader));
      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "backgroundColor" then
      Read_Next (Reader); --  START_OBJECT
      Skip_Current_Object (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "screenshot" then
      Read_Next (Reader); --  STRING_VALUE
      Read_Next (Reader);
   end if;

   --  Next up if itemTypeDisplayName then also flavorText, uiItemDisplayStyle, and itemTypeAndTierDisplayName
   if VS2S (Key_Name (Reader)) = "itemTypeDisplayName" then
      Read_Next (Reader); --  STRING_VALUE
      Read_Next (Reader); --  "flavorText"
      Read_Next (Reader); --  STRING_VALUE
      Read_Next (Reader); --  "uiItemDisplayStyle"
      Read_Next (Reader); --  STRING_VALUE

      Read_Next (Reader); --  "itemTypeAndTierDisplayName"
      Read_Next (Reader);
      Item.Item_Type_And_Tier_Display_Name := VS2UB (String_Value (Reader));
      Read_Next (Reader);
   end if;

   --  skip some optionals before "inventory"
   if VS2S (Key_Name (Reader)) = "displaySource" then
      Read_Next (Reader); --  STRING_VALUE
      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "tooltipStyle" then
      Read_Next (Reader); --  STRING_VALUE
      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "action" then
      Read_Next (Reader); --  START_OBJECT
      Skip_Current_Object (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "crafting" then
      Read_Next (Reader); --  START_OBJECT
      Skip_Current_Object (Reader);
   end if;

   --  Start DestinyItemInventoryBlockDefinition
   --  Practically guaranteed to exist, but better to handle it this way
   if VS2S (Key_Name (Reader)) = "inventory" then
      Wait_Until_Key (Reader, "maxStackSize");
      Read_Next (Reader);
      Item.Max_Stack_Size :=
        Quantity_Type (As_Integer (Number_Value (Reader)));

      Read_Next (Reader); --  "bucketTypeHash"
      Read_Next (Reader);
      Item.Bucket_Type_Hash :=
        Destiny_Inventory_Bucket_Definition_Manifest_Hash
          (As_Integer (Number_Value (Reader)));

      Wait_Until_Key (Reader, "tierType");
      Read_Next (Reader);
      Item.Tier_Type :=
        Destiny_Tier_Type'Enum_Val (As_Integer (Number_Value (Reader)));

      Wait_Until_Event (Reader, End_Object);
      Read_Next (Reader);
   end if;
   --  End DestinyItemInventoryBlockDefinition

   --  Skip a bunch of optionals on the way to "quality" if present
   if VS2S (Key_Name (Reader)) = "setData" then
      Read_Next (Reader); --  START_OBJECT
      Skip_Current_Object (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "stats" then
      Read_Next (Reader); --  START_OBJECT
      Skip_Current_Object (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "emblemObjectiveHash" then
      Read_Next (Reader); --  NUMBER_VALUE
      Read_Next (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "equippingBlock" then
      Read_Next (Reader); --  START_OBJECT
      Skip_Current_Object (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "translationBlock" then
      Read_Next (Reader); --  START_OBJECT
      Skip_Current_Object (Reader);
   end if;

   if VS2S (Key_Name (Reader)) = "preview" then
      Read_Next (Reader); --  START_OBJECT
      Skip_Current_Object (Reader);
   end if;

   --  Start DestinyItemQualityBlockDefinition
   if VS2S (Key_Name (Reader)) = "quality" then
      Wait_Until_Key (Reader, "displayVersionWatermarkIcons");
      Read_Next (Reader); --  START_ARRAY
      Read_Next (Reader); --  STRING_VALUE or END_ARRAY
      while Event_Kind (Reader) /= End_Array loop
         Item.Display_Version_Watermark_Icons.Append
           (VS2UB (String_Value (Reader)));
         Read_Next (Reader); --  STRING_VALUE or END_ARRAY
      end loop;
   end if;
   --  End DestinyItemQualityBlockDefinition

   Wait_Until_Key (Reader, "allowActions");
   Read_Next (Reader);
   Item.Allow_Actions := Boolean_Value (Reader);

   Read_Next (Reader); --  "doesPostmasterPullHaveSideEffects"
   Read_Next (Reader);
   Item.Postmaster_Pull_Has_Side_Effects := Boolean_Value (Reader);

   Wait_Until_Key (Reader, "itemType");
   Read_Next (Reader);
   Item.Item_Type :=
     Destiny_Item_Type'Enum_Val (As_Integer (Number_Value (Reader)));

   Wait_Until_Key (Reader, "equippable");
   Read_Next (Reader); --  BOOLEAN_VALUE

   Read_Next (Reader); --  optionally "damageTypeHashes"
   if VS2S (Key_Name (Reader)) = "damageTypeHashes" then
      Read_Next (Reader); --  START_ARRAY
      Skip_Current_Array (Reader); --  "damageTypes"
      Read_Next (Reader); --  START_ARRAY
      Skip_Current_Array (Reader); --  "defaultDamageType"
      Read_Next (Reader); --  NUMBER_VALUE

      Read_Next (Reader); --  "defaultDamageTypeHash"
      Read_Next (Reader);
      Item.Default_Damage_Type_Hash :=
        Destiny_Damage_Type_Definition_Manifest_Hash
          (As_Integer (Number_Value (Reader)));
   end if;

--   Put_Line ("Loaded " & (+Item.Name));
--   Put_Line (Item'Image);
   The_Manifest.Destiny_Inventory_Items.Insert
     (Destiny_Inventory_Item_Definition_Manifest_Hash (Hash), Item);
end API.Manifest.Inventory_Item_Callback;
