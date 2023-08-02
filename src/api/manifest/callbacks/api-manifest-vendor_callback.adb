pragma Ada_2022;

with VSS.JSON.Pull_Readers; use VSS.JSON.Pull_Readers;
use VSS.JSON;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared.Debug;

procedure API.Manifest.Vendor_Callback
  (Hash         :        Manifest_Hash;
   Reader       : in out JSON_Simple_Pull_Reader;
   The_Manifest :    out Manifest_Type)
is
   Vendor : Destiny_Vendor_Definition;

   DDCD_Index : Display_Category_Index_Type;
begin
   ---------------------------
   --  displayProperties {} --
   ---------------------------
   --  Begin DestinyVendorDisplayPropertiesDefinition << displayProperties
   Wait_Until_Key (Reader, "subtitle");
   Read_Next (Reader);
   Vendor.Subtitle := VS2UB (String_Value (Reader));

   Wait_Until_Key (Reader, "description");
   Read_Next (Reader);
   Vendor.Description := VS2UB (String_Value (Reader));

   Read_Next (Reader); --  "name"
   Read_Next (Reader);
   Vendor.Name := VS2UB (String_Value (Reader));

   Read_Next (Reader); -- "icon" or "hasIcon"
   if VS2S (String_Value (Reader)) = "icon" then
      Read_Next (Reader);
      Vendor.Icon_Path := VS2UB (String_Value (Reader));
   end if;
   --  End DestinyVendorDisplayPropertiesDefinition

   ---------------------
   -- displayItemHash --
   ---------------------
   Wait_Until_Key (Reader, "displayItemHash");
   Read_Next (Reader);
   Vendor.Display_Item_Hash :=
     Manifest_Hash (As_Integer (Number_Value (Reader)));

   -------------------
   -- inhibitBuying --
   -------------------
   Read_Next (Reader); --  "inhibitBuying"
   Read_Next (Reader);
   Vendor.Inhibit_Buying := Boolean_Value (Reader);

   --------------------
   -- inhibitSelling --
   --------------------
   Read_Next (Reader); --  "inhibitSelling"
   Read_Next (Reader);
   Vendor.Inhibit_Selling := Boolean_Value (Reader);

   -----------------
   -- factionHash --
   -----------------
   Read_Next (Reader); --  "factionHash"
   Read_Next (Reader);
   Vendor.Faction_Hash := Manifest_Hash (As_Integer (Number_Value (Reader)));

   ----------------------
   -- failureStrings[] --
   ----------------------
   Wait_Until_Key (Reader, "failureStrings");
   Read_Next (Reader); --  START_ARRAY
   Read_Next (Reader); -- STRING_VALUE or END_ARRAY
   while Event_Kind (Reader) /= End_Array loop
      Vendor.Failure_Strings.Append (VS2UB (String_Value (Reader)));
      Read_Next (Reader); --  STRING_VALUE or END_ARRAY
   end loop;

   -------------
   -- enabled --
   -------------
   Wait_Until_Key (Reader, "enabled");
   Read_Next (Reader);
   Vendor.Enabled := Boolean_Value (Reader);

   -------------
   -- visible --
   -------------
   Read_Next (Reader); --  "visible"
   Read_Next (Reader);
   Vendor.Visible := Boolean_Value (Reader);

   --------------------------
   -- displayCategories [] --
   --------------------------
   Wait_Until_Key (Reader, "displayCategories");
   Read_Next (Reader); --  START_ARRAY
   Read_Next (Reader); --  START_OBJECT or END_ARRAY
   while Event_Kind (Reader) /= End_Array loop
      Read_Next (Reader); --  "index"
      Read_Next (Reader);
      DDCD_Index := Display_Category_Index_Type (As_Integer (Number_Value (Reader)));

      --  Start DestinyDisplayPropertiesDefinition
      Wait_Until_Key (Reader, "name");
      Read_Next (Reader);

      Wait_Until_Event (Reader, End_Object);
      --  End DestinyDisplayPropertiesDefinition
      Read_Next (Reader);

      Vendor.Display_Categories.Insert
        (DDCD_Index, (Name => VS2UB (String_Value (Reader))));

      --  No more info needed
      Wait_Until_Event (Reader, End_Object);
      Read_Next (Reader); --  START_OBJECT or END_ARRAY
   end loop;

   --------------
   -- itemList --
   --------------
   Wait_Until_Key (Reader, "itemList");
   Read_Next (Reader); --  START_ARRAY
   Read_Next (Reader); --  START_OBJECT or END_ARRAY
   while Event_Kind (Reader) /= End_Array loop
      Read_Destiny_Vendor_Item :
         declare
            DVID       : Destiny_Vendor_Item_Definition;
            DVID_Index : Vendor_Item_Index_Type;
         begin
            Read_Next (Reader); --  "vendorItemIndex"
            Read_Next (Reader);
            DVID_Index :=
              Vendor_Item_Index_Type (As_Integer (Number_Value (Reader)));

            Read_Next (Reader); --  "itemHash"
            Read_Next (Reader);
            DVID.Item_Hash :=
              Manifest_Hash (As_Integer (Number_Value (Reader)));

            Read_Next (Reader); --  "quantity"
            Read_Next (Reader);
            DVID.Quantity :=
              Quantity_Type (As_Integer (Number_Value (Reader)));

            Read_Next (Reader); --  "failureIndexes"
            Read_Next (Reader); --  START_ARRAY
            Read_Next (Reader); --  NUMBER_VALUE or END_ARRAY
            while Event_Kind (Reader) /= End_Array loop
               DVID.Failure_Indexes.Append
                 (Failure_Index_Type (As_Integer (Number_Value (Reader))));
               Read_Next (Reader); --  NUMBER_VALUE or END_ARRAY
            end loop;

            Wait_Until_Key (Reader, "displayCategoryIndex");
            Read_Next (Reader);
            DVID.Display_Category_Index :=
              Display_Category_Index_Type (As_Integer (Number_Value (Reader)));

            Wait_Until_Key (Reader, "socketOverrides");
            Read_Next (Reader); --  START_ARRAY
            Read_Next (Reader); --  START_OBJECT or END_ARRAY
            while Event_Kind (Reader) /= End_Array loop
               Add_Socket_Override :
                  declare
                     DVISO : Destiny_Vendor_Item_Socket_Override_Type;
                  begin
                     Read_Next
                       (Reader); -- singleItemHash or randomizedOptionsCount
                     if VS2S (Key_Name (Reader)) = "singleItemHash" then
                        Read_Next (Reader);
                        DVISO.Single_Item_Hash :=
                          Manifest_Hash (As_Integer (Number_Value (Reader)));
                        Read_Next (Reader); --  randomizedOptionsCount
                     end if;

                     Read_Next (Reader);
                     DVISO.Randomized_Options_Count :=
                       Quantity_Type (As_Integer (Number_Value (Reader)));

                     Read_Next (Reader); --  "socketTypeHash"
                     Read_Next (Reader);
                     DVISO.Socket_Type_Hash :=
                       Manifest_Hash (As_Integer (Number_Value (Reader)));

                     DVID.Socket_Overrides.Append (DVISO);

                     --  All done
                     Read_Next (Reader); -- END_OBJECT
                     Read_Next (Reader); -- START_OBJECT or END_ARRAY
                  end Add_Socket_Override;
            end loop;

            Read_Next (Reader); --  "unpurchaseable"
            Read_Next (Reader);
            DVID.Unpurchaseable := Boolean_Value (Reader);

            Vendor.Items.Insert (DVID_Index, DVID);

            --  No more info needed
            Read_Next (Reader); --  START_OBJECT or END_ARRAY
         end Read_Destiny_Vendor_Item;
   end loop;

   --------------
   -- COMPLETE --
   --------------
   --  Add finished Vendor to Manifest
   Put_Line (Vendor'Image);
   The_Manifest.Destiny_Vendors.Insert (Hash, Vendor);
   raise Program_Error with "Intended behaviour for debugging.";
end API.Manifest.Vendor_Callback;
