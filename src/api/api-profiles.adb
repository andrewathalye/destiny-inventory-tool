pragma Ada_2022;

--  GNATCOLL
with GNATCOLL.JSON; use GNATCOLL.JSON;

--  Local Packages
with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared;
with Tasks.Download;

package body API.Profiles is
   function Parse_Item
     (In_Item : JSON_Value) return Item_Type
   is
      Item : Item_Type;
   begin
      Item.Item_Hash := Manifest_Hash (Long_Long_Integer'(In_Item.Get ("itemHash").Get));
      Item.Item_Instance_ID := In_Item.Get ("itemInstanceId");
      Item.Quantity := Integer_32 (Integer'(In_Item.Get ("quantity")));
      Item.Bind_Status := Bind_Status_Type'Enum_Val (Integer'(In_Item.Get ("bindStatus")));
      Item.Location := Item_Location_Type'Enum_Val (Integer'(In_Item.Get ("location")));
      Item.Bucket_Hash := Manifest_Hash (Long_Long_Integer'(In_Item.Get ("bucketHash").Get));
      Item.Transfer_Status := Transfer_Status_Type'Enum_Val (Integer'(In_Item.Get ("transferStatus")));
      Item.Lockable := In_Item.Get ("lockable");

      Read_State : declare
         Temp : constant Unsigned_32 := Unsigned_32 (Integer'(In_Item.Get ("state")));
      begin
         Item.State :=
          (Locked               => (Temp and 2#1#) > 0,
          Tracked               => (Temp and 2#10#) > 0,
          Masterwork            => (Temp and 2#100#) > 0,
          Crafted               => (Temp and 2#1000#) > 0,
          Highlighted_Objective => (Temp and 2#1_0000#) > 0);
      end Read_State;

      Item.Override_Style_Item_Hash := Manifest_Hash (Long_Long_Integer'(In_Item.Get ("overrideStyleItemHash").Get));
      Item.Expiration_Date := In_Item.Get ("expirationDate");
      Item.Version_Number := Integer_32 (Integer'(In_Item.Get ("versionNumber")));

      return Item;
   end Parse_Item;
   pragma Inline (Parse_Item);

   function Parse_Stats (In_Stats : JSON_Value) return Stats_Map
   is
      Stats : Stats_Map;

      procedure Stats_Callback (Name : UTF8_String; Value : JSON_Value)
      is
      begin
         Stats.Insert (Manifest_Hash'Value (Name), Integer_32'Value (Value.Get));
      end Stats_Callback;
   begin
      Map_JSON_Object (In_Stats, Stats_Callback'Access);
      return Stats;
   end Parse_Stats;
   pragma Inline (Parse_Stats);

   function Parse_Loadout_Items (In_Items : JSON_Array) return Loadout_Item_List
   is
      function Parse_Plug_Item_Hashes (In_Hashes : JSON_Array) return Manifest_Hash_List
      is
         Hashes : Manifest_Hash_List;
      begin
         for Hash of In_Hashes loop
            Hashes.Append (Manifest_Hash (Long_Long_Integer'(Hash.Get)));
         end loop;

         return Hashes;
      end Parse_Plug_Item_Hashes;

      Items : Loadout_Item_List;
   begin
      for Item of In_Items loop
         Items.Append
          (Loadout_Item_Type'(
           Item.Get ("instanceId"),
           Parse_Plug_Item_Hashes
            (Item.Get ("plugItemHashes"))));
      end loop;

      return Items;
   end Parse_Loadout_Items;
   pragma Inline (Parse_Loadout_Items);

   function Get_Profile (M : Membership_Type) return Profile_Type is

      Result : Profile_Type;

      Profile_Data : constant JSON_Value :=
        Read
          (Strm =>
             Tasks.Download.Download
               ((+
                 (API_Root & "/Destiny2/" &
                  Memberships.Find_Default_Platform_ID (M) & "/Profile/" &
                  (+M.Primary_Membership_ID) & "/" &
                  "?components=ProfileInventories,ProfileCurrencies,PlatformSilver,Characters,CharacterInventories,CharacterProgressions,CharacterEquipment,CharacterLoadouts")),
                 Needs_Auth => True),
           Filename => "<profile_data>")
          .Get
          ("Response");

      Profile_Inventory : constant JSON_Array :=
        Profile_Data.Get ("profileInventory").Get ("data").Get ("items");

      Profile_Currencies : constant JSON_Array :=
        Profile_Data.Get ("profileCurrencies").Get ("data").Get ("items");

      --  Indexed by platform id
      Platform_Silver : constant JSON_Value :=
        Profile_Data.Get ("platformSilver").Get ("data").Get
          ("platformSilver");

      Characters : constant JSON_Array :=
        Profile_Data.Get ("characters").Get ("data");

      --  Indexed by character id
      Character_Inventories : constant JSON_Value :=
        Profile_Data.Get ("characterInventories").Get ("data");

      Character_Loadouts : constant JSON_Value :=
        Profile_Data.Get ("characterLoadouts").Get ("data");

      Character_Equipment : constant JSON_Value :=
        Profile_Data.Get ("characterEquipment").Get ("data");

   begin
      Debug.Put_Line ("Get profiles");

      Parse_Items : for Item of Profile_Inventory loop
         Result.Profile_Inventory.Append (Parse_Item (Item));
      end loop Parse_Items;

      Parse_Currencies : for Item of Profile_Currencies loop
         Result.Profile_Currencies.Append (Parse_Item (Item));
      end loop Parse_Currencies;

      Parse_Silver : for Platform in Platform_Silver_Array'Range loop
         Result.Platform_Silver (Platform) := Parse_Item
     (Platform_Silver.Get
      (Platform'Enum_Rep'Image (1 .. Platform'Enum_Rep'Image'Last)));
      end loop Parse_Silver;

      Parse_Characters : for Character of Characters loop
         Result.Characters.Append (Character_Type'(
          Character.Get ("characterId"),
          Character.Get ("datelastPlayed"),
          Integer_32 (Integer'(Character.Get ("light"))),
          Parse_Stats (Character.Get ("stats")),
          Manifest_Hash (Long_Long_Integer'(Character.Get ("raceHash").Get)),
          Manifest_Hash (Long_Long_Integer'(Character.Get ("genderHash").Get)),
          Manifest_Hash (Long_Long_Integer'(Character.Get ("classHash").Get)),
          Character.Get ("emblemPath"),
          Character.Get ("emblemBackgroundPath"),
          Manifest_Hash (Long_Long_Integer'(Character.Get ("titleRecordHash").Get))
        ));
      end loop Parse_Characters;

    Parse_Character_Inventories : for Character of Result.Characters loop
       Add_Inventory_Items : for Item of JSON_Array'(Character_Inventories.Get (+Character.Character_ID)) loop
         Result.Character_Inventories (Character.Character_ID).Append (Parse_Item (Item));
       end loop Add_Inventory_Items;
    end loop Parse_Character_Inventories;

    Parse_Character_Loadouts : for Character of Result.Characters loop
       Add_Loadouts : for Loadout of JSON_Array'(Character_Loadouts.Get (+Character.Character_ID)) loop
         Result.Character_Loadouts (Character.Character_ID).Append (Loadout_Type'(
          Manifest_Hash (Long_Long_Integer'(Loadout.Get ("colorHash").Get)),
          Manifest_Hash (Long_Long_Integer'(Loadout.Get ("iconHash").Get)),
          Manifest_Hash (Long_Long_Integer'(Loadout.Get ("nameHash").Get)),
          Parse_Loadout_Items (Loadout.Get ("items"))));
       end loop Add_Loadouts;
    end loop Parse_Character_Loadouts;

    Parse_Character_Equipment : for Character of Result.Characters loop
       Add_Equipped_Items : for Item of JSON_Array'(Character_Equipment.Get (+Character.Character_ID)) loop
         Result.Character_Equipment (Character.Character_ID).Append (Parse_Item (Item));
       end loop Add_Equipped_Items;
    end loop Parse_Character_Equipment;

      return Result;
   end Get_Profile;
end API.Profiles;
