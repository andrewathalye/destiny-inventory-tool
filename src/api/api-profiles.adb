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

      begin
         Item.Item_Instance_ID := In_Item.Get ("itemInstanceId");
      exception
         when Constraint_Error => null;
      end;

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

      begin
         Item.Override_Style_Item_Hash := Manifest_Hash (Long_Long_Integer'(In_Item.Get ("overrideStyleItemHash").Get));
      exception
         when Constraint_Error => null;
      end;

      begin
         Item.Expiration_Date := In_Item.Get ("expirationDate");
      exception
         when Constraint_Error => null;
      end;

      begin
         Item.Version_Number := Integer_32 (Integer'(In_Item.Get ("versionNumber")));
      exception
         when Constraint_Error => null;
      end;

      return Item;
   end Parse_Item;
   pragma Inline (Parse_Item);

   function Parse_Stats (In_Stats : JSON_Value) return Stats_Map
   is
      Stats : Stats_Map;

      procedure Stats_Callback (Name : UTF8_String; Value : JSON_Value)
      is
      begin
         Stats.Insert (Manifest_Hash'Value (Name), Integer_32 (Integer'(Value.Get)));
      end Stats_Callback;
   begin
      Map_JSON_Object (In_Stats, Stats_Callback'Access);
      return Stats;
   end Parse_Stats;
   pragma Inline (Parse_Stats);

   function Parse_Characters (In_Characters : JSON_Value) return Character_List
   is
      Result : Character_List;

      procedure Character_Callback (Name : UTF8_String; Value : JSON_Value)
      is
         pragma Unreferenced (Name);

         function Safe_Get_Title_Record_Hash return Manifest_Hash
         is
         begin
            return Manifest_Hash (Long_Long_Integer'(Value.Get ("titleRecordHash").Get));
         exception
            when Constraint_Error => return 0;
         end Safe_Get_Title_Record_Hash;
      begin
         Result.Append (Character_Type'(
          Value.Get ("characterId"),
          Value.Get ("dateLastPlayed"),
          Integer_32 (Integer'(Value.Get ("light"))),
          Parse_Stats (Value.Get ("stats")),
          Manifest_Hash (Long_Long_Integer'(Value.Get ("raceHash").Get)),
          Manifest_Hash (Long_Long_Integer'(Value.Get ("genderHash").Get)),
          Manifest_Hash (Long_Long_Integer'(Value.Get ("classHash").Get)),
          Value.Get ("emblemPath"),
          Value.Get ("emblemBackgroundPath"),
          Safe_Get_Title_Record_Hash));
      end Character_Callback;
   begin
      Map_JSON_Object (In_Characters, Character_Callback'Access);
      return Result;
   end Parse_Characters;
   pragma Inline (Parse_Characters);

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
           Item.Get ("itemInstanceId"),
           Parse_Plug_Item_Hashes
            (Item.Get ("plugItemHashes"))));
      end loop;

      return Items;
   end Parse_Loadout_Items;
   pragma Inline (Parse_Loadout_Items);

   pragma Warnings (Off);
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

      --  Indexed by character id (technically Objects, not Arrays)
      Characters : constant JSON_Value :=
        Profile_Data.Get ("characters").Get ("data");

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

      Debug.Put_Line ("Get currencies");
      Parse_Currencies : for Item of Profile_Currencies loop
         Result.Profile_Currencies.Append (Parse_Item (Item));
      end loop Parse_Currencies;

      --  Note: Fragile
      Debug.Put_Line ("Get silver");
      Result.Platform_Silver (PSN) := Parse_Item (Platform_Silver.Get ("TigerPsn"));
      Result.Platform_Silver (Xbox) := Parse_Item (Platform_Silver.Get ("TigerXbox"));
      Result.Platform_Silver (Blizzard) := Parse_Item (Platform_Silver.Get ("TigerBlizzard"));
      Result.Platform_Silver (Stadia) := Parse_Item (Platform_Silver.Get ("TigerStadia"));
      Result.Platform_Silver (Steam) := Parse_Item (Platform_Silver.Get ("TigerSteam"));
      Result.Platform_Silver (Next) := Parse_Item (Platform_Silver.Get ("BungieNext"));
      Result.Platform_Silver (EGS) := Parse_Item (Platform_Silver.Get ("TigerEgs"));

      Debug.Put_Line ("Get characters");
      Result.Characters := Parse_Characters (Characters);

      Debug.Put_Line ("Get charinvs");
    Parse_Character_Inventories : for Character of Result.Characters loop
       Result.Character_Inventories.Insert (Character.Character_ID, Item_Vectors.Empty_Vector);
       Add_Inventory_Items : for Item of JSON_Array'(Character_Inventories.Get (+Character.Character_ID).Get ("items")) loop
         Result.Character_Inventories (Character.Character_ID).Append (Parse_Item (Item));
       end loop Add_Inventory_Items;
    end loop Parse_Character_Inventories;

      Debug.Put_Line ("Get loadouts");
    Parse_Character_Loadouts : for Character of Result.Characters loop
       Result.Character_Loadouts.Insert (Character.Character_ID, Loadout_Vectors.Empty_Vector);
       Add_Loadouts : for Loadout of JSON_Array'(Character_Loadouts.Get (+Character.Character_ID).Get ("loadouts")) loop
         Result.Character_Loadouts (Character.Character_ID).Append (Loadout_Type'(
          Manifest_Hash (Long_Long_Integer'(Loadout.Get ("colorHash").Get)),
          Manifest_Hash (Long_Long_Integer'(Loadout.Get ("iconHash").Get)),
          Manifest_Hash (Long_Long_Integer'(Loadout.Get ("nameHash").Get)),
          Parse_Loadout_Items (Loadout.Get ("items"))));
       end loop Add_Loadouts;
    end loop Parse_Character_Loadouts;

      Debug.Put_Line ("Get chareqs");
    Parse_Character_Equipment : for Character of Result.Characters loop
       Result.Character_Equipment.Insert (Character.Character_ID, Item_Vectors.Empty_Vector);
       Add_Equipped_Items : for Item of JSON_Array'(Character_Equipment.Get (+Character.Character_ID).Get ("items")) loop
         Result.Character_Equipment (Character.Character_ID).Append (Parse_Item (Item));
       end loop Add_Equipped_Items;
    end loop Parse_Character_Equipment;

      return Result;
   end Get_Profile;
end API.Profiles;
