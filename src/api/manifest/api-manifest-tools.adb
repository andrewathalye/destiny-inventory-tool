pragma Ada_2022;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

--  Local Packages
with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared;

package body API.Manifest.Tools is
   --  Private Subprograms
   function Find_Objective_By_Hash
     (Objectives : Plug_Objective_List;
      Hash       : Destiny_Objective_Definition_Manifest_Hash)
      return Plug_Objective_Type
   is
   begin
      Search_Objectives :
         for Objective of Objectives loop
            if Objective.Objective_Hash = Hash then
               return Objective;
            end if;
         end loop Search_Objectives;
      raise Constraint_Error with "No objective found for hash" & Hash'Image;
   end Find_Objective_By_Hash;

   function Find_Socket_By_Plug_Hash
     (Sockets : Consolidated_Socket_List;
      Hash    : Destiny_Inventory_Item_Definition_Manifest_Hash)
      return Consolidated_Socket_Type
   is
   begin
      Search_Sockets :
         for Socket of Sockets loop
            if Socket.Plug_Hash = Hash then
               return Socket;
            end if;
         end loop Search_Sockets;

      raise Constraint_Error with "No socket found for hash" & Hash'Image;
   end Find_Socket_By_Plug_Hash;

   function Get_Gender
     (M : Manifest_Type; C : Character_Type) return Destiny_Gender_Type is
     (M.Destiny_Genders (C.Gender_Hash).Gender_Type);

   --  Note: The consolidated list does not include any completely empty sockets (e.g. Plug_Hash = 0)
   function Consolidate_Sockets
     (Sockets    : Socket_List;
      Objectives : Plug_Objective_Map)
      return Consolidated_Socket_List
   is
      List : Consolidated_Socket_List;
   begin
      Add_Sockets :
         for Socket of Sockets loop
            if Socket.Plug_Hash /= 0 then
               List.Append
                 (Consolidated_Socket_Type'
                    (Plug_Hash  => Socket.Plug_Hash,
                     Is_Enabled => Socket.Is_Enabled,
                     Is_Visible => Socket.Is_Visible,
                     Objectives =>
                       (if
                          Objectives.Contains (Socket.Plug_Hash)
                        then
                          Objectives (Socket.Plug_Hash)
                        else [])));
            end if;
         end loop Add_Sockets;

      return List;
   end Consolidate_Sockets;

   --  Public Subprograms
   function Get_Description
     (M : Manifest_Type; C : Character_Type) return String is
     ((+M.Destiny_Races (C.Race_Hash) (Get_Gender (M, C))) & " " &
      (+M.Destiny_Classes (C.Class_Hash) (Get_Gender (M, C))));

   function Get_Description
     (M : Manifest_Type;
      P : Profile_Type;
      I : Item_Type)
      return Item_Description
   is

      Manifest_Item : constant Destiny_Inventory_Item_Definition :=
        M.Destiny_Inventory_Items (I.Item_Hash);
      Override_Item : constant Destiny_Inventory_Item_Definition :=
        (case I.Override_Style_Item_Hash is
           when 0      => Manifest_Item,
           when others =>
             M.Destiny_Inventory_Items (I.Override_Style_Item_Hash));

   begin
      return
        (Name             => Manifest_Item.Name,
         Description      => Manifest_Item.Description,
         Item_Hash        => I.Item_Hash,
         Item_Instance_ID => I.Item_Instance_ID,

          --  Stack
          Quantity      => I.Quantity,
         Max_Stack_Size => Manifest_Item.Max_Stack_Size,

          --  Buckets
          Location               => I.Location,
         Bucket_Hash             => I.Bucket_Hash,
         Default_Bucket_Hash     => Manifest_Item.Bucket_Type_Hash,
         Bucket_Location => Bucket_Location_Type'Enum_Val (I.Bucket_Hash),
         Default_Bucket_Location =>
           Bucket_Location_Type'Enum_Val (Manifest_Item.Bucket_Type_Hash),
         Transfer_Status => I.Transfer_Status,

          --  Item state
          Category =>
           M.Destiny_Inventory_Buckets (Manifest_Item.Bucket_Type_Hash)
             .Category,
         State         => I.State,
         Allow_Actions => Manifest_Item.Allow_Actions,

          --  Item style
          Icon_Path     => Override_Item.Icon_Path,
          Secondary_Icon_Path => Override_Item.Secondary_Icon_Path,
          Secondary_Overlay_Path => Override_Item.Secondary_Overlay_Path,
          Secondary_Special_Path => Override_Item.Secondary_Special_Path,
         Watermark_Path =>
           (if
              I.Version_Number /= -1
            then
              Manifest_Item.Display_Version_Watermark_Icons
                (Natural (I.Version_Number))
            else Manifest_Item.Watermark_Path),
         Default_Damage_Type_Hash => Manifest_Item.Default_Damage_Type_Hash,
         Style_Overridden         => I.Override_Style_Item_Hash /= 0,

          --  Item qualities
          Postmaster_Pull_Has_Side_Effects =>
           Manifest_Item.Postmaster_Pull_Has_Side_Effects,
         Item_Type                       => Manifest_Item.Item_Type,
         Tier_Type                       => Manifest_Item.Tier_Type,
         Item_Type_And_Tier_Display_Name =>
           Manifest_Item.Item_Type_And_Tier_Display_Name,

          --  Instance-specific traits
          Light_Level =>
           (if
              I.Item_Instance_ID /= -1 and
              P.Item_Components.Instances.Contains (I.Item_Instance_ID)
            then
              P.Item_Components.Instances (I.Item_Instance_ID).Light_Level
            else -1),
         Energy_Capacity =>
           (if
              I.Item_Instance_ID /= -1 and
              P.Item_Components.Instances.Contains (I.Item_Instance_ID)
            then
              P.Item_Components.Instances (I.Item_Instance_ID).Energy_Capacity
            else -1),
         Energy_Used =>
           (if
              I.Item_Instance_ID /= -1 and
              P.Item_Components.Instances.Contains (I.Item_Instance_ID)
            then
              P.Item_Components.Instances (I.Item_Instance_ID).Energy_Used
            else -1),
         Stats =>
           (if
              I.Item_Instance_ID /= -1 and
              P.Item_Components.Stats.Contains (I.Item_Instance_ID)
            then
              P.Item_Components.Stats (I.Item_Instance_ID)
            else []),
         Sockets =>
           Consolidate_Sockets
             (Sockets =>
                (if
                   I.Item_Instance_ID /= -1
                   and then P.Item_Components.Sockets.Contains
                     (I.Item_Instance_ID)
                 then
                   P.Item_Components.Sockets (I.Item_Instance_ID)
                 else []),
              Objectives =>
                (if
                   I.Item_Instance_ID /= -1
                   and then P.Item_Components.Plug_Objectives.Contains
                     (I.Item_Instance_ID)
                 then
                   P.Item_Components.Plug_Objectives (I.Item_Instance_ID)
                 else [])),
         Perks =>
           (if
              I.Item_Instance_ID /= -1 and
              P.Item_Components.Perks.Contains (I.Item_Instance_ID)
            then
              P.Item_Components.Perks (I.Item_Instance_ID)
            else []));
   exception
      when E : Constraint_Error =>
         Put_Line
           (Standard_Error,
            "[ERROR] An error occurred when parsing the item with hash" &
            I.Item_Hash'Image);

         Put_Line (Standard_Error, Exception_Information (E));

         Debug.Put_Line (I'Image);

         return
           (Name => +("Unknown Item No." & I.Item_Hash'Image), others => <>);
   end Get_Description;

   --  Get a description for a "generic", uninstanced manifest item. Used currently for displaying plugs.
   function Get_Description
     (M    : Manifest_Type;
      Hash : Destiny_Inventory_Item_Definition_Manifest_Hash)
      return Item_Description
   is
      MDID : constant Destiny_Inventory_Item_Definition :=
        M.Destiny_Inventory_Items (Hash);
   begin
      return
        (Name        => MDID.Name,
         Description => MDID.Description,
         Item_Hash   => Hash,

         Default_Bucket_Hash     => MDID.Bucket_Type_Hash,
         Default_Bucket_Location =>
           Bucket_Location_Type'Enum_Val (MDID.Bucket_Type_Hash),

         Icon_Path      => MDID.Icon_Path,
         Secondary_Icon_Path => MDID.Secondary_Icon_Path,
         Secondary_Overlay_Path => MDID.Secondary_Overlay_Path,
         Secondary_Special_Path => MDID.Secondary_Special_Path,
         Watermark_Path => MDID.Watermark_Path,

         Item_Type                       => MDID.Item_Type,
         Tier_Type                       => MDID.Tier_Type,
         Item_Type_And_Tier_Display_Name =>
           MDID.Item_Type_And_Tier_Display_Name,

         others =>
           <> --  No more information available
      );
   end Get_Description;

   function Get_Title
     (M : Manifest_Type; C : Character_Type) return Unbounded_String
   is
   begin
      if C.Title_Record_Hash /= 0 then
         return M.Destiny_Titles (C.Title_Record_Hash) (Get_Gender (M, C));
      end if;
      return Null_Unbounded_String;
   end Get_Title;

   function Get_Weapon_Level (D : Item_Description) return Quantity_Type is
      --  Variables
      Shaped_Weapon_Socket : Consolidated_Socket_Type;

      --  Constants
      Shaped_Weapon_Plug_Hash_1 : constant := 659_359_923;
      Shaped_Weapon_Plug_Hash_2 : constant := 1_922_808_508;
      Shaped_Weapon_Plug_Hash_3 : constant := 4_029_346_515;

      Weapon_Crafting_Level_Objective_Hash : constant := 3_077_315_735;
   begin
      --  Check to make sure the input item is a crafted weapon
      if not D.State.Crafted then
         raise Invalid_Item with "Item not crafted";
      elsif D.Item_Type /= Weapon then
         raise Invalid_Item with "Item is not a weapon";
      end if;

      --  The Shaped Weapon plug can have three different hashes.
      --  TODO: Find a better way
      Find_Shaped_Weapon_Socket :
         begin
            Shaped_Weapon_Socket :=
              Find_Socket_By_Plug_Hash (D.Sockets, Shaped_Weapon_Plug_Hash_1);
         exception
            when Constraint_Error =>
               begin
                  Shaped_Weapon_Socket :=
                    Find_Socket_By_Plug_Hash
                      (D.Sockets, Shaped_Weapon_Plug_Hash_2);
               exception
                  when Constraint_Error =>
                     Shaped_Weapon_Socket :=
                       Find_Socket_By_Plug_Hash
                         (D.Sockets, Shaped_Weapon_Plug_Hash_3);
               end;
         end Find_Shaped_Weapon_Socket;

      return
        Find_Objective_By_Hash
          (Objectives => Shaped_Weapon_Socket.Objectives,
           Hash       => Weapon_Crafting_Level_Objective_Hash)
          .Progress;
   end Get_Weapon_Level;

end API.Manifest.Tools;
