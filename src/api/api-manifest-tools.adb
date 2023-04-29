pragma Ada_2022;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

--  Local Packages
with Shared.Strings; use Shared.Strings;

package body API.Manifest.Tools is
--  Private Subprograms

   function Get_Gender
     (M : Manifest_Type; C : Character_Type) return Destiny_Gender_Type is
     (M.Destiny_Genders (C.Gender_Hash).Gender_Type);

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
      Instance_ID : constant Item_Instance_ID_Type :=
        (if Length (I.Item_Instance_ID) = 0 then 0
         else Item_Instance_ID_Type'Value (+I.Item_Instance_ID));

   begin
      return
        (Name             => Manifest_Item.Name,
         Description      => Manifest_Item.Description,
         Item_Hash        => I.Item_Hash,
         Item_Instance_ID => I.Item_Instance_ID,

          --  Stack
          Quantity      => I.Quantity,
         Max_Stack_Size => Manifest_Item.Max_Stack_Size,
         Location       => I.Location,

          --  Buckets
          Bucket_Hash            => I.Bucket_Hash,
         Default_Bucket_Hash     => Manifest_Item.Bucket_Type_Hash,
         Bucket_Location => Bucket_Location_Type'Enum_Val (I.Bucket_Hash),
         Default_Bucket_Location =>
           Bucket_Location_Type'Enum_Val (Manifest_Item.Bucket_Type_Hash),

         --  Item state
         Category =>
           M.Destiny_Inventory_Buckets (Manifest_Item.Bucket_Type_Hash)
             .Category,
         State           => I.State,
         Allow_Actions   => Manifest_Item.Allow_Actions,
         Transfer_Status => I.Transfer_Status,

          --  Item style
          Icon_Path     => Override_Item.Icon_Path,
         Watermark_Path =>
           (if
              I.Version_Number /= -1
            then
              Manifest_Item.Display_Version_Watermark_Icons
                (Natural (I.Version_Number))
            else Manifest_Item.Watermark_Path),
         Style_Overridden => I.Override_Style_Item_Hash /= 0,

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
              Instance_ID /= 0 and
              P.Item_Components.Instances.Contains (Instance_ID)
            then
              P.Item_Components.Instances (Instance_ID).Primary_Stat_Value
            else 0),
         Energy_Capacity =>
           (if
              Instance_ID /= 0 and
              P.Item_Components.Instances.Contains (Instance_ID)
            then
              P.Item_Components.Instances (Instance_ID).Energy_Capacity
            else 0),
         Energy_Used =>
           (if
              Instance_ID /= 0 and
              P.Item_Components.Instances.Contains (Instance_ID)
            then
              P.Item_Components.Instances (Instance_ID).Energy_Used
            else 0),
         Stats =>
           (if
              Instance_ID /= 0 and
              P.Item_Components.Stats.Contains (Instance_ID)
            then
              P.Item_Components.Stats (Instance_ID)
            else []),
         Sockets =>
           (if
              Instance_ID /= 0 and
              P.Item_Components.Sockets.Contains (Instance_ID)
            then
              P.Item_Components.Sockets (Instance_ID)
            else []),
         Perks =>
           (if
              Instance_ID /= 0 and
              P.Item_Components.Perks.Contains (Instance_ID)
            then
              P.Item_Components.Perks (Instance_ID)
            else []));
   exception
      when E : Constraint_Error =>
         Put_Line
           (Standard_Error,
            "[ERROR] An error occurred when parsing the item with hash" &
            I.Item_Hash'Image);

         Put_Line (Standard_Error, Exception_Information (E));

         return
           (Name => +("Unknown Item No." & I.Item_Hash'Image), others => <>);
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

end API.Manifest.Tools;
