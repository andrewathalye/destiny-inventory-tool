pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

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
     (M : Manifest_Type; I : Item_Type) return Item_Description
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
        (Name                    => Manifest_Item.Name,
         Description             => Manifest_Item.Description,
         Item_Hash               => I.Item_Hash,
         Item_Instance_ID        => I.Item_Instance_ID,
         Quantity                => I.Quantity,
         Max_Stack_Size          => Manifest_Item.Max_Stack_Size,
         Location                => I.Location,
         Bucket_Hash             => I.Bucket_Hash,
         Default_Bucket_Hash     => Manifest_Item.Bucket_Type_Hash,
         Bucket_Location => Bucket_Location_Type'Enum_Val (I.Bucket_Hash),
         Default_Bucket_Location =>
           Bucket_Location_Type'Enum_Val (Manifest_Item.Bucket_Type_Hash),
         Category =>
           M.Destiny_Inventory_Buckets (Manifest_Item.Bucket_Type_Hash)
             .Category,
         State           => I.State,
         Allow_Actions   => Manifest_Item.Allow_Actions,
         Transfer_Status => I.Transfer_Status,
         Icon_Path       => Override_Item.Icon_Path,
         Watermark_Path  =>
           (if
              I.Version_Number /= -1
            then
              Manifest_Item.Display_Version_Watermark_Icons
                (Natural (I.Version_Number))
            else Manifest_Item.Watermark_Path),
         Style_Overridden                 => I.Override_Style_Item_Hash /= 0,
         Postmaster_Pull_Has_Side_Effects =>
           Manifest_Item.Postmaster_Pull_Has_Side_Effects,
         Item_Type                       => Manifest_Item.Item_Type,
         Tier_Type                       => Manifest_Item.Tier_Type,
         Item_Type_And_Tier_Display_Name =>
           Manifest_Item.Item_Type_And_Tier_Display_Name);
   exception
      when Constraint_Error =>
         Put_Line
           (Standard_Error,
            "[ERROR] An error occurred when parsing the item with hash" &
            I.Item_Hash'Image);
         --  Return dummy item to avoid a hard crash Enums are set to avoid
         --  invalid values

         declare

            D : Item_Description;

         begin
            D.Name := +("Unknown Item No." & I.Item_Hash'Image);
            D.Bucket_Location         := Unknown;
            D.Default_Bucket_Location := Unknown;
            D.Item_Type               := None;
            D.Tier_Type               := Common;
            return D;
         end;
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
