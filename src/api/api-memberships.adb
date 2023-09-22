pragma Ada_2022;

package body API.Memberships is

   function Find_Default_Platform
     (M : Membership_Type) return Bungie_Platform_Type
   is
   begin
      for DM of M.Destiny_Memberships loop
         if DM.Membership_ID = M.Primary_Membership_ID then
            return DM.Membership_Type;
         end if;
      end loop;
      return None;
   end Find_Default_Platform;

   function Find_Default_Platform_ID (M : Membership_Type) return String is

      Result : constant String := Find_Default_Platform (M)'Enum_Rep'Image;

   begin
      return Result (Result'First + 1 .. Result'Last);
   end Find_Default_Platform_ID;

end API.Memberships;
