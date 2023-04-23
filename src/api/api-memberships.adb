pragma Ada_2022;

--  GNATCOLL
with GNATCOLL.JSON; use GNATCOLL.JSON;

--  Local Packages
with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared;
with Tasks.Download;

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

   function Get_Memberships return Membership_Type is

      Result : Membership_Type;

      All_Data : constant JSON_Value :=
        Read
          (Strm =>
             Tasks.Download.Download
               (+(API_Root & "/User/GetMembershipsForCurrentUser/"),
                Needs_Auth => True),
           Filename => "<membership_data>")
          .Get
          ("Response");
      Destiny_Memberships : constant JSON_Array :=
        All_Data.Get ("destinyMemberships");
      Bungie_Net_User : constant JSON_Value := All_Data.Get ("bungieNetUser");
   begin
      Debug.Put_Line ("Get memberships");
      for Membership of Destiny_Memberships loop
         Result.Destiny_Memberships.Append
           (Group_User_Info_Card_Type'
              (Membership.Get ("LastSeenDisplayName"),
               Bungie_Platform_Type'Enum_Val
                 (Integer'(Membership.Get ("membershipType"))),
               Membership.Get ("membershipId")));
      end loop;

      Result.Primary_Membership_ID := All_Data.Get ("primaryMembershipId");
      Result.Bungie_Net_User       :=
        (Bungie_Net_User.Get ("membershipId"),
         Bungie_Net_User.Get ("uniqueName"),
         Bungie_Net_User.Get ("displayName"),
         Bungie_Net_User.Get ("locale"));

      return Result;
   end Get_Memberships;

end API.Memberships;
