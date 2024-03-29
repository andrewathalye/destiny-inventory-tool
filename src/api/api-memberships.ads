with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package API.Memberships is
   --  Types
   type Bungie_Platform_Type is
     (None, Xbox, PSN, Steam, Blizzard, Stadia, EGS, Demon, Next);
   for Bungie_Platform_Type use (0, 1, 2, 3, 4, 5, 6, 10, 254);
   --  Referenced as GUIC in the code for brevity

   type Group_User_Info_Card_Type is record
      Last_Seen_Display_Name : Unbounded_String;
      Membership_Type        : Bungie_Platform_Type;
      Membership_ID          : Unbounded_String;
      --  Fields omitted
   end record;

   package GUIC_Vector is new Ada.Containers.Vectors
     (Natural, Group_User_Info_Card_Type);
   subtype Group_User_Info_Card_Vector is GUIC_Vector.Vector;

   type Bungie_Net_User_Type is record
      Membership_ID : Unbounded_String; -- For Bungie.net
      Unique_Name   : Unbounded_String;
      Display_Name  : Unbounded_String;
      Locale        : Unbounded_String;
      --  Fields omitted
   end record;

   type Membership_Type is record
      Destiny_Memberships   : Group_User_Info_Card_Vector;
      Primary_Membership_ID : Unbounded_String; --  Nullable
      Bungie_Net_User : Bungie_Net_User_Type;
   end record;

   --  Subprograms
   function Find_Default_Platform
     (M : Membership_Type) return Bungie_Platform_Type;
   --  Return the primary platform associated with the given
   --  Membership. This platform cannot be used directly for
   --  API calls, but may be compared against any other platform
   --  data obtained via API calls.

   function Find_Default_Platform_ID (M : Membership_Type) return String;
   --  Returns the numeric value of Bungie_Platform_Type corresponding
   --  to the given Membership. This value is represented as a String
   --  for ease of use with API calls.

end API.Memberships;
