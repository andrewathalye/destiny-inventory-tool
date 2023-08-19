with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

--  AWS
with AWS.Headers;

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
      Primary_Membership_ID : Unbounded_String :=
        Null_Unbounded_String; -- For platform profile
      Bungie_Net_User : Bungie_Net_User_Type;
   end record;

   --  Subprograms
   function Find_Default_Platform
     (M : Membership_Type) return Bungie_Platform_Type;
   function Find_Default_Platform_ID (M : Membership_Type) return String;

   --  Acquisition
   function Get_Memberships
     (Headers : AWS.Headers.List) return Membership_Type;
   --  Get a Membership_Type using authentication headers from Headers
   --  In most cases, the next step is to combine Headers and Membership using
   --  API.Identification to obtain an Auth_Type record, which is accepted
   --  by all other endpoints for identification
end API.Memberships;
