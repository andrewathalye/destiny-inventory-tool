pragma Ada_2022;

--  VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
use VSS.JSON.Pull_Readers;
use VSS.JSON;
with VSS.Text_Streams.Memory_UTF8_Input;
use VSS.Text_Streams.Memory_UTF8_Input;
use VSS.Text_Streams;
with VSS.Stream_Element_Vectors.Conversions;
use VSS.Stream_Element_Vectors.Conversions;

--  Local Packages
with API.Debug;

with Shared.JSON;    use Shared.JSON;
with Shared.Strings; use Shared.Strings;
with Shared.Debug;

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

      Reader : JSON_Simple_Pull_Reader;
      Stream : Memory_UTF8_Input_Stream_Access := new Memory_UTF8_Input_Stream;
      Result : Membership_Type;

   begin
      Shared.Debug.Put_Line ("Get memberships");
      Set_Data
        (Stream.all,
         To_Stream_Element_Vector
           (Tasks.Download.Download
              (+(API_Root & "/User/GetMembershipsForCurrentUser/"),
               Needs_Auth => True,
               Caching    => API.Debug.Caching)));
      Set_Stream (Reader, Input_Text_Stream_Access (Stream));

      Wait_Until_Key (Reader, "destinyMemberships");
      Read_Next (Reader); -- Start_Array

      Read_Destiny_Memberships :
         loop
            Read_Next (Reader);

            case Event_Kind (Reader) is
               when Start_Object =>
                  Submit_GUIC :
                     declare

                        GUIC : Group_User_Info_Card_Type;

                     begin
                        Wait_Until_Key (Reader, "LastSeenDisplayName");
                        Read_Next (Reader);
                        GUIC.Last_Seen_Display_Name :=
                          VS2UB (String_Value (Reader));

                        Wait_Until_Key (Reader, "membershipType");
                        Read_Next (Reader);
                        GUIC.Membership_Type :=
                          Bungie_Platform_Type'Enum_Val
                            (As_Integer (Number_Value (Reader)));

                        Wait_Until_Key (Reader, "membershipId");
                        Read_Next (Reader);
                        GUIC.Membership_ID := VS2UB (String_Value (Reader));
                        Result.Destiny_Memberships.Append (GUIC);

                        Wait_Until_Event (Reader, End_Object);
                     end Submit_GUIC;

               when End_Array =>
                  exit Read_Destiny_Memberships;

               when others =>
                  raise Program_Error;
            end case;
         end loop Read_Destiny_Memberships;

         --  Primary_Membership_ID
      Wait_Until_Key (Reader, "primaryMembershipId");
      Read_Next (Reader);
      Result.Primary_Membership_ID := VS2UB (String_Value (Reader));

      --  Bungie_Net_User
      Wait_Until_Key (Reader, "bungieNetUser");
      Wait_Until_Key (Reader, "membershipId");
      Read_Next (Reader);
      Result.Bungie_Net_User.Membership_ID := VS2UB (String_Value (Reader));

      Wait_Until_Key (Reader, "uniqueName");
      Read_Next (Reader);
      Result.Bungie_Net_User.Unique_Name := VS2UB (String_Value (Reader));

      Wait_Until_Key (Reader, "displayName");
      Read_Next (Reader);
      Result.Bungie_Net_User.Display_Name := VS2UB (String_Value (Reader));

      Wait_Until_Key (Reader, "locale");
      Read_Next (Reader);
      Result.Bungie_Net_User.Locale := VS2UB (String_Value (Reader));

      Free (Stream);
      return Result;
   end Get_Memberships;

end API.Memberships;
