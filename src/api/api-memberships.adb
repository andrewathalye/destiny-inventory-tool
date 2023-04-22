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
with Shared.JSON;    use Shared.JSON;
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

      Reader : JSON_Simple_Pull_Reader;
      Stream : Memory_UTF8_Input_Stream_Access := new Memory_UTF8_Input_Stream;
      Result : Membership_Type;

   begin
      Debug.Put_Line ("Get memberships");
      Set_Data
        (Stream.all,
         To_Stream_Element_Vector
           (Tasks.Download.Download
              (+(API_Root & "/User/GetMembershipsForCurrentUser/"),
               Needs_Auth => True,
               Caching    => Debug_Caching)));
      Set_Stream (Reader, Input_Text_Stream_Access (Stream));
      --  "Response" and Start Object
      Read_Next (Reader); -- Start_Document
      Read_Next (Reader); -- Start_Object
      Read_Next (Reader);

      case Event_Kind (Reader) is
         when Key_Name =>
            if VS2S (Key_Name (Reader)) /= "Response" then
               raise Program_Error;
            end if;
            Read_Next (Reader);

            case Event_Kind (Reader) is
               when Start_Object =>
                  null;

               when others =>
                  raise Program_Error;
            end case;

         when others =>
            raise Program_Error;
      end case;
      --  Destiny Memberships
      Read_Next (Reader);

      case Event_Kind (Reader) is
         when Key_Name =>
            if VS2S (Key_Name (Reader)) /= "destinyMemberships" then
               raise Program_Error;
            end if;
            Read_Next (Reader);

            case Event_Kind (Reader) is
               when Start_Array => -- Memberships
                  Read_Destiny_Memberships :
                  loop
                     Read_Next (Reader);

                     case Event_Kind (Reader) is
                        when Start_Object =>
                           Submit_GUIC :
                           declare

                              GUIC : Group_User_Info_Card_Type;

                           begin
                              Read_GUIC :
                              loop
                                 Read_Next (Reader);

                                 case Event_Kind (Reader) is
                                    when Key_Name =>
                                       if VS2S (Key_Name (Reader)) =
                                         "LastSeenDisplayName"
                                       then
                                          Read_Next (Reader);
                                          GUIC.Last_Seen_Display_Name :=
                                            VS2UB (String_Value (Reader));

                                       elsif VS2S (Key_Name (Reader)) =
                                         "membershipType"
                                       then
                                          Read_Next (Reader);
                                          GUIC.Membership_Type :=
                                            Bungie_Platform_Type'Enum_Val
                                              (As_Integer
                                                 (Number_Value (Reader)));

                                       elsif VS2S (Key_Name (Reader)) =
                                         "membershipId"
                                       then
                                          Read_Next (Reader);
                                          GUIC.Membership_ID :=
                                            VS2UB (String_Value (Reader));
                                       end if;

                                    when End_Object =>
                                       exit Read_GUIC;

                                    when others =>
                                       null;
                                 end case;
                              end loop Read_GUIC;
                              Result.Destiny_Memberships.Append (GUIC);
                           end Submit_GUIC;

                        when End_Array =>
                           exit Read_Destiny_Memberships;

                        when others =>
                           null;
                     end case;
                  end loop Read_Destiny_Memberships;

               when others =>
                  raise Program_Error;
            end case;

         when others =>
            raise Program_Error;
      end case;
      --  Primary_Membership_ID
      Read_Next (Reader);

      case Event_Kind (Reader) is
         when Key_Name =>
            if VS2S (Key_Name (Reader)) /= "primaryMembershipId" then
               raise Program_Error;
            end if;
            Read_Next (Reader);

            case Event_Kind (Reader) is
               when String_Value =>
                  Result.Primary_Membership_ID :=
                    VS2UB (String_Value (Reader));

               when Null_Value =>
                  null;

               when others =>
                  raise Program_Error;
            end case;

         when others =>
            raise Program_Error;
      end case;
      --  Bungie_Net_User
      Read_Next (Reader);

      case Event_Kind (Reader) is
         when Key_Name =>
            if VS2S (Key_Name (Reader)) /= "bungieNetUser" then
               raise Program_Error;
            end if;
            Read_Next (Reader);

            case Event_Kind (Reader) is
               when Start_Object => -- Bungie_Net_User
                  Read_Bungie_Net_User :
                  loop
                     Read_Next (Reader);

                     case Event_Kind (Reader) is
                        when Key_Name =>
                           if VS2S (Key_Name (Reader)) = "membershipId" then
                              Read_Next (Reader);
                              Result.Bungie_Net_User.Membership_ID :=
                                VS2UB (String_Value (Reader));

                           elsif VS2S (Key_Name (Reader)) = "uniqueName" then
                              Read_Next (Reader);
                              Result.Bungie_Net_User.Unique_Name :=
                                VS2UB (String_Value (Reader));

                           elsif VS2S (Key_Name (Reader)) = "displayName" then
                              Read_Next (Reader);
                              Result.Bungie_Net_User.Display_Name :=
                                VS2UB (String_Value (Reader));

                           elsif VS2S (Key_Name (Reader)) = "locale" then
                              Read_Next (Reader);
                              Result.Bungie_Net_User.Locale :=
                                VS2UB (String_Value (Reader));

                           elsif VS2S (Key_Name (Reader)) = "context"
                           then -- This contains nested objects
                              Read_Next (Reader); -- Start_Object

                              declare

                                 Nesting_Level : Natural := 1;

                              begin
                                 while Nesting_Level /= 0 loop
                                    Read_Next (Reader);

                                    case Event_Kind (Reader) is
                                       when Start_Object =>
                                          Nesting_Level := @ + 1;

                                       when End_Object =>
                                          Nesting_Level := @ - 1;

                                       when others =>
                                          null;
                                    end case;
                                 end loop;
                              end;
                           end if;

                        when End_Object =>
                           exit Read_Bungie_Net_User;

                        when others =>
                           null;
                     end case;
                  end loop Read_Bungie_Net_User;

               when others =>
                  raise Program_Error;
            end case;

         when others =>
            raise Program_Error;
      end case;
      Free (Stream);
      return Result;
   end Get_Memberships;

end API.Memberships;
