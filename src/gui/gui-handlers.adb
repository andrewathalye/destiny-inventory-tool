pragma Ada_2022;

with Ada.Exceptions; use Ada.Exceptions;
with GNAT.OS_Lib;    use GNAT.OS_Lib;

--  Gtk
with Gtk.Search_Entry;   use Gtk.Search_Entry;
with Gtk.Popover;        use Gtk.Popover;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Label;          use Gtk.Label;
with Gtk.Box;            use Gtk.Box;

--  Local Packages
with GUI.Character;
with GUI.Global;
with GUI.Base;
with GUI.Items;
with GUI.Tasks; use GUI.Tasks;

with GUI.Elements.Handlers; use GUI.Elements.Handlers;

with API.Transfers;
with API.Manifest.Tools;
use all type API.Manifest.Tools.Bucket_Location_Type;

with API.Profiles;
use type API.Profiles.Character_Type;
use all type API.Profiles.Transfer_Status_Type;

with API.Definitions.Destiny_Inventory_Item;
use all type API.Definitions.Destiny_Inventory_Item.Destiny_Tier_Type;
use all type API.Definitions.Destiny_Inventory_Item.Destiny_Item_Type;

with API.Definitions.Destiny_Inventory_Bucket;
use all type API.Definitions.Destiny_Inventory_Bucket.Item_Location_Type;
use all type
  API.Definitions.Destiny_Inventory_Bucket.Destiny_Inventory_Bucket_Category;

with API.Inventories.Character;
with API.Inventories.Global; use API;

with Shared.Strings; use Shared.Strings;
with Shared.Debug;   use Shared;

package body GUI.Handlers is
   --  Global Handlers (Private)

   pragma Warnings (Off, "is not referenced");

   --  Exit after window closed
   procedure Window_Close_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is
   begin
      OS_Exit (0);
   end Window_Close_Handler;

   --  Opens the character switching menu
   procedure Emblem_Button_Clicked_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is
   begin
      Character_Menu.Popup;
   end Emblem_Button_Clicked_Handler;

   --  Update the search query when changed and re-render
   procedure Search_Changed_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is
   begin
      Items.Search_Query := +Search.Get_Chars (0);
      GUI.Global.Render;
      GUI.Character.Render;
   end Search_Changed_Handler;

   --  Dismiss an error dialogue
   procedure Error_Dialog_Close_Button_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is
   begin
      Error_Dialog.Hide;
   end Error_Dialog_Close_Button_Handler;

   --  Reload all profile data
   procedure Reload_Button_Clicked_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is
   begin
      GUI.Base.Reload_Profile_Data;
   end Reload_Button_Clicked_Handler;

   --  A button exclusively for equipping items which
   --  cannot be transferred
   procedure Equip_Button_Clicked_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is
   begin
      Debug.Put_Line ("Equip a non-transferrable item");

      Transfers.Equip
        (Auth => Identification,
         Inventory =>
           GUI.Character.Inventory (GUI.Character.Current_Character_Index),
         D      => GUI.Current_Item,
         Source => GUI.Character.Current_Character);

      --  Update UI State
      GUI.Character.Inventory (GUI.Character.Current_Character_Index).Remove
        (GUI.Current_Item);
      GUI.Character.Inventory (GUI.Character.Current_Character_Index).Equip
        (GUI.Current_Item);

      --  Simulate Emblem Change if Necessary
      --  Later on this may be used to generically update stats on equipment change
      --  TODO
      if GUI.Current_Item.Item_Type = Emblem then
         GUI.Profile.Characters (GUI.Character.Current_Character_Index)
           .Emblem_Hash :=
           GUI.Current_Item.Item_Hash;

         GUI.Global.Update_GUI;
         GUI.Character.Update_For_Character
           (GUI.Character.Current_Character_Index);

         GUI.Tasks.Global_Task.Execute (Base.Event_Image_Callback'Access);
         --  This is an event handler, so it isn’t possible to pause the GUI task
         --  from here (it would hang indefinitely). Instead use the thread-unsafe
         --  variant because we are in the thread that would be impacted anyway :)
      end if;

      GUI.Character.Render; --  Global Side (Vault) not changed by this
   end Equip_Button_Clicked_Handler;

   --  Install Handlers
   procedure Set_Handlers (Builder : Gtkada_Builder) is
   begin
      Builder.Register_Handler
        ("window_close_handler", Window_Close_Handler'Access);

      Builder.Register_Handler
        ("emblem_button_clicked_handler",
         Emblem_Button_Clicked_Handler'Access);

      Builder.Register_Handler
        ("search_changed_handler", Search_Changed_Handler'Access);

      Builder.Register_Handler
        ("error_dialog_close_button_handler",
         Error_Dialog_Close_Button_Handler'Access);

      Builder.Register_Handler
        ("reload_button_clicked_handler",
         Reload_Button_Clicked_Handler'Access);

      --  Widget_Callback.Connect
      --    (Vault_Button, "clicked",
      --     Widget_Callback.To_Marshaller (Vault_Handler'Access));

      Builder.Register_Handler
        ("equip_button_clicked_handler", Equip_Button_Clicked_Handler'Access);
   end Set_Handlers;

   --  Dynamic Handlers (Public)
   pragma Warnings (Off, "is not referenced");

   procedure Character_Menu_Button_Clicked_Handler
     (Button    : access Gtk_Widget_Record'Class;
      User_Data : API.Profiles.Character_Range)
   is
   begin
      Character_Menu.Popdown;
      Character.Update_For_Character (User_Data);
      GUI.Character.Render;
   end Character_Menu_Button_Clicked_Handler;

   --  The two handlers below are additionally responsible for simulating the
   --  transfer clientside

   procedure Transfer_Handler
     (Button : access Gtk_Widget_Record'Class;
      Target : Profiles.Character_Type)
   is
      --  Renames
      Target_Inventory :
        API.Inventories.Character.Character_Inventory_Type renames
        GUI.Character.Inventory (GUI.Profile.Characters.Find_Index (Target));
   begin
      Debug.Put_Line ("Item Transfer");
      Debug.Put_Line
        ("Source: " &
         Manifest.Tools.Get_Description
           (The_Manifest, GUI.Character.Current_Character));
      Debug.Put_Line
        ("Target: " & Manifest.Tools.Get_Description (The_Manifest, Target));

      --  Unvault
      if GUI.Current_Item.Location = Vault then
         Debug.Put_Line ("Method: Unvault");

         begin
            Transfers.Unvault
              (Auth => Identification,
            Character_Inventory => GUI.Character.Inventory (GUI.Character.Current_Character_Index),
               M => GUI.The_Manifest,
               D => GUI.Current_Item,
               Target => Target);
         exception
            when Transfers.No_Room_In_Destination =>
               GUI.Base.Error_Message
                 ("Item Transfer Failed", "No Room in Destination");
               return;

            when Transfers.Item_Already_Here =>
               Debug.Put_Line
                 ("Couldn't unvault is already where it would have been sent");
               return;
         end;

         --  Update UI State
         GUI.Global.Inventory.Remove (GUI.Current_Item);
         Target_Inventory.Add (GUI.Current_Item);
         GUI.Global.Render;
         return;
      end if;

      --  Retrieve from Postmaster
      --  Note: Can't check Location because Postmaster isn't always returned
      --  for that

      if GUI.Current_Item.Bucket_Location = Postmaster then
         Debug.Put_Line ("Method: Postmaster_Pull");

         begin
            Transfers.Postmaster_Pull
              (Auth => Identification,
            Vault_Inventory => GUI.Global.Inventory,
               Character_Inventory => GUI.Character.Inventory (GUI.Character.Current_Character_Index),
               M => GUI.The_Manifest,
               D => GUI.Current_Item,
               Source => GUI.Character.Current_Character);
         exception
            when Transfers.No_Room_In_Destination =>
               GUI.Base.Error_Message
                 ("Item Transfer Failed", "No Room in Destination");
               return;
         end;

         --  Transfer to correct character
         if GUI.Character.Current_Character /= Target then
            begin
               Transfers.Transfer
                 (Auth => Identification,
                  Vault_Inventory => GUI.Global.Inventory,
                  Target_Inventory => Target_Inventory,
                  M => GUI.The_Manifest,
                  D => GUI.Current_Item,
                  Source => GUI.Character.Current_Character,
                  Target => Target);
            exception -- The previous action cannot be undone, so keep going
               when Transfers.No_Room_In_Destination =>
                  Debug.Put_Line
                    ("Failed to finish transfer, but postmaster pull can't be rolled back! Reloading for consistency.");
                  GUI.Base.Reload_Profile_Data;
                  GUI.Base.Error_Message
                    ("Item Transfer Failed: Profile Reloaded",
                     "No Room in Destination");
                  return;
            end;
         end if;

         --  Update UI State
         GUI.Character.Inventory (GUI.Character.Current_Character_Index).Remove
           (GUI.Current_Item);
         Target_Inventory.Add (GUI.Current_Item);

         GUI.Character.Render;
         return;
      end if;

      --  Equip
      if GUI.Character.Current_Character = Target and
        Current_Item.Category = Equippable
      then
         Debug.Put_Line ("Method: Equip");

         begin
            Transfers.Equip
              (Auth => Identification,
          Inventory => GUI.Character.Inventory (GUI.Character.Current_Character_Index),
               D => GUI.Current_Item,
               Source => Target);
         exception
            when Transfers.Item_Unique_Equip_Restricted =>
               Base.Error_Message
                 ("Item Equip Failed",
                  "You may only equip one exotic at a time.");
               return;
         end;

         --  Update UI State
         Target_Inventory.Remove (GUI.Current_Item);
         Target_Inventory.Equip (GUI.Current_Item);

         GUI.Character.Render;
         return;
      end if;

      --  Normal Transfer
      Debug.Put_Line ("Method: Transfer");

      begin
         Transfers.Transfer
           (Identification,
         GUI.Global.Inventory,
            Target_Inventory,
            GUI.The_Manifest,
            GUI.Current_Item,
            GUI.Character.Current_Character,
            Target);
      exception
         when Transfers.No_Room_In_Destination =>
            GUI.Base.Error_Message
              ("Item Transfer Failed", "No Room in Destination");
            return;
      end;
      --  Update UI State
      GUI.Character.Inventory (GUI.Character.Current_Character_Index).Remove
        (GUI.Current_Item);
      Target_Inventory.Add (GUI.Current_Item);

      GUI.Character.Render_Contents (GUI.Current_Item.Bucket_Location);
   exception
      when C : others =>
         Debug.Put_Line (Exception_Information (C));
         GUI.Base.Reload_Profile_Data;
         GUI.Base.Error_Message
           ("Item Transfer Failed: Profile Reloaded",
            Exception_Name (C) & ": " & Exception_Message (C));
   end Transfer_Handler;

   --  Vault an Item
   procedure Vault_Handler (Button : access Gtk_Widget_Record'Class) is
   begin
      Debug.Put_Line ("Vault Item");

      if GUI.Current_Item.Bucket_Location = Postmaster then
         Debug.Put_Line ("Item was in Postmaster: pulling first");
         begin
            Transfers.Postmaster_Pull
              (Identification,
             GUI.Global.Inventory,
               GUI.Character.Inventory (GUI.Character.Current_Character_Index),
               GUI.The_Manifest,
               GUI.Current_Item,
               GUI.Character.Current_Character);
         exception
            when Transfers.No_Room_In_Destination =>
               GUI.Base.Error_Message
                 ("Item Transfer Failed", "No Room in Destination");
               return;
         end;
      end if;

      begin
         Transfers.Vault
           (Identification,
            GUI.Global.Inventory,
            GUI.The_Manifest,
            GUI.Current_Item,
            GUI.Character.Current_Character);
      exception
         when Transfers.No_Room_In_Destination =>
            GUI.Base.Error_Message
              ("Item Transfer Failed", "No Room in Destination");
            return;

         when Transfers.Item_Already_Here =>
            Debug.Put_Line ("The item was already there, aborting attempt");
            return;
      end;

      --  Update inventory state
      GUI.Character.Inventory (GUI.Character.Current_Character_Index).Remove
        (GUI.Current_Item);

      GUI.Global.Inventory.Add (GUI.Current_Item);
      GUI.Global.Render;

      --  Redraw as little as possible for performance :)
      if GUI.Current_Item.Bucket_Location = Postmaster then
         GUI.Character.Render;
      else
         GUI.Character.Render_Contents (GUI.Current_Item.Bucket_Location);
         --  A smaller render that will be faster (hopefully)
      end if;
   exception
      when E : others =>
         Debug.Put_Line (Exception_Information (E));
         GUI.Base.Reload_Profile_Data;
         GUI.Base.Error_Message
           ("Item Transfer Failed: Profile Reloaded",
            Exception_Name (E) & ": " & Exception_Message (E));
   end Vault_Handler;
   pragma Warnings (On, "is not referenced");

   --  Display Transfer Menu When Item is Clicked
   procedure Item_Button_Handler
     (Widget    : access Gtk_Widget_Record'Class;
      User_Data : Manifest.Tools.Item_Description)
   is
   begin
      Current_Item := User_Data;

      --  Display item details
      Items.Populate_Item_Details (Current_Item);
      Item_Details.Set_Relative_To (Widget);
      Item_Details.Popup;

      --  Don't show the normal transfer menu for nontransferrables
      --  TODO handle some edge cases better
      if User_Data.Transfer_Status /= Can_Transfer then
         --  Subclasses and Emblems can be Equipped, but not Transferred
         case User_Data.Item_Type is
            when Subclass | Emblem =>
               Equip_Menu.Set_Relative_To (Item_Details);
               Equip_Menu.Popup;
               return;
            when others =>
               null;
         end case;

         --  It is often possible to transfer items in the postmaster to the
         --  Vault
         if User_Data.Bucket_Location = Postmaster and
           not User_Data.Postmaster_Pull_Has_Side_Effects
         then
            Vault_Menu.Set_Relative_To (Item_Details);
            Vault_Menu.Popup;
            return;
         end if;

         --  For other items, we can’t perform any actions
         return;
      end if;

      Transfer_Menu.Set_Relative_To (Item_Details);
      Transfer_Menu.Popup;
   end Item_Button_Handler;

   procedure Socket_Item_Button_Handler
     (Widget    : access Gtk_Widget_Record'Class;
      User_Data : Manifest.Tools.Item_Description)
   is
   begin
      Socket_Name.Set_Label (+User_Data.Name);

      Socket_Type_And_Tier_Display_Name.Set_Label
        (+User_Data.Item_Type_And_Tier_Display_Name);
      Socket_Type_And_Tier_Display_Name.Show;

      --  Set colour by rarity (using CSS)
      Socket_Name_Type_Box.Set_Name
        ((case User_Data.Tier_Type is
            when Unknown | Basic | Currency => "socket_name_type_box_basic",
            when Common   => "socket_name_type_box_common",
            when Rare     => "socket_name_type_box_rare",
            when Superior => "socket_name_type_box_superior",
            when Exotic   => "socket_name_type_box_exotic"));

      Socket_Description.Set_Label (+User_Data.Description);

      Socket_Popover.Set_Relative_To (Widget);
      Socket_Popover.Popup;
   end Socket_Item_Button_Handler;
end GUI.Handlers;
