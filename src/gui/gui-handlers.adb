pragma Ada_2022;
with GNAT.OS_Lib; use GNAT.OS_Lib;
--  Gtk
with Gtk.Search_Entry;   use Gtk.Search_Entry;
with Gtk.Popover;        use Gtk.Popover;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;

--  Local Packages
with GUI.Character;
with GUI.Global;
with GUI.Base;
with API.Transfers;
with API.Manifest.Tools;
use all type API.Manifest.Tools.Bucket_Location_Type;
with API.Profiles;
use type API.Profiles.Character_Type;
use all type API.Profiles.Transfer_Status_Type;
with API.Manifest;
use all type API.Manifest.Item_Location_Type;
use all type API.Manifest.Destiny_Inventory_Bucket_Category;
with API.Inventories.Character;
with API.Inventories.Global; use API;
with Shared.Strings;         use Shared.Strings; -- For "+"
with Shared.Debug;           use Shared;

package body GUI.Handlers is
   --  Global Handlers (Private)

   pragma Warnings (Off, "is not referenced");

   procedure Window_Close_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is
   begin
      OS_Exit (0);
   end Window_Close_Handler;

   procedure Emblem_Button_Clicked_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is

      Character_Menu : constant Gtk_Popover :=
        Gtk_Popover (GUI.Builder.Get_Object ("character_menu"));

   begin
      Character_Menu.Popup;
   end Emblem_Button_Clicked_Handler;

   procedure Search_Changed_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is

      Search : constant Gtk_Search_Entry :=
        Gtk_Search_Entry (GUI.Builder.Get_Object ("search"));

   begin
      Base.Search_Query := +Search.Get_Chars (0);
      GUI.Locked_Wrapper (GUI.Global.Render'Access);
      GUI.Locked_Wrapper (GUI.Character.Render'Access);
   end Search_Changed_Handler;

   procedure Error_Dialog_Close_Button_Handler
     (Builder : access Gtkada_Builder_Record'Class)
   is

      Error_Dialog : constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog (Builder.Get_Object ("error_dialog"));

   begin
      Error_Dialog.Hide;
   end Error_Dialog_Close_Button_Handler;

   procedure Postmaster_Vault_Handler (Button : access Gtk_Widget_Record'Class)
   is
   begin
      Debug.Put_Line ("Postmaster Vault Item");

      begin
         Transfers.Postmaster_Pull
           (GUI.Global.Inventory,
            GUI.Character.Inventory,
            GUI.The_Manifest,
            GUI.Current_Item,
            GUI.Character.Current_Character);
      exception
         when Transfers.Out_Of_Space =>
            Debug.Put_Line ("Out of Vault space, aborting attempt");
            return;
      end;
      --  Update UI State
      Inventories.Character.Remove_Item
        (GUI.Character.Inventory,
         GUI.Character.Current_Character,
         GUI.Current_Item);
      Inventories.Global.Add_Item (GUI.Global.Inventory, GUI.Current_Item);
      GUI.Locked_Wrapper (GUI.Global.Render'Access);
      GUI.Locked_Wrapper (GUI.Character.Render'Access);
   end Postmaster_Vault_Handler;
   pragma Warnings (On, "is not referenced");

   --  Install Handlers
   procedure Set_Handlers is

      Vault_Button : constant Gtk_Widget :=
        Gtk_Widget (Builder.Get_Object ("vault_button"));

   begin
      Register_Handler
        (GUI.Builder, "window_close_handler", Window_Close_Handler'Access);
      Register_Handler
        (Builder,
         "emblem_button_clicked_handler",
         Emblem_Button_Clicked_Handler'Access);
      Register_Handler
        (Builder, "search_changed_handler", Search_Changed_Handler'Access);
      Register_Handler
        (Builder,
         "error_dialog_close_button_handler",
         Error_Dialog_Close_Button_Handler'Access);
      Widget_Callback.Connect
        (Vault_Button,
         "clicked",
         Widget_Callback.To_Marshaller (Postmaster_Vault_Handler'Access));
   end Set_Handlers;

   --  Dynamic Handlers (Public)
   pragma Warnings (Off, "is not referenced");

   procedure Character_Menu_Button_Clicked_Handler
     (Button : access Gtk_Widget_Record'Class; User_Data : Natural)
   is

      Character_Menu : constant Gtk_Popover :=
        Gtk_Popover (GUI.Builder.Get_Object ("character_menu"));

   begin
      Character_Menu.Popdown;
      Character.Update_For_Character (Profile.Characters (User_Data));
      GUI.Locked_Wrapper (Character.Render'Access);
   end Character_Menu_Button_Clicked_Handler;
   --  The two handlers below are additionally responsible for simulating the
   --  transfer clientside

   procedure Transfer_Handler
     (Button : access Gtk_Widget_Record'Class;
      Target : Profiles.Character_Type)
   is
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
              (GUI.Character.Inventory,
               GUI.The_Manifest,
               GUI.Current_Item,
               Target);
         exception
            when Transfers.Out_Of_Space =>
               Debug.Put_Line
                 ("Couldn't unvault because the destination is out of space");
               return;

            when Transfers.Already_Here =>
               Debug.Put_Line
                 ("Couldn't unvault is already where it would have been sent");
               return;
         end;
         --  Update UI State
         Inventories.Global.Remove_Item
           (GUI.Global.Inventory, GUI.Current_Item);
         Inventories.Character.Add_Item
           (GUI.Character.Inventory, Target, GUI.Current_Item);
         GUI.Locked_Wrapper (GUI.Global.Render'Access);
         return;
      end if;
      --  Retrieve from Postmaster
      --  Note: Can't check Location because Postmaster isn't always returned
      --  for that

      if GUI.Current_Item.Bucket_Location = Postmaster then
         Debug.Put_Line ("Method: Postmaster_Pull");

         begin
            Transfers.Postmaster_Pull
              (GUI.Global.Inventory,
               GUI.Character.Inventory,
               GUI.The_Manifest,
               GUI.Current_Item,
               GUI.Character.Current_Character);
         exception
            when Transfers.Out_Of_Space =>
               Debug.Put_Line ("Failed to pull from postmaster: out of space");
               return;
         end;
         --  Transfer to correct character

         if GUI.Character.Current_Character /= Target then
            begin
               Transfers.Transfer
                 (GUI.Global.Inventory,
                  GUI.Character.Inventory,
                  GUI.The_Manifest,
                  GUI.Current_Item,
                  GUI.Character.Current_Character,
                  Target);
            exception -- The previous action cannot be undone, so keep going
               when Transfers.Out_Of_Space =>
                  Debug.Put_Line
                    ("Failed to finish transfer, but postmaster pull can't be rolled back!");
            end;
         end if;
         --  Update UI State
         Inventories.Character.Remove_Item
           (GUI.Character.Inventory,
            GUI.Character.Current_Character,
            GUI.Current_Item);
         Inventories.Character.Add_Item
           (GUI.Character.Inventory, Target, GUI.Current_Item);
         GUI.Locked_Wrapper (GUI.Character.Render'Access);
         return;
      end if;
      --  Equip

      if GUI.Character.Current_Character = Target and
        Current_Item.Category = Equippable
      then
         Debug.Put_Line ("Method: Equip");
         Transfers.Equip (GUI.Current_Item, Target);
         --  Update UI State
         Inventories.Character.Remove_Item
           (GUI.Character.Inventory, Target, GUI.Current_Item);
         Inventories.Character.Equip_Item
           (GUI.Character.Inventory, Target, GUI.Current_Item);
         GUI.Locked_Wrapper (GUI.Character.Render'Access);
         return;
      end if;
      --  Normal Transfer
      Debug.Put_Line ("Method: Transfer");

      begin
         Transfers.Transfer
           (GUI.Global.Inventory,
            GUI.Character.Inventory,
            GUI.The_Manifest,
            GUI.Current_Item,
            GUI.Character.Current_Character,
            Target);
      exception
         when Transfers.Out_Of_Space =>
            Debug.Put_Line
              ("Out of space somewhere along the chain, aborting transfer");
            return;
      end;
      --  Update UI State
      Inventories.Character.Remove_Item
        (GUI.Character.Inventory,
         GUI.Character.Current_Character,
         GUI.Current_Item);
      Inventories.Character.Add_Item
        (GUI.Character.Inventory, Target, GUI.Current_Item);
      GUI.Character.Locked_Render_Contents (GUI.Current_Item.Bucket_Location);
   end Transfer_Handler;
   --  Vault an Item

   procedure Vault_Handler (Button : access Gtk_Widget_Record'Class) is
   begin
      Debug.Put_Line ("Vault Item");

      begin
         Transfers.Vault
           (GUI.Global.Inventory,
            GUI.The_Manifest,
            GUI.Current_Item,
            GUI.Character.Current_Character);
      exception
         when Transfers.Out_Of_Space =>
            Debug.Put_Line ("Out of Vault space, aborting attempt");
            GUI.Base.Error_Message ("Item Transfer Failed", "Out of Space");
            return;

         when Transfers.Already_Here =>
            Debug.Put_Line ("The item was already there, aborting attempt");
            return;
      end;
      --  Update inventory state
      Inventories.Character.Remove_Item
        (GUI.Character.Inventory,
         GUI.Character.Current_Character,
         GUI.Current_Item);
      Inventories.Global.Add_Item (GUI.Global.Inventory, GUI.Current_Item);
      GUI.Locked_Wrapper (GUI.Global.Render'Access);
      --  Redraw as little as possible for performance :)

      if GUI.Current_Item.Location = Postmaster then
         GUI.Locked_Wrapper (GUI.Character.Render'Access);

      else
         GUI.Character.Locked_Render_Contents
           (GUI.Current_Item.Bucket_Location);
         --  A smaller render that will be faster (hopefully)
      end if;
   end Vault_Handler;
   pragma Warnings (On, "is not referenced");

   --  Display Transfer Menu When Item is Clicked
   procedure Item_Button_Handler
     (Widget    : access Gtk_Widget_Record'Class;
      User_Data : Manifest.Tools.Item_Description)
   is

      Item_Details : constant Gtk_Popover :=
        Gtk_Popover (Builder.Get_Object ("item_details"));
      Transfer_Menu : constant Gtk_Popover :=
        Gtk_Popover (Builder.Get_Object ("transfer_menu"));
      Vault_Menu : constant Gtk_Popover :=
        Gtk_Popover (Builder.Get_Object ("vault_menu"));

   begin
      Current_Item := User_Data;
      Debug.Put_Line (Current_Item'Image);

      --  Display item details
      Base.Populate_Item_Details (Current_Item);
      Item_Details.Set_Relative_To (Widget);
      Item_Details.Popup;

      pragma Warnings (Off, "unreachable code");
      return;

      --  Don't show the normal transfer menu for nontransferrables
      if User_Data.Transfer_Status /= Can_Transfer then
         --  It is often possible to transfer items in the postmaster to the
         --  Vault
         if User_Data.Bucket_Location = Postmaster and
           not User_Data.Postmaster_Pull_Has_Side_Effects
         then
            Vault_Menu.Set_Relative_To (Widget);
            Vault_Menu.Popup;
         end if;
         --  For other items, we can’t perform any actions
      else
         Transfer_Menu.Set_Relative_To (Widget);
         Transfer_Menu.Popup;
      end if;
   end Item_Button_Handler;

end GUI.Handlers;
