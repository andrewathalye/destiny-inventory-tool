--  Local Packages
with API.Manifest.Tools;
with API.Profiles;
with API.Inventories.Global;
with API.Inventories.Character;

package API.Transfers is
   --  Exceptions
   --  Only raised for local checks
   No_Room_In_Destination       : exception; --  DestinyNoRoomInDestination
   Item_Already_Here            : exception;
   Item_Not_Transferrable       : exception; --  DestinyItemNotTransferrable
   Item_Action_Forbidden        : exception; --  DestinyItemActionForbidden
   Item_Not_Found               : exception; --  DestinyItemNotFound
   Item_Unique_Equip_Restricted : exception; --  DestinyItemUniqueEquipRestricted

   --  Only raised for remote checks
   System_Disabled                        : exception; --  SystemDisabled
   Cannot_Perform_Action_At_This_Location : exception; --  DestinyCannotPerformActionAtThisLocation
   Desynchronised : exception; --  Any of the above exceptions when not caught locally
   Unknown_Error : exception; --  Any unknown exceptions

   --  Note: The below subprograms perform local checks for consistency and also
   --  interpret server responses.
   --
   --  If a local check passed but a serverside check failed, then an exception
   --  will be raised and the state will be synchronised (e.g. Profile
   --  redownloaded, inventories cleared, etc.)

   procedure Vault
     (Vault_Inventory : Inventories.Global.Global_Inventory_Type;
      M               : Manifest.Manifest_Type;
      D               : Manifest.Tools.Item_Description;
      Source          : Profiles.Character_Type);

   procedure Unvault
     (Character_Inventory : Inventories.Character.Character_Inventory_Type;
      M                   : Manifest.Manifest_Type;
      D                   : Manifest.Tools.Item_Description;
      Target              : Profiles.Character_Type);

   procedure Postmaster_Pull
     (Vault_Inventory     : Inventories.Global.Global_Inventory_Type;
      Character_Inventory : Inventories.Character.Character_Inventory_Type;
      M                   : Manifest.Manifest_Type;
      D                   : Manifest.Tools.Item_Description;
      Source              : Profiles.Character_Type);

   procedure Equip
     (Inventory : Inventories.Character.Character_Inventory_Type;
      D         : Manifest.Tools.Item_Description;
      Source    : Profiles.Character_Type);

   --  Provided for convenience
   procedure Transfer
     (Vault_Inventory  : Inventories.Global.Global_Inventory_Type;
      Target_Inventory : Inventories.Character.Character_Inventory_Type;
      M                : Manifest.Manifest_Type;
      D                : Manifest.Tools.Item_Description;
      Source, Target   : Profiles.Character_Type);

end API.Transfers;
