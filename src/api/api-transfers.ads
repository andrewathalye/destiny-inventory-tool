--  Local Packages
with API.Manifest.Tools;
with API.Profiles;
with API.Inventories.Global;
with API.Inventories.Character;

with API.Identification;

package API.Transfers is
   -----------
   -- LOCAL --
   -----------
   No_Room_In_Destination       : exception; --  DestinyNoRoomInDestination
   Item_Already_Here            : exception;
   Item_Not_Transferrable       : exception; --  DestinyItemNotTransferrable
   Item_Action_Forbidden        : exception; --  DestinyItemActionForbidden
   Item_Not_Found               : exception; --  DestinyItemNotFound
   Item_Unique_Equip_Restricted : exception; --  DestinyItemUniqueEquipRestricted

   -----------------
   -- REMOTE-ONLY --
   -----------------
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

   --------------------------
   -- SYNCHRONOUS TRANSFER --
   --------------------------

   ---------------
   -- CANONICAL --
   ---------------
   procedure Vault
     (Auth            : API.Identification.Auth_Type;
      Vault_Inventory : Inventories.Global.Global_Inventory_Type;
      M               : Manifest.Manifest_Type;
      D               : Manifest.Tools.Item_Description;
      Source          : Profiles.Character_Type);
   --  Vault a single item from character Source
   --  Checks to ensure there is sufficient space for the item in the Vault.

   procedure Unvault
     (Auth                : API.Identification.Auth_Type;
      Character_Inventory : Inventories.Character.Character_Inventory_Type;
      M                   : Manifest.Manifest_Type;
      D                   : Manifest.Tools.Item_Description;
      Target              : Profiles.Character_Type);
   --  Unvault a single item to character Target
   --  Checks to ensure there is sufficient room in the character’s inventory.

   procedure Postmaster_Pull
     (Auth                : API.Identification.Auth_Type;
      Vault_Inventory     : Inventories.Global.Global_Inventory_Type;
      Character_Inventory : Inventories.Character.Character_Inventory_Type;
      M                   : Manifest.Manifest_Type;
      D                   : Manifest.Tools.Item_Description;
      Source              : Profiles.Character_Type);
   --  Pulls an item from the Postmaster to character Source (technically was already there)
   --  Checks to ensure there is sufficient room ni the new bucket

   procedure Equip
     (Auth      : API.Identification.Auth_Type;
      Inventory : Inventories.Character.Character_Inventory_Type;
      D         : Manifest.Tools.Item_Description;
      Source    : Profiles.Character_Type);
   --  Equips a single item from character Source -> Source
   --  Basic equippability checks performed.

   -------------------
   -- META-FUNCTION --
   -------------------
   procedure Transfer
     (Auth             : API.Identification.Auth_Type;
      Vault_Inventory  : Inventories.Global.Global_Inventory_Type;
      Target_Inventory : Inventories.Character.Character_Inventory_Type;
      M                : Manifest.Manifest_Type;
      D                : Manifest.Tools.Item_Description;
      Source, Target   : Profiles.Character_Type);
   --  Meta-function to transfer an item from Source -> Target
   --  Performs checks to ensure the item is not equipped and the Target has room.

end API.Transfers;
