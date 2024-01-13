private with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
private with Ada.Streams;

--  Gtkada
private with Gdk.Pixbuf;

--  Local Packages
with API.Identification;
with API.Profiles;
with API.Manifest; use API;

private with API.Manifest.Tools;
private with Shared.Strings;

--  Note: Unless marked otherwise, nothing here or in child packages is thread-safe. Use only from GUI task.
package GUI is

   --  Before use, execute GUI.Base.Reload_Data Undefined behaviour will occur
   --  otherwise
   Identification : API.Identification.Auth_Type;
   Profile        : Profiles.Profile_Type;
   The_Manifest   : Manifest.Manifest_Type;
private
   use Ada.Streams;
   use Gdk.Pixbuf;
   use API.Manifest.Tools;

   --  Subprograms (Other Utilities in GUI.Base) Image Loading
   function Load_Image
     (File_Name : String; Data : Stream_Element_Array) return Gdk_Pixbuf;

   --  Instantiations
   package Pixbuf_Hash_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Gdk_Pixbuf,
      Hash            => Shared.Strings.Hash,
      Equivalent_Keys => Shared.Strings.Equivalent_Keys);
   subtype Pixbuf_Hash_Map is Pixbuf_Hash_Maps.Map;

   --  Global State
   Global_Pixbuf_Cache : Pixbuf_Hash_Map;
   Current_Item        : Manifest.Tools.Item_Description;
end GUI;
