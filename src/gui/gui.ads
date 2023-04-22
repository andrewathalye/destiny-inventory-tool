private with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- private with Ada.Streams;
with Ada.Streams; use Ada.Streams;

-- Gtkada
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget;     use Gtk.Widget;

private with Gdk.Pixbuf;

-- Local Packages
with API.Profiles;
with API.Manifest;
private with API.Manifest.Tools;
with API.Memberships; use API;

private with Shared.Strings;

package GUI is
   -- Note
   -- A global lock is held on the GUI when GTK events are being processed and
   -- image data is being updated because Tasks.Download.Download_Type performs
   -- asynchronous updates from a different thread.

   -- Variables
   Builder :
     Gtkada_Builder; -- Left uninitialised, should be setup along with Gtk_Main

   -- Before use, execute GUI.Base.Reload_Data
   -- Undefined behaviour will occur otherwise
   Auth_Data : Auth_Storage_Type;
   Headers   : Auth_Header_Type;

   Membership   : Memberships.Membership_Type;
   Profile      : Profiles.Profile_Type;
   The_Manifest : Manifest.Manifest_Type;

   -- Types
   type Unsafe_Subprogram_Type is access procedure;

   -- Subprograms
   -- A simple wrapper to call subprograms which expect the GUI to be unlocked
   -- Unlocks the GUI, calls the subprogram, and then locks the GUI again
   -- It is unsafe to call subprograms which themselves rely on the GUI's state from
   -- within an Unsafe_Subprogram_Type
   procedure Locked_Wrapper (Unsafe_Subprogram : Unsafe_Subprogram_Type) with
     Inline;

   -- A simple wrapper which perform an iteration of the Main Loop, locking the GUI first
   -- and then unlocking it afterwrads
   procedure Locking_Main_Iteration with
     Inline;
private
   use Ada.Streams;

   use Gdk.Pixbuf;

   use API.Manifest.Tools;

   -- Subprograms (Other Utilities in GUI.Base)
   -- Image Loading
   function Load_Image
     (File_Name : String; Data : Stream_Element_Array) return Gdk_Pixbuf;

   -- Note: To be used with Tasks.Download.Download_Task
   procedure Image_Callback
     (File_Name : Unbounded_String;
      Widget    : Gtk_Widget;
      Data      : Stream_Element_Array);

   -- Instantiations
   package Pixbuf_Hash_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Gdk_Pixbuf,
      Hash            => Shared.Strings.Hash,
      Equivalent_Keys => Shared.Strings.Equivalent_Keys);
   subtype Pixbuf_Hash_Map is Pixbuf_Hash_Maps.Map;

   -- Global State
   Global_Pixbuf_Cache : Pixbuf_Hash_Map;
   Current_Item        : Manifest.Tools.Item_Description;

   protected Lock_Object is
      entry Lock;
      procedure Unlock;
   private
      Locked : Boolean := False;
   end Lock_Object;
end GUI;
