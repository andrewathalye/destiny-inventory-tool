pragma Ada_2022;

with System;

-- Gtkada
with Gtk.Image; use Gtk.Image;
with Gtk.Main;

with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib.Error; use Glib.Error;
use Glib;

-- Local Packages
with Shared.Strings; use Shared.Strings; -- Only for "+"

package body GUI is
   -- Protected Object
   protected body Lock_Object is
      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      procedure Unlock is
      begin
         if not Locked then
            raise Program_Error with "GUI.Lock_Object.Unlock: Double-unlock";
         end if;

         Locked := False;
      end Unlock;
   end Lock_Object;

   -- Private Subprograms
   -- Private-Exported
   -- Exclusively for JPEG / PNG format images
   function Load_Image
     (File_Name : String; Data : Stream_Element_Array) return Gdk_Pixbuf
   is
      -- Imported Subprograms
      type Pixbuf_Loader is null record;

      function Gdk_Pixbuf_Loader_New_With_Type
        (Image_Type : String;
         Error      : access GError)
         return access Pixbuf_Loader with
        Import => True, Convention => C;

      function Gdk_Pixbuf_Loader_Write
        (Loader : access Pixbuf_Loader;
         Buffer : System.Address;
         Count  : Gsize;
         Error  : access GError)
         return Gboolean with
        Import => True, Convention => C;

      function Gdk_Pixbuf_Loader_Get_Pixbuf
        (Loader : access Pixbuf_Loader) return System.Address with
        Import => True, Convention => C;

      function Gdk_Pixbuf_Loader_Close
        (Loader : access Pixbuf_Loader;
         Error  : access GError)
         return Gboolean with
        Import => True, Convention => C;

      -- Constants
      FType : constant String :=
        (if File_Name (File_Name'Last - 2 .. File_Name'Last) = "png" then "png"
         else "jpeg");

      -- Variables
      Loader  : access Pixbuf_Loader;
      Pixbuf  : System.Address;
      Discard : Gboolean;
   begin
      Loader := Gdk_Pixbuf_Loader_New_With_Type (FType & ASCII.NUL, null);

      Discard :=
        Gdk_Pixbuf_Loader_Write
          (Loader, Data (Data'First)'Address, Data'Length, null);

      Pixbuf := Gdk_Pixbuf_Loader_Get_Pixbuf (Loader);

      if Gdk_Pixbuf'(Convert (Pixbuf)) = null then
         raise Program_Error with "Image could not be processed";
      end if;

      Discard := Gdk_Pixbuf_Loader_Close (Loader, null);

      return Convert (Pixbuf);
   end Load_Image;

   -- Called by Download_Tasks as they finish downloads
   -- The lock must be acquired first to avoid data races
   -- (Lock set before each Main_Loop iteration)

   procedure Image_Callback
     (File_Name : Unbounded_String;
      Widget    : Gtk_Widget;
      Data      : Stream_Element_Array)
   is
      Temp : constant Gdk_Pixbuf := Load_Image (+File_Name, Data);
   begin
      GUI.Lock_Object.Lock;
      Critical_Section :
      begin
         if Global_Pixbuf_Cache.Contains (File_Name) then
            Gtk_Image (Widget).Set (Temp);
            Lock_Object.Unlock;

            return;
         end if;

         -- Cache Pixbuf
         Global_Pixbuf_Cache.Insert (File_Name, Temp);

         Gtk_Image (Widget).Set (Temp);
      end Critical_Section;
      GUI.Lock_Object.Unlock;
   end Image_Callback;

   -- Public Subprograms
   procedure Locked_Wrapper (Unsafe_Subprogram : Unsafe_Subprogram_Type) is
   begin
      GUI.Lock_Object.Unlock;
      begin
         Unsafe_Subprogram.all;
      end;
      GUI.Lock_Object.Lock;
   end Locked_Wrapper;

   procedure Locking_Main_Iteration is
      Discard : Boolean;
   begin
      GUI.Lock_Object.Lock;
      Critical_Section :
      begin
         Discard := Gtk.Main.Main_Iteration;
      end Critical_Section;
      GUI.Lock_Object.Unlock;
   end Locking_Main_Iteration;
end GUI;
