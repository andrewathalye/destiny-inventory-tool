pragma Ada_2022;
with System;

--  Gtkada
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib.Error; use Glib.Error;
use Glib;

package body GUI is
   --  Private Subprograms
   --  Private-Exported
   --  Exclusively for JPEG / PNG format images

   function Load_Image
     (File_Name : String; Data : Stream_Element_Array) return Gdk_Pixbuf
   is
      --  Imported Subprograms
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

      --  Constants
      PNG   : constant String := "png" & ASCII.NUL;
      JPEG  : constant String := "jpeg" & ASCII.NUL;
      FType : constant String :=
        (if File_Name (File_Name'Last - 2 .. File_Name'Last) = "png" then PNG
         else JPEG);

      --  Variables
      Loader  : access Pixbuf_Loader;
      Pixbuf  : System.Address;
      Discard : Gboolean;

   begin
      Loader  := Gdk_Pixbuf_Loader_New_With_Type (FType, null);
      Discard :=
        Gdk_Pixbuf_Loader_Write
          (Loader, Data (Data'First)'Address, Data'Length, null);
      Pixbuf := Gdk_Pixbuf_Loader_Get_Pixbuf (Loader);

      if Gdk_Pixbuf'(Convert (Pixbuf)) = null then
         raise Program_Error with "Image could not be processed: " & File_Name;
      end if;

      Discard := Gdk_Pixbuf_Loader_Close (Loader, null);

      return Convert (Pixbuf);
   end Load_Image;
end GUI;
