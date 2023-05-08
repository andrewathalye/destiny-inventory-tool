with Gtk.Overlay; use Gtk.Overlay;

--  Returns an Overlay for an item description, using the specified
--  download queue and on-click handler
function GUI.Base.Get_Overlay
  (D       : Manifest.Tools.Item_Description;
   T       : Tasks.Download.Download_Task;
   Handler : User_Callback_Item_Description.Marshallers.Marshaller)
   return Gtk_Overlay;
