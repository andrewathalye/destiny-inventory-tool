with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Unchecked_Deallocation;

-- Gtkada
with Gtk.Widget; use Gtk.Widget;

package Tasks.Download is
	-- Types
	type Stream_Element_Array_Access is access Stream_Element_Array;
	procedure Free is new Unchecked_Deallocation (Stream_Element_Array, Stream_Element_Array_Access);

	-- Returned to the calling task
	-- to determine relevant object
	type Download_Data_Type is record
		Path : Unbounded_String;
		Widget : Gtk_Widget;
		Data : Stream_Element_Array_Access;
	end record;

	-- TODO: Download data must be freed by the _caller_
	-- Make Download_Data_Type a controlled type to avoid this
	task type Download_Task is
		entry Download (
			Path : Unbounded_String;
			Widget : Gtk_Widget;
			Needs_Auth : Boolean := False);

		entry Execute;
		entry Clear;

		entry Complete (Data : out Download_Data_Type);
	end Download_Task;

	Contents_Task : Download_Task;
	Character_Task : Download_Task;
	Global_Task : Download_Task;

	-- Synchronous version of the above task. Provided for convenience.
	function Download (
		Path : Unbounded_String;
		Needs_Auth : Boolean := False) return Stream_Element_Array;
end Tasks.Download;
