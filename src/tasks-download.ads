with Ada.Streams; use Ada.Streams;
with Ada.Finalization;
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
	type Download_Data_Type is new Ada.Finalization.Limited_Controlled with private;
	function Path (Self : Download_Data_Type) return Unbounded_String with Inline;
	function Widget (Self : Download_Data_Type) return Gtk_Widget with Inline;
	function Data (Self : Download_Data_Type) return Stream_Element_Array with Inline;

	-- Internal procedures
--	procedure Initialize (Object : in out Download_Data_Type) is null;
	procedure Finalize (Object : in out Download_Data_Type);

	-- Task
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
private
	type Download_Data_Internal is record
		Path : Unbounded_String;
		Widget : Gtk_Widget;
		Data : Stream_Element_Array_Access;
	end record;

	type Download_Data_Type is new Ada.Finalization.Limited_Controlled with record
		Internal : Download_Data_Internal;
	end record;
end Tasks.Download;
