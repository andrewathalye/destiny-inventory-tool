pragma Ada_2022;

with Ada.Containers.Vectors;
use Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

-- AWS
with AWS.Client;
with AWS.Response;
use AWS;

-- Local Packages
with API; use API;
with Shared; use Shared;

package body Tasks.Download is
	-- Debugging
	Simulate_Slow : constant Boolean := False;

	-- Subprograms for Download_Data_Type
	procedure Finalize (Object : in out Download_Data_Type)
	is begin
		-- DDT could be created but never filled by a call
		-- to Download_Task.Complete, so don't assume
		-- that access values are valid

		if Object.Internal.Widget /= null then
			Object.Internal.Widget.Unref;
			Free (Object.Internal.Data);
		end if;
	exception
		when E : others =>
			Put_Line (Standard_Error,
				Exception_Information (E));

			Reraise_Occurrence (E);
	end Finalize;
	
	function Path (Self : Download_Data_Type) return Unbounded_String is
		(Self.Internal.Path);
	function Widget (Self : Download_Data_Type) return Gtk_Widget is
		(Self.Internal.Widget);
	function Data (Self : Download_Data_Type) return Stream_Element_Array is
		(Self.Internal.Data.all);

	-- Synchronous download function
	function Download (
		Path : Unbounded_String;
		Needs_Auth : Boolean := False) return Stream_Element_Array
	is begin
		if Simulate_Slow then
			delay 0.1;
		end if;

		if Has_Cached (+Path) then
			return Get_Cached (+Path);
		else
			Put_Debug ("Download " & (+Path));

			if Needs_Auth then
				raise Program_Error with "Auth unimplemented for Tasks.Download";
			end if;

			declare
				Data : Response.Data;
			begin
				Data := Client.Get (API.Bungie_Root & (+Path));
				Check_Status (Data);
				Cache (+Path, Response.Message_Body (Data));
				return Response.Message_Body (Data);
			end;
		end if;
	end Download;

	task body Download_Task is
		-- Types
		type Download_Queue_Entry is record
			Path : Unbounded_String;
			Widget : Gtk_Widget;
			Needs_Auth : Boolean;
		end record;

		-- Instantiation
		package DDV is new Ada.Containers.Vectors (Natural, Download_Data_Internal);
		subtype Download_Data_List is DDV.Vector;

		package DQV is new Ada.Containers.Vectors (Natural, Download_Queue_Entry);
		subtype Download_Queue_Type is DQV.Vector;

		-- Download cache
		Downloads : Download_Data_List;
		Download_Queue : Download_Queue_Type;

		function Has_Data return Boolean is (Downloads.Length > 0) with Inline;
		function All_Done return Boolean is (Download_Queue.Length = 0) with Inline;
	begin
		loop
			select
				-- Enqueue download
				accept Download (
					Path : Unbounded_String;
					Widget : Gtk_Widget;
					Needs_Auth : Boolean := False)
				do
					Widget.Ref; -- Keep a reference so the Widget is not invalidated
						-- Note this is not automatic because we're in a separate thread
						-- The Widget will be unrefed when the Download_Data_Internal leaves scope or the Queue is cleared
					Download_Queue.Append (Download_Queue_Entry'(Path, Widget, Needs_Auth));
				end Download;
			or
				when not Has_Data and not All_Done =>
					-- Execute download
					accept Execute;

					for DQE of Download_Queue loop
						declare
							SEA : constant Stream_Element_Array_Access := new Stream_Element_Array'(
								Download (
									DQE.Path,
									DQE.Needs_Auth));
						begin
							Downloads.Append (Download_Data_Internal'(DQE.Path, DQE.Widget, SEA));
						end;
					end loop;

					Download_Queue.Clear;
			or
				-- Return download data and reduce queue size
				when Has_Data =>
					accept Complete (Data : out Download_Data_Type)
					do
						Data.Internal := Downloads.Last_Element;
						Downloads.Delete_Last;
					end Complete;
			or -- Clear a download queue
				accept Clear;

				-- Since the queue is being cleared the
				-- Widget will never make it to a DDT,
				-- so Unref it now
				for DQE of Download_Queue loop
					DQE.Widget.Unref;
				end loop;

				Download_Queue.Clear;
				Downloads.Clear;
			or
				terminate;
			end select;
		end loop;
	exception
		when E : others =>
			Put_Line (Standard_Error,
				Exception_Information (E));

			Reraise_Occurrence (E);
	end Download_Task;
end Tasks.Download;
