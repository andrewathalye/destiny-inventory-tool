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

with GUI;

with Shared.Strings; use Shared.Strings;
with Shared.Debug;
with Shared.Files;
use Shared;

package body Tasks.Download is
	-- Debugging
	Simulate_Slow : constant Boolean := False;

	-- Synchronous download function
	function Download (
		Path : Unbounded_String;
		Needs_Auth : Boolean := False;
		Caching : Boolean := True) return Stream_Element_Array
	is
		Data : Response.Data;
	begin
		if Simulate_Slow then
			delay 0.1;
		end if;

		if Caching and then Files.Has_Cached (+Path) then
			return Files.Get_Cached (+Path);
		else
			Debug.Put_Line ("Download " & (+Path));

			if Needs_Auth then
				Data := Client.Get (
					URL => API.Bungie_Root & (+Path),
					Headers => GUI.Headers);
			else
				Data := Client.Get (API.Bungie_Root & (+Path));
			end if;

			Check_Status (Data);

			if Caching then
				Files.Cache (+Path, Response.Message_Body (Data));
			end if;

			return Response.Message_Body (Data);
		end if;
	end Download;

	task body Download_Task is
		-- Types
		type Download_Queue_Entry is record
			Path : Unbounded_String;
			Widget : Gtk_Widget;
			Needs_Auth : Boolean;
			Complete : Boolean;
		end record;

		-- Instantiation
		package DQV is new Ada.Containers.Vectors (Natural, Download_Queue_Entry);
		subtype Download_Queue_Type is DQV.Vector;

		-- Download cache
		Download_Queue : Download_Queue_Type;

		-- Local Data
		Callback_L : Download_Callback;
	begin
		loop
			select
				-- Does nothing since there was no process ongoing
				accept Interrupt;
			or
				-- Enqueue download
				accept Download (
					Path : Unbounded_String;
					Widget : Gtk_Widget;
					Needs_Auth : Boolean := False)
				do
					Widget.Ref; -- Keep a reference so the Widget is not invalidated
						-- Note this is not automatic because we're in a separate thread
					Download_Queue.Append (Download_Queue_Entry'(Path, Widget, Needs_Auth, False));
				end Download;
			or
				-- Execute download and perform callback
				accept Execute (Callback : Download_Callback) do
					Callback_L := Callback;
				end Execute;

				Execute_Loop :
				for DQE of Download_Queue loop
					select
						-- Interrupt the download 
						accept Interrupt;
						exit Execute_Loop;
					else
						if not DQE.Complete then
							declare
								SEA : constant Stream_Element_Array :=
									Download (
										DQE.Path,
										DQE.Needs_Auth);
							begin
								Callback_L (DQE.Path, DQE.Widget, SEA);
								DQE.Widget.Unref; -- let it be freed when needed
							end;
						end if;

						DQE.Complete := True;
					end select;
				end loop Execute_Loop;

				Download_Queue.Clear;
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
