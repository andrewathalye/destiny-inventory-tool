pragma Ada_2022;

with Ada.Containers.Vectors;
use Ada.Containers;

-- AWS
with AWS.Client;
with AWS.Response;
use AWS;

-- Local Packages
with API; use API;
with Shared; use Shared;

package body Tasks.Download is
	function Download (
		Path : Unbounded_String;
		Needs_Auth : Boolean := False) return Stream_Element_Array
	is begin
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
		package DDV is new Ada.Containers.Vectors (Natural, Download_Data_Type);
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
							Downloads.Append (Download_Data_Type'(DQE.Path, DQE.Widget, SEA));
						end;
					end loop;

					Download_Queue.Clear;
			or
				-- Return download data and reduce queue size
				when Has_Data =>
					accept Complete (Data : out Download_Data_Type)
					do
						Data := Downloads.Last_Element;
						Downloads.Delete_Last;
					end Complete;
			or -- Eliminate downloads for widgets that no longer exist
				-- Done inside a rendezvous to avoid race conditions

				accept Clear do
					Download_Queue.Clear;
					Downloads.Clear;
				end Clear;
			or
				terminate;
			end select;
		end loop;
	exception
		when E : others =>
			Put_Debug (E'Image);
	end Download_Task;
end Tasks.Download;
