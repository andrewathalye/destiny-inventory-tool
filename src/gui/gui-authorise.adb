with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;

-- Gtk
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Window; use Gtk.Window;
with Gtk.Main;

-- Local Packages
with API.Authorise; use API;

procedure GUI.Authorise is
   -- Global Variables
   function Generate_State return String is
      package Unsigned_64_Random is new Ada.Numerics.Discrete_Random
        (Unsigned_64);
      G : Unsigned_64_Random.Generator;
   begin
      Unsigned_64_Random.Reset (G);

      declare
         Output : constant String := Unsigned_64_Random.Random (G)'Image;
      begin
         return Output (Output'First + 1 .. Output'Last);
      end;
   end Generate_State;

   State : constant String := Generate_State;

   -- Tasking
   task Auth_Loop is
      entry Start;
      entry Close;
      entry Stop;
   end Auth_Loop;

   task body Auth_Loop is
      Auth_Window : Gtk_Window;
      Auth_URL    : Gtk_Entry;
      Discard     : Boolean;
   begin
      loop
         select
            accept Start;

            Auth_Window := Gtk_Window (GUI.Builder.Get_Object ("auth_window"));
            Auth_URL    := Gtk_Entry (GUI.Builder.Get_Object ("auth_url"));

            Auth_URL.Set_Text
              (API.Authorise.OAuth_Authorise_Endpoint & "?client_id=" &
               API.Authorise.Client_ID & "&response_type=code" & "&state=" &
               State);

            Auth_Window.Show;

            Gtk_Main_Loop :
            loop
               select
                  accept Stop;
                  exit Gtk_Main_Loop;
               else
                  select
                     accept Close;
                     Auth_Window.Hide;
                  else
                     Discard := Gtk.Main.Main_Iteration;
                  end select;
               end select;
            end loop Gtk_Main_Loop;
         or
            terminate;
         end select;
      end loop;
   end Auth_Loop;
begin
   Auth_Loop.Start;
   GUI.Auth_Data := API.Authorise.Do_Authorise (State);
   Auth_Loop.Close;
   Auth_Loop.Stop;
end GUI.Authorise;
