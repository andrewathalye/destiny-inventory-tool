with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;

--  Gtk
with Gtk.Main;

--  AWS
with AWS.Headers;

--  Local Packages
with GUI.Elements.Authorise; use GUI.Elements.Authorise;

with API.Tasks.Authorise;

function GUI.Authorise return AWS.Headers.List is
   --  Global Variables
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
   Headers : AWS.Headers.List;

   --  Tasking
   task Auth_Loop is

      entry Start;
      entry Close;
      entry Stop;

   end Auth_Loop;

   task body Auth_Loop is
      Discard : Boolean;

   begin
      loop
         select
            accept Start;
            Auth_URL.Set_Text (API.Tasks.Authorise.Get_URL (State));
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
   Headers := API.Tasks.Authorise.Do_Authorise (State);
   Auth_Loop.Close;
   Auth_Loop.Stop;

   return Headers;
end GUI.Authorise;
