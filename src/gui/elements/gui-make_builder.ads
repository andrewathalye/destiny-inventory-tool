--  Single-purpose function:
--  Initialises GTK and returns a Gtkada_Builder after loading
--  the file _Name_
--
--  Do not use outside of elaboration / environment task

with Gtkada.Builder; use Gtkada.Builder;

function GUI.Make_Builder (Name : String) return Gtkada_Builder;
