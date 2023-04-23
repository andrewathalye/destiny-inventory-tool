with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Unchecked_Deallocation;

--  VSS
with VSS.JSON.Pull_Readers.Simple; use VSS.JSON.Pull_Readers.Simple;
use VSS.JSON.Pull_Readers;
with VSS.Text_Streams.Memory_UTF8_Input;
use VSS.Text_Streams.Memory_UTF8_Input;

package Shared.JSON is
   --  Types
   type Memory_UTF8_Input_Stream_Access is access Memory_UTF8_Input_Stream;

   --  Instantiated Subprograms
   procedure Free is new Unchecked_Deallocation
     (Memory_UTF8_Input_Stream, Memory_UTF8_Input_Stream_Access);

   --  Subprograms
   --  Note: Returned Stream_Access must be Freed
   function Get_Stream
     (JSON_Data : Unbounded_String) return Memory_UTF8_Input_Stream_Access;

   procedure Wait_Until_Key
     (Reader : in out JSON_Simple_Pull_Reader; Key : String);

   procedure Wait_Until_Event
     (Reader : in out JSON_Simple_Pull_Reader; Event : JSON_Event_Kind);
end Shared.JSON;
