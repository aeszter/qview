with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with DOM.Core;

package Viewer is
   sgeroot : constant String := "/cm/shared/apps/sge/current";
   procedure View;
   procedure View_Jobs_In_Queue (Queue : String);
   procedure View_Global_Jobs;
   procedure View_Waiting_Jobs;
   procedure View_Jobs_Of_User (User : String);
   procedure View_Job (Job_ID : String);
   procedure Set_Params (Params : String);
   function Setup_Parser (Selector : String) return DOM.Core.Document;
   procedure View_Detailed_Queues;
   package String_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Unbounded_String);
private
   procedure View_Jobs (Selector : String);
   Assumption_Error  : exception;
   My_Params : Unbounded_String;
end Viewer;
