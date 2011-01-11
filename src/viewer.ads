with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Viewer is
   sgeroot : constant String := "/cm/shared/apps/sge/current";
   procedure View;
   procedure View_Jobs_In_Queue (Queue : String);
   procedure View_Global_Jobs;
   procedure View_Waiting_Jobs;
   procedure View_Jobs_Of_User (User : String);
   procedure View_Job (Job_ID : String);
   procedure Set_Params (Params : String);
   procedure View_Detailed_Queues;
private
   procedure View_Jobs (Selector : String);
   Max_J_Name_Length : constant Natural := 20;
   Assumption_Error  : exception;
   My_Params : Unbounded_String;
end Viewer;
