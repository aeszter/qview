package Viewer is
   sgeroot : constant String := "/cm/shared/apps/sge/current";
   procedure View;
   procedure View_Jobs_In_Queue (Queue : String);
   procedure View_Global_Jobs;
   procedure View_Waiting_Jobs;
private
   function Param_Is (Param : String; Expected : String) return Boolean;
   procedure View_Jobs (Selector : String);
   Max_J_Name_Length : constant Natural := 20;
end Viewer;