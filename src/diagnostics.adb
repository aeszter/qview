with Ada.Text_IO; use Ada.Text_IO;

package body Diagnostics is

   ---------------------------------------------------------------------------
   -- Put_Time ---------------------------------------------------------------
   --  Purpose: Put User and CPU times for both self and children to Standard Out
   --  Parameters: none
   ---------------------------------------------------------------------------

   procedure Put_Time is
      Times : Process_Times;
      Self  : Seconds;
      Children : Seconds;
   begin
      Times := Get_Process_Times;
      Self := To_Seconds (User_CPU_Time_Of (Times) + System_CPU_Time_Of (Times));
      Children := To_Seconds (Descendants_User_CPU_Time_Of (Times)
                   + Descendants_System_CPU_Time_Of (Times));
      Put_Line (Self'Img & "s self " & Children'Img & "s children");
   end Put_Time;

   function To_Seconds (T : Tick_Count) return Seconds is
   begin
      return Seconds (T) / Seconds (Ticks_Per_Second);
   end To_Seconds;

      function times (buf : tms_ptr) return clock_t;
   pragma Import (C, times, "times");

   -------------------------
   --  Get_Process_Times  --
   -------------------------

   function Get_Process_Times return Process_Times is
      t : Process_Times;
   begin
      t.Elapsed_Real_Time := times (t.tms'Unchecked_Access);
      return Process_Times (t);
   end Get_Process_Times;
   -----------------------
   --  User_CPU_Time_Of --
   -----------------------

   function User_CPU_Time_Of (Times : Process_Times) return Tick_Count is
   begin
      return Tick_Count (Times.tms.tms_utime);
   end User_CPU_Time_Of;

   --------------------------
   --  System_CPU_Time_Of  --
   --------------------------

   function System_CPU_Time_Of (Times : Process_Times) return Tick_Count is
   begin
      return Tick_Count (Times.tms.tms_stime);
   end System_CPU_Time_Of;

   ------------------------------------
   --  Descendants_User_CPU_Time_Of  --
   ------------------------------------

   function Descendants_User_CPU_Time_Of (Times : Process_Times)
      return Tick_Count is
   begin
      return Tick_Count (Times.tms.tms_cutime);
   end Descendants_User_CPU_Time_Of;

   --------------------------------------
   --  Descendants_System_CPU_Time_Of  --
   --------------------------------------

   function Descendants_System_CPU_Time_Of (Times : Process_Times)
      return Tick_Count is
   begin
      return Tick_Count (Times.tms.tms_cstime);
   end Descendants_System_CPU_Time_Of;


end Diagnostics;
