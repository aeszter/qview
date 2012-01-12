with Ada.Text_IO; use Ada.Text_IO;

-----------------
-- Diagnostics --
-----------------

package body Diagnostics is
   --  Types from Florist
      ALIGNMENT : constant := Natural'Min (Standard'Maximum_Alignment, 8);
   --  worst-case alignment requirement

      SC_CLK_TCK : constant := 2;
   type int is range -2**31 .. (2**31)-1;
   for int'Size use 32;
   type long is range -2**31 .. (2**31)-1;
   for long'Size use 32;

   type clock_t is mod 2**32;
   for clock_t'Size use 32;
   type Tick_Count is new clock_t;

   type struct_tms is record
      tms_utime : clock_t;
      tms_stime : clock_t;
      tms_cutime : clock_t;
      tms_cstime : clock_t;
   end record;

   for struct_tms use record
      tms_utime at 0 range 0 .. 31;
      tms_stime at 4 range 0 .. 31;
      tms_cutime at 8 range 0 .. 31;
      tms_cstime at 12 range 0 .. 31;
   end record;
   pragma Convention (C_Pass_By_Copy, struct_tms);
   for struct_tms'Alignment use ALIGNMENT;
   pragma Warnings (Off);
   --  There may be holes in the record, due to
   --  components not defined by POSIX standard.
   for struct_tms'Size use 128;
   pragma Warnings (On);
   type tms_ptr is access constant struct_tms;
   pragma Convention (C, tms_ptr);
   type Process_Times is record
      tms : aliased struct_tms;
      Elapsed_Real_Time : clock_t;
   end record;

--  From Florist

   function sysconf (c_name : int) return long;
   pragma Import (C, sysconf, "sysconf");

   --  End Florist
   Ticks_Per_Second : constant Tick_Count :=
     Tick_Count (sysconf (SC_CLK_TCK));

   type Seconds is delta 0.1 digits 5;
   function To_Seconds (T : Tick_Count) return Seconds;
   function Get_Process_Times return Process_Times;
   function User_CPU_Time_Of (Times : Process_Times) return Tick_Count;
   function System_CPU_Time_Of (Times : Process_Times) return Tick_Count;
   function Descendants_User_CPU_Time_Of (Times : Process_Times)
                                          return Tick_Count;
   function Descendants_System_CPU_Time_Of (Times : Process_Times)
                                            return Tick_Count;

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
