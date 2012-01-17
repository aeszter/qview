with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; with Ada.Calendar.Formatting;
with Ada.Integer_Text_IO;
use Ada;
with POSIX.Process_Times; use POSIX.Process_Times;

-----------------
-- Diagnostics --
-----------------

package body Diagnostics is

   type Seconds is delta 0.1 digits 5;

   ----------------
   -- To_Seconds --
   ----------------

   function To_Seconds (T : Tick_Count) return Seconds is
   begin
      return Seconds (T) / Seconds (Ticks_Per_Second);
   end To_Seconds;


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

   procedure Put_Date is
      procedure Put_With_Zero (N : Natural) is
      begin
         if N < 10 then
            Put ('0');
         end if;
         Integer_Text_IO.Put (N, Width => 1);
      end Put_With_Zero;

      Now     : Ada.Calendar.Time := Ada.Calendar.Clock;
      Year    : Ada.Calendar.Year_Number;
      Month   : Ada.Calendar.Month_Number;
      Day     : Ada.Calendar.Day_Number;
      Hour    : Ada.Calendar.Formatting.Hour_Number;
      Minute  : Ada.Calendar.Formatting.Minute_Number;
      Second  : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;


   begin
      Ada.Calendar.Formatting.Split (Now,
                                     Year       => Year,
                                     Month      => Month,
                                     Day        => Day,
                                     Hour       => Hour,
                                     Minute     => Minute,
                                     Second     => Second,
                                     Sub_Second => Sub_Second);
      Put_With_Zero (Day);
      Ada.Text_IO.Put ('-');
      Put_With_Zero (Month);
      Ada.Text_IO.Put ('-');
      Ada.Integer_Text_IO.Put (Year, Width => 4);
      Ada.Text_IO.Put (' ');
      Put_With_Zero (Hour);
      Ada.Text_IO.Put (':');
      Put_With_Zero (Minute);
      Ada.Text_IO.Put (':');
      Put_With_Zero (Second);
      Text_IO.Put (" UTC");
   end Put_Date;

end Diagnostics;
