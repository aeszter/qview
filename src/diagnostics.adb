with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; with Ada.Calendar.Formatting;
with Ada.Integer_Text_IO;
use Ada;
with POSIX.Process_Times; use POSIX.Process_Times;

-----------------
-- Diagnostics --
-----------------

package body Diagnostics is

   type Seconds is delta 0.1 digits 6;

   function To_Seconds (T : Tick_Count) return Seconds;

   procedure Put_Date is
      procedure Put_With_Zero (N : Natural);

      procedure Put_With_Zero (N : Natural) is
      begin
         if N < 10 then
            Put ('0');
         end if;
         Integer_Text_IO.Put (N, Width => 1);
      end Put_With_Zero;

      Now     : constant Ada.Calendar.Time := Ada.Calendar.Clock;
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

   procedure Put_Memory is
      Status_File : Ada.Text_IO.File_Type;
      Line        : String (1 .. 256);
      Last : Natural; -- length of line read
   begin
      Open (File     => Status_File,
            Mode     => In_File,
            Name     => "/proc/self/status");
      while not End_Of_File (Status_File) loop
         Get_Line (Status_File, Line, Last);
         if Line (Line'First .. Line'First + 6) = "VmPeak:" then
            Ada.Text_IO.Put (Line (Line'First + 7 .. Last));
            Ada.Text_IO.Put (" virtual");
         elsif Line (Line'First .. Line'First + 5) = "VmHWM:" then
            Ada.Text_IO.Put (Line (Line'First + 6 .. Last));
            Ada.Text_IO.Put (" RSS");
         end if;
      end loop;
      Close (Status_File);
   end Put_Memory;

   procedure Put_Time is
      Times : Process_Times;
      Self  : Seconds;
      Children : Seconds;
   begin
      Times := Get_Process_Times;
      Self := To_Seconds (User_CPU_Time_Of (Times) + System_CPU_Time_Of (Times));
      Children := To_Seconds (Descendants_User_CPU_Time_Of (Times)
                              + Descendants_System_CPU_Time_Of (Times));
      if Children > 0.0 then
         Put_Line (Self'Img & "s self " & Children'Img & "s children");
      else
         Put_Line (Self'Img & "s");
      end if;
   end Put_Time;

   function To_Seconds (T : Tick_Count) return Seconds is
   begin
      return Seconds (T) / Seconds (Ticks_Per_Second);
   exception
      when Constraint_Error =>
         return Seconds (0);
   end To_Seconds;

end Diagnostics;
