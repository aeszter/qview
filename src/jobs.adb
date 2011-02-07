with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with GNAT.Calendar.Time_IO;
with Resources;
with HTML;
with Ada.Exceptions; use Ada.Exceptions;

package body Jobs is

   -------------
   -- New_Job --
   -------------

   function New_Job (Number                            : Natural;
                     Name, Owner                       : Unbounded_String;
                     Priority                          : Fixed;
                     State                             : Job_State;
                     Slots, PE                         : Unbounded_String; Submission_Time : Time;
                     CPU, Mem, IO                      : Float := 0.0;
                     Override_Tickets, Share_Tickets   : Natural := 0;
                     Functional_Tickets                : Natural := 0;
                     Urgency                           : Float   := 0.0;
                     Resource_Contrib, Waiting_Contrib : Natural := 0;
                     Posix_Priority                    : Integer := 0;
                     Hard_Requests, Soft_Requests      : Resources.Resource_Lists.List
                     := Resources.Resource_Lists.Empty_List;
                     Queue                             : Unbounded_String := Null_Unbounded_String
                    ) return Job
   is
      J : Job;
   begin
      J.Number := Number;
      J.Full_Name := Name;
      if Length (J.Full_Name) > Max_Name_Length then
         J.Name := Head (Source => J.Full_Name,
                         Count  => Max_Name_Length);
         J.Name_Truncated := True;
      else
         J.Name := J.Full_Name;
         J.Name_Truncated := False;
      end if;

      J.Owner := Owner;
      J.Priority := Priority;
      J.State := State;
      J.Slots              := Slots;
      J.PE                 := PE;
      J.Submission_Time    := Submission_Time;
      --  qstat -ext
      J.CPU                := CPU;
      J.Mem                := Mem;
      J.IO                 := IO;
      J.Override_Tickets   := Override_Tickets;
      J.Share_Tickets      := Share_Tickets;
      J.Functional_Tickets := Functional_Tickets;

      --  qstat -urg
      J.Urgency            := Fixed (Urgency);
      J.Resource_Contrib   := Resource_Contrib;
      J.Waiting_Contrib    := Waiting_Contrib;

      --  qstat -pri
      J.Posix_Priority     := Posix_Priority;

      --  resources used for Bunching jobs
      J.Hard               := Hard_Requests;
      Resources.Sort (J.Hard);
      J.Soft               := Soft_Requests;
      Resources.Sort (J.Soft);
      J.Queue              := Queue;

      return J;
   end New_Job;


   ---------------------
   -- State_As_String --
   ---------------------

   function State_As_String (J : Job) return String is
   begin
      case J.State is
         when dt => return "dt";
         when dr => return "dr";
         when Eqw => return "Eqw";
         when t => return "t";
         when r => return "r";
         when Rr => return "Rr";
         when Rq => return "Rq";
         when qw => return "qw";
         when hqw => return "hqw";
         when unknown => return "unknown";
      end case;

   end State_As_String;

   function To_State (State : String) return Job_State is
   begin
      if State = "dt" then
         return dt;
      elsif State = "dr" then
         return dr;
      elsif State = "Eqw" then
         return Eqw;
      elsif State = "t" then
         return t;
      elsif State = "r" then
         return r;
      elsif State = "Rr" then
         return Rr;
      elsif State = "Rq" then
         return Rq;
      elsif State = "qw" then
         return qw;
      elsif State = "hqw" then
         return hqw;
      else
         return unknown;
      end if;
   end To_State;

   -------------
   -- On_Hold --
   -------------

   function On_Hold (J : Job) return Boolean is
   begin
      case J.State is
         when hqw => return True;
         when unknown => raise Constraint_Error;
         when others => return False;
      end case;
   end On_Hold;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (J : Job) return Boolean is
   begin
      case J.State is
         when Eqw => return True;
         when unknown => raise Constraint_Error;
         when others => return False;
      end case;
   end Has_Error;

   ------------------
   -- Name_As_HTML --
   ------------------

   function Name_As_HTML (J : Job) return String is
   begin
      if J.Name_Truncated then
         return To_String ("<acronym title=""" & J.Full_Name & """>"
                           & J.Name & "</acronym>");
      else
         return To_String (J.Name);
      end if;

   end Name_As_HTML;

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (List : Node_List) is
      Children        : Node_List;
      C               : Node;
      --  Job fields
      Job_Name        : Unbounded_String;
      Number          : Integer;
      PE, Slots       : Unbounded_String;
      Priority        : Fixed;
      Owner           : Unbounded_String;
      State           : Jobs.Job_State;
      Submission_Time : Time;
      Time_Buffer     : String (1 .. 19);
      F_Tickets       : Natural;
      S_Tickets       : Natural;
      O_Tickets       : Natural;
      CPU_Usage       : Float;
      Mem_Usage       : Float;
      IO_Usage        : Float;
      Wait_Contrib    : Natural;
      Resource_Contrib : Natural;
      Urgency          : Float;
      User_Priority : Natural;

   begin
      for Index in 1 .. Length (List) loop
         Children := Child_Nodes (Item (List, Index - 1));
         for Ch_Index in 0 .. Length (Children) - 1 loop
            C := Item (Children, Ch_Index);
            if Name (C) = "JB_job_number" then
               Number := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "JAT_prio" then
               Priority := Fixed'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_name" then
               Job_Name := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_owner" then
               Owner := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "state" then
               State := To_State (Value (First_Child (C)));
            elsif Name (C) = "JB_submission_time" or else
               Name (C) = "JAT_start_time" then
               Time_Buffer := Value (First_Child (C));
               if Time_Buffer (11) /= 'T' then
                  raise Time_Error;
               end if;
               Time_Buffer (11) := ' ';
               Submission_Time := GNAT.Calendar.Time_IO.Value (Time_Buffer);
            elsif Name (C) = "slots" then
               Slots := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "requested_pe" then
               PE := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "ftickets" then
               F_Tickets := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "stickets" then
               S_Tickets := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "otickets" then
               O_Tickets := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "cpu_usage" then
               CPU_Usage := Float'Value (Value (First_Child (C)));
            elsif Name (C) = "mem_usage" then
               Mem_Usage := Float'Value (Value (First_Child (C)));
            elsif Name (C) = "io_usage" then
               IO_Usage := Float'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_wtcontr" then
               Wait_Contrib := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_rrcontr" then
               Resource_Contrib := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_nurg" then
               Urgency := Float'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_priority" then
               User_Priority := Integer'Value (Value (First_Child (C)));
            end if;
         end loop;
         Job_List.Append (New_Job (Number             => Number,
                                   Name               => Job_Name,
                                   Owner              => Owner,
                                   Priority           => Priority,
                                   State              => State,
                                   Slots              => Slots,
                                   PE                 => PE,
                                   Submission_Time    => Submission_Time,
                                   Share_Tickets      => S_Tickets,
                                   Functional_Tickets => F_Tickets,
                                   Override_Tickets   => O_Tickets,
                                   CPU                => CPU_Usage,
                                   Mem                => Mem_Usage,
                                   IO                 => IO_Usage,
                                   Waiting_Contrib    => Wait_Contrib,
                                   Resource_Contrib   => Resource_Contrib,
                                   Posix_Priority     => User_Priority,
                                   Urgency            => Urgency));
      end loop;
   exception
      when E : others
         => HTML.Error ("Unable to read job info: " & Exception_Message (E));
   end Append_List;


   ------------------
   -- Sort_By      --
   --  Purpose:  Sort the job list by any column/field
   --  Parameter Field: Title of the column to sort by
   ------------------


   procedure Sort_By (Field : String; Direction : String) is
   begin
      if Field = "Number" then
         Sorting_By_Number.Sort (Job_List);
      elsif Field = "Name" then
         Sorting_By_Name.Sort (Job_List);
      elsif Field = "Owner" then
         Sorting_By_Owner.Sort (Job_List);
      elsif Field = "Priority" then
         Sorting_By_Priority.Sort (Job_List);
      elsif Field = "Submitted" then
         Sorting_By_Submission_Time.Sort (Job_List);
      elsif Field = "Slots" then
         Sorting_By_Slots.Sort (Job_List);
      elsif Field = "State" then
         Sorting_By_State.Sort (Job_List);
      elsif Field = "CPU" then
         Sorting_By_CPU_Used.Sort (Job_List);
      elsif Field = "Memory" then
         Sorting_By_Memory_Used.Sort (Job_List);
      elsif Field = "IO" then
         Sorting_By_IO_Used.Sort (Job_List);
      elsif Field = "Priority" then
         Sorting_By_Priority.Sort (Job_List);
      elsif Field = "O" then
         Sorting_By_Override.Sort (Job_List);
      elsif Field = "S" then
         Sorting_By_Share.Sort (Job_List);
      elsif Field = "F" then
         Sorting_By_Functional.Sort (Job_List);
      elsif Field = "Urgency" then
         Sorting_By_Urgency.Sort (Job_List);
      elsif Field = "Resource" then
         Sorting_By_Resource_Contrib.Sort (Job_List);
      elsif Field = "Waiting" then
         Sorting_By_Waiting_Contrib.Sort (Job_List);
      elsif Field = "Custom" then
         Sorting_By_Posix_Priority.Sort (Job_List);
      else
         Ada.Text_IO.Put_Line ("<em>Error</em>: Sorting by " & Field & " unimplemented");
      end if;
      if Direction = "dec" then
         Job_List.Reverse_Elements;
      end if;
   end Sort_By;

   ------------------------
   -- Precedes_By_Resources   --
   --  Purpose: Check whether one job should precede another when sorted by various resources
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting.
   --  This does a multi-column sort by slots, runtime, PE and so on.
   --  If neither a < b nor a > b, then a and b belong to the same Bunch.
   ------------------------

   function Precedes_By_Resources (Left, Right : Job) return Boolean is
   begin
      if Left.Queue < Right.Queue then
         return True;
      elsif Left.Queue > Right.Queue then
         return False;
      elsif Left.PE < Right.PE then
         return True;
      elsif Left.PE > Right.PE then
         return False;
      elsif Left.Slots < Right.Slots then
         return True;
      elsif Left.Slots > Right.Slots then
         return False;
      elsif Resources.Precedes (Left.Hard, Right.Hard) then
         return True;
      elsif Resources.Precedes (Right.Hard, Left.Hard) then
         return False;
      elsif Resources.Precedes (Left.Soft, Right.Soft) then
         return True;
      elsif Resources.Precedes (Right.Soft, Left.Soft) then
         return False;
      else
         return False;
      end if;
   end Precedes_By_Resources;

   ------------------------
   -- Precedes_By_Name   --
   --  Purpose: Check whether one job should precede another when sorted by name
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   ------------------------

   function Precedes_By_Name (Left, Right : Job) return Boolean is
   begin
      return Left.Full_Name < Right.Full_Name;
   end Precedes_By_Name;

   --------------------------
   -- Precedes_By_Number   --
   --  Purpose: Check whether one job should precede another when sorted by number
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Number (Left, Right : Job) return Boolean is
   begin
      return Left.Number < Right.Number;
   end Precedes_By_Number;

   --------------------------
   -- Precedes_By_Owner   --
   --  Purpose: Check whether one job should precede another when sorted by owner
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Owner (Left, Right : Job) return Boolean is
   begin
      return Left.Owner < Right.Owner;
   end Precedes_By_Owner;

   --------------------------
   -- Precedes_By_Priority   --
   --  Purpose: Check whether one job should precede another when sorted by priority
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Priority (Left, Right : Job) return Boolean is
   begin
      return Left.Priority < Right.Priority;
   end Precedes_By_Priority;

   --------------------------
   -- Precedes_By_Submission_time   --
   --  Purpose: Check whether one job should precede another when sorted by submission time
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Submission_Time (Left, Right : Job) return Boolean is
   begin
      return Left.Submission_Time < Right.Submission_Time;
   end Precedes_By_Submission_Time;

   --------------------------
   -- Precedes_By_Slots   --
   --  Purpose: Check whether one job should precede another when sorted by
   --           number of slots
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Slots (Left, Right : Job) return Boolean is
   begin
      return Integer'Value (To_String (Left.Slots)) < Integer'Value (To_String (Right.Slots));
   end Precedes_By_Slots;

   --------------------------
   -- Precedes_By_State   --
   --  Purpose: Check whether one job should precede another when sorted by state
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_State (Left, Right : Job) return Boolean is
   begin
      return Left.State < Right.State;
   end Precedes_By_State;

   --------------------------
   -- Precedes_By_CPU_Used   --
   --  Purpose: Check whether one job should precede another when sorted by CPU time
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_CPU_Used (Left, Right : Job) return Boolean is
   begin
      return Left.CPU < Right.CPU;
   end Precedes_By_CPU_Used;

   --------------------------
   -- Precedes_By_Memory_Used   --
   --  Purpose: Check whether one job should precede another when sorted by memory usage
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Memory_Used (Left, Right : Job) return Boolean is
   begin
      return Left.Mem < Right.Mem;
   end Precedes_By_Memory_Used;

   --------------------------
   -- Precedes_By_IO_Used   --
   --  Purpose: Check whether one job should precede another when sorted by IO usage
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_IO_Used (Left, Right : Job) return Boolean is
   begin
      return Left.IO < Right.IO;
   end Precedes_By_IO_Used;

   --------------------------
   -- Precedes_By_Override   --
   --  Purpose: Check whether one job should precede another when sorted by override tickets
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Override (Left, Right : Job) return Boolean is
   begin
      return Left.Override_Tickets < Right.Override_Tickets;
   end Precedes_By_Override;

   --------------------------
   -- Precedes_By_Share   --
   --  Purpose: Check whether one job should precede another when sorted by share tickets
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Share (Left, Right : Job) return Boolean is
   begin
      return Left.Share_Tickets < Right.Share_Tickets;
   end Precedes_By_Share;

   --------------------------
   -- Precedes_By_Functional   --
   --  Purpose: Check whether one job should precede another when sorted by functional tickets
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Functional (Left, Right : Job) return Boolean is
   begin
      return Left.Functional_Tickets < Right.Functional_Tickets;
   end Precedes_By_Functional;

   --------------------------
   -- Precedes_By_Urgency   --
   --  Purpose: Check whether one job should precede another when sorted by urgency tickets
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Urgency (Left, Right : Job) return Boolean is
   begin
      return Left.Urgency < Right.Urgency;
   end Precedes_By_Urgency;

   --------------------------
   -- Precedes_By_Waiting_Contrib   --
   --  Purpose: Check whether one job should precede another when sorted by waiting time contribution to priority
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Waiting_Contrib (Left, Right : Job) return Boolean is
   begin
      return Left.Waiting_Contrib < Right.Waiting_Contrib;
   end Precedes_By_Waiting_Contrib;

   --------------------------
   -- Precedes_By_Resource_Contrib   --
   --  Purpose: Check whether one job should precede another when sorted by resource contribution to priority
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Resource_Contrib (Left, Right : Job) return Boolean is
   begin
      return Left.Resource_Contrib < Right.Resource_Contrib;
   end Precedes_By_Resource_Contrib;

   --------------------------
   -- Precedes_By_Ppsix_Priority   --
   --  Purpose: Check whether one job should precede another when sorted by POSIX priority
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Posix_Priority (Left, Right : Job) return Boolean is
   begin
      return Left.Posix_Priority < Right.Posix_Priority;
   end Precedes_By_Posix_Priority;


   ------------------------
   -- Same               --
   --  Purpose: Check whether two jobs are identical
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left and Right are identical
   --  Description: This implements the "=" operator for package Doubly_Linked_Lists;
   --    we compare job numbers (since they are unique)
   ------------------------


   function Same (Left, Right : Job) return Boolean is
   begin
      if Left.Number = 0 then
         return False;
      elsif Left.Number = Right.Number then
         return True;
      else
         return False;
      end if;
   end Same;

end Jobs;
