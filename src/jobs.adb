with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with GNAT.Calendar.Time_IO;

package body Jobs is

   -------------
   -- New_Job --
   -------------

   function New_Job (Number, Name, Owner, Priority, State,
                     Slots, PE : Unbounded_String; Submission_Time : Time;
                     CPU, Mem, IO : Unbounded_String := Null_Unbounded_String;
                     Override_Tickets, Share_Tickets, Functional_Tickets : Natural := 0;
                     Urgency, Resource_Contrib, Waiting_Contrib          : Natural := 0;
                     Posix_Priority                                      : Integer := 0)
                     return Job
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
      if State = "dt" then
         J.State := dt;
      elsif State = "dr" then
         J.State := dr;
      elsif State = "Eqw" then
         J.State := Eqw;
      elsif State = "t" then
         J.State := t;
      elsif State = "r" then
         J.State := r;
      elsif State = "Rr" then
         J.State := Rr;
      elsif State = "qw" then
         J.State := qw;
      elsif State = "hqw" then
         J.State := hqw;
      else
         J.State := unknown;
         Ada.Text_IO.Put_Line ("<em>Error: found unknown job state "
                               & To_String (State) & "</em>");
      end if;
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
      J.Urgency            := Urgency;
      J.Resource_Contrib   := Resource_Contrib;
      J.Waiting_Contrib    := Waiting_Contrib;

      --  qstat -pri
      J.Posix_Priority     := Posix_Priority;

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
         when qw => return "qw";
         when hqw => return "hqw";
         when unknown => return "unknown";
      end case;

   end State_As_String;

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
      Number          : Unbounded_String;
      PE, Slots       : Unbounded_String;
      Priority        : Unbounded_String;
      Owner           : Unbounded_String;
      State           : Unbounded_String;
      Submission_Time : Time;
      Time_Buffer     : String (1 .. 19);
      F_Tickets       : Natural;
      S_Tickets       : Natural;
      O_Tickets       : Natural;

   begin
      for Index in 1 .. Length (List) loop
         Children := Child_Nodes (Item (List, Index - 1));
         for Ch_Index in 0 .. Length (Children) - 1 loop
            C := Item (Children, Ch_Index);
            if Name (C) = "JB_job_number" then
               Number := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JAT_prio" then
               Priority := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_name" then
               Job_Name := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_owner" then
               Owner := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "state" then
               State := To_Unbounded_String (Value (First_Child (C)));
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
            elsif Name (C) /= "#text" then
               Ada.Text_IO.Put_Line (Name (C));
            end if;
         end loop;
         Job_List.Append (New_Job (Number          => Number,
                                   Name            => Job_Name,
                                   Owner           => Owner,
                                   Priority        => Priority,
                                   State           => State,
                                   Slots           => Slots,
                                   PE              => PE,
                                   Submission_Time => Submission_Time,
                                   Share_Tickets   => S_Tickets,
                                   Functional_Tickets => F_Tickets,
                                  Override_Tickets => O_Tickets));
      end loop;
   end Append_List;


   ------------------
   -- Sort_By      --
   --  Purpose:  Sort the job list by any column/field
   --  Parameter Field: Title of the column to sort by
   ------------------


   procedure Sort_By (Field : String) is
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
      else
         Ada.Text_IO.Put_Line ("<em>Error</em>: Sorting by " & Field & " unimplemented");
      end if;
   end Sort_By;

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
      if Left.Number = "" then
         return False;
      elsif Left.Number = Right.Number then
         return True;
      else
         return False;
      end if;
   end Same;

end Jobs;
