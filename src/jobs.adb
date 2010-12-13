with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with Ada.Text_IO;

package body Jobs is

   -------------
   -- New_Job --
   -------------

   function New_Job
     (Number, Full_Name, Name, Owner, Priority, State,
                     Slots, PE, Submission_Time : Unbounded_String)
      return Job
   is
      J : Job;
   begin
      J.Number := Number;
      J.Full_Name := Full_Name;
      J.Name := Name;
      J.Owner := Owner;
      J.Priority := Priority;
      J.State := State;
      J.Slots := Slots;
      J.PE := PE;
      J.Submission_Time := Submission_Time;
      return J;
   end New_Job;

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (List : Node_List) is
      Children : Node_List;
      C        : Node;
      --  Job fields
      Short_Name : Unbounded_String;
      Full_Name  : Unbounded_String;
      Number     : Unbounded_String;
      PE, Slots  : Unbounded_String;
      Priority   : Unbounded_String;
      Owner      : Unbounded_String;
      State      : Unbounded_String;
      Submission_Time : Unbounded_String;

   begin
      for Index in 1 .. Length (List) loop
--         Ada.Text_IO.Put ("<tr>");
         Children := Child_Nodes (Item (List, Index - 1));
         for Ch_Index in 0 .. Length (Children) - 1 loop
            C := Item (Children, Ch_Index);
            if Name (C) = "JB_job_number" then
               Number := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JAT_prio" then
               Priority := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_name" then
               Full_Name := To_Unbounded_String (Value (First_Child (C)));
               if Length (Full_Name) > Max_Name_Length then
                  Short_Name := Head (Source => Full_Name,
                                Count  => Max_Name_Length);
               else
                  Short_Name := Full_Name;
               end if;
            elsif Name (C) = "JB_owner" then
               Owner := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "state" then
               State := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_submission_time" then
               Submission_Time :=
               To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "slots" then
               Slots := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "requested_pe" then
               PE := To_Unbounded_String (Value (First_Child (C)));
            end if;
         end loop;
         Job_List.Append (New_Job (Number          => Number,
                                   Full_Name       => Full_Name,
                                   Name            => Short_Name,
                                   Owner           => Owner,
                                   Priority        => Priority,
                                   State           => State,
                                   Slots           => Slots,
                                   PE              => PE,
                                   Submission_Time => Submission_Time));
--         HTML.Put_Cell (Data => J_Number, Link_Param => "job_id");
--         HTML.Put_Cell (Data => J_Owner, Link_Param => "user");
--         HTML.Put_Cell (Data => J_Name);
--         HTML.Put_Cell (Data => J_Priority);
--         HTML.Put_Cell (Data => J_Submission_Time);
--         HTML.Put_Cell (Data => J_Slots, Tag => "td class=""right""");
--         HTML.Put_Cell (Data => J_State);
--         Ada.Text_IO.Put ("</tr>");
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
      else
         Ada.Text_IO.Put_Line ("<em>Error</em>: Sorting by " & Field & "unimplemented");
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
   end;

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
         return false;
      elsif Left.Number = Right.Number then
         return true;
      else
         return false;
      end if;
   end Same;

end Jobs;
