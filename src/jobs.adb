with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with Ada.Text_IO;
with Ada.Calendar;   use Ada.Calendar;
with GNAT.Calendar.Time_IO;
with Ada.Calendar.Conversions;
with Resources;      use Resources; use Resources.Resource_Lists;
with Slots;          use Slots; use Slots.Slot_Lists;
with Utils;          use Utils; use Utils.String_Lists;
with HTML;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Interfaces.C;

package body Jobs is

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

   --------------
   -- To_State --
   --------------

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

   --------------
   -- End_Time --
   --------------

   function End_Time (J : Job) return Time is
   begin
      return J.Submission_Time
        + Ada.Real_Time.To_Duration (Ada.Real_Time.Seconds (Resources.Get_Numerical (J.Hard, "h_rt")));
   end End_Time;

   function Remaining_Time (J : Job) return Duration is
   begin
      return End_Time (J) - Ada.Calendar.Clock;
   end Remaining_Time;


   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (List : Node_List) is
      N : Node;
   begin
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         if Name (N) = "djob_info" then
            N := Item (Child_Nodes (N), 1);
            if Name (N) /= "element" then
               raise Assumption_Error with "Expected ""element"" Found """
                 & Name (N) & """";
            end if;
         end if;
         Job_List.Append (New_Job (Child_Nodes (N)));
      end loop;
   exception
      when E : others
         => HTML.Error ("Unable to read job info: " & Exception_Message (E));
   end Append_List;

   -------------
   -- New_Job --
   -------------

   function New_Job (List : Node_List) return Job is
      C           : Node;
      A           : Attr;
      J           : Job;
      Time_Buffer : String (1 .. 19);
   begin
      J.Merge_Std_Err := Undecided;
      J.Reserve := Undecided;
      J.Mem := 0.0;
      J.IO := 0.0;
      J.CPU := 0.0;
      for Index in 0 .. Length (List) - 1 loop
         C := Item (List, Index);
         if Name (C) = "JB_job_number" then
            J.Number := Integer'Value (Value (First_Child (C)));
         elsif Name (C) = "JAT_prio" then
            J.Priority := Fixed'Value (Value (First_Child (C)));
         elsif Name (C) = "JB_name" or else
         Name (C) = "JB_job_name" then
            J.Full_Name := To_Unbounded_String (Value (First_Child (C)));
            if Length (J.Full_Name) > Max_Name_Length then
               J.Name := Head (Source => J.Full_Name,
                         Count  => Max_Name_Length);
               J.Name_Truncated := True;
            else
               J.Name := J.Full_Name;
               J.Name_Truncated := False;
            end if;
         elsif Name (C) = "JB_owner" then
            J.Owner := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "state" then
            J.State := To_State (Value (First_Child (C)));
         elsif Name (C) = "JB_submission_time" then
            if Value (First_Child (C))'Length > 11 and then
              Value (First_Child (C)) (11) = 'T' then
               Time_Buffer := Value (First_Child (C));
               Time_Buffer (11) := ' ';
               J.Submission_Time := GNAT.Calendar.Time_IO.Value (Time_Buffer);
            else
               J.Submission_Time := Ada.Calendar.Conversions.To_Ada_Time
                 (Interfaces.C.long'Value (Value (First_Child (C))));
            end if;

         elsif Name (C) = "JAT_start_time" then
            Time_Buffer := Value (First_Child (C));
            if Time_Buffer (11) /= 'T' then
               raise Time_Error;
            end if;
            Time_Buffer (11) := ' ';
            J.Submission_Time := GNAT.Calendar.Time_IO.Value (Time_Buffer);
         elsif Name (C) = "queue_name" then
            null; -- ignore
         elsif Name (C) = "slots" then
            J.Slot_Number := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "ftickets" then
            J.Functional_Tickets := Integer'Value (Value (First_Child (C)));
         elsif Name (C) = "stickets" then
            J.Share_Tickets := Integer'Value (Value (First_Child (C)));
         elsif Name (C) = "otickets" then
            J.Override_Tickets := Integer'Value (Value (First_Child (C)));
         elsif Name (C) = "cpu_usage" then
            J.CPU := Float'Value (Value (First_Child (C)));
         elsif Name (C) = "mem_usage" then
            J.Mem := Float'Value (Value (First_Child (C)));
         elsif Name (C) = "io_usage" then
            J.IO := Float'Value (Value (First_Child (C)));
         elsif Name (C) = "JB_wtcontr" then
            J.Waiting_Contrib := Integer'Value (Value (First_Child (C)));
         elsif Name (C) = "JB_rrcontr" then
            J.Resource_Contrib := Integer'Value (Value (First_Child (C)));
         elsif Name (C) = "JB_nurg" then
            J.Urgency := Fixed'Value (Value (First_Child (C)));
         elsif Name (C) = "JB_priority" then
            J.Posix_Priority := Integer'Value (Value (First_Child (C)));
         elsif Name (C) = "hard_req_queue" then
            J.Queue := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "full_job_name" then
            null; -- ignore
         elsif Name (C) = "requested_pe" then
            A := Get_Named_Item (Attributes (C), "name");
            J.PE := To_Unbounded_String (Value (A));
         elsif Name (C) = "hard_request" then
            A := Get_Named_Item (Attributes (C), "name");
            J.Hard.Append (New_Resource (Name  => Value (A),
                                        Value => Value (First_Child (C))));
         elsif Name (C) = "soft_request" then
            A := Get_Named_Item (Attributes (C), "name");
            J.Soft.Append (New_Resource (Name  => Value (A),
                                        Value => Value (First_Child (C))));
         elsif Name (C) = "predecessor_jobs" or else
            Name (C) = "predecessor_jobs_req" then
            null; -- ignore
         elsif Name (C) = "JB_hard_resource_list" then
            Extract_Resource_List (J, Child_Nodes (C));
         elsif Name (C) = "JB_hard_queue_list" then
            Extract_Queue_List (J, Child_Nodes (C));
         elsif Name (C) = "JB_ja_tasks" then
            Extract_Tasks (J, Child_Nodes (C));
         elsif Name (C) = "JB_pe_range" then
            Extract_PE_Range (J, Child_Nodes (C));

         elsif Name (C) = "JB_department" then
            J.Department := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JB_project" then
            if Length (Child_Nodes (C)) > 0 then
               J.Project := To_Unbounded_String (Value (First_Child (C)));
            else
               J.Project := To_Unbounded_String ("none");
            end if;
         elsif Name (C) = "JB_ar" then
            J.Job_Array := To_Unbounded_String (Value (First_Child (C)));
         elsif            Name (C) = "JB_ja_structure" then
            null;  -- to array
         elsif Name (C) = "JB_exec_file" then
            J.Exec_File := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JB_group" then
            J.Group := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JB_merge_stderr" then
            J.Merge_Std_Err := To_Tri_State (Value (First_Child (C)));
         elsif Name (C) = "JB_stdout_path_list" then
            Extract_Paths (J.Std_Out_Paths, Child_Nodes (C));
         elsif Name (C) = "JB_stderr_path_list" then
            Extract_Paths (J.Std_Err_Paths, Child_Nodes (C));
         elsif Name (C) = "JB_script_file" then
            J.Script_File := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JB_cwd" then
            J.Directory := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JB_reserve" then
            J.Reserve := To_Tri_State (Value (First_Child (C)));
         elsif Name (C) = "JB_pe" then
            J.PE := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JB_notify" then
            J.Notify := To_Tri_State (Value (First_Child (C)));
         elsif Name (C) = "JB_account" then
            J.Account := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "granted_pe" then
            null;
         elsif Name (C) = "JB_urg" or else
           Name (C) = "JB_dlcontr" or else
           Name (C) = "JAT_ntix" or else
           Name (C) = "JAT_share" or else
           Name (C) = "JB_jobshare" or else
           Name (C) = "tickets" or else
           Name (C) = "JB_nppri" or else
           Name (C) = "JB_uid" or else
           Name (C) = "JB_gid" or else
           Name (C) = "JB_mail_list" or else
           Name (C) = "JB_mail_options" or else
           Name (C) = "JB_deadline" or else
           Name (C) = "JB_shell_list" or else
           Name (C) = "JB_env_list" or else
           Name (C) = "JB_checkpoint_attr" or else
           Name (C) = "JB_checkpoint_interval" or else
           Name (C) = "JB_verify" or else
           Name (C) = "JB_restart" or else
           Name (C) = "JB_soft_wallclock_gmt" or else
           Name (C) = "JB_hard_wallclock_gmt" or else
           Name (C) = "JB_execution_time" or else
           Name (C) = "JB_script_size" or else
           Name (C) = "JB_version" or else
           Name (C) = "JB_type" or else
           Name (C) = "JB_verify_suitable_queues" or else
           Name (C) = "JB_override_tickets" then
            null;

         elsif Name (C) /= "#text" then
            Ada.Text_IO.Put_Line ("Unknown Field: " & Name (C));
         end if;
      end loop;

      if J.Queue = "" then
         J.Queue := To_Unbounded_String ("*");
      end if;


      Resources.Sort (J.Hard);
      Resources.Sort (J.Soft);
      return J;
   exception
      when E : others =>
         HTML.Error ("Failed to parse job: " & Exception_Message (E));
         HTML.Error ("Node type: """ & Name (C)
                     & """ Value: """ & Value (First_Child (C)) & """");
         return J;
   end New_Job;

   ---------------------------
   -- Extract_Resource_List --
   ---------------------------

   procedure Extract_Resource_List (J : in out Job; Resource_Nodes : Node_List) is
         Resource_Tags      : Node_List;
         N, R               : Node;
         Res_Value          : Unbounded_String;
      Res_Name           : Unbounded_String;
      Res_Bool           : Boolean;
      Res_State : Tri_State;
   begin
      for I in 1 .. Length (Resource_Nodes) loop
         N := Item (Resource_Nodes, I - 1);
         if Name (N) = "qstat_l_requests" then
            Res_Bool := False;
            Res_State := Undecided;
            Resource_Tags := Child_Nodes (N);
            for J in 1 .. Length (Resource_Tags) loop
               R := Item (Resource_Tags, J - 1);
               if Name (R) = "CE_name" then
                  Res_Name := To_Unbounded_String (Value (First_Child (R)));
               elsif Name (R) = "CE_stringval" then
                  Res_Value := To_Unbounded_String (Value (First_Child (R)));
               elsif Name (R) = "CE_valtype" and then
              Value (First_Child (R)) = "5" then
                  Res_Bool := True;
                  --  maybe check for relop here?
               end if;
            end loop;
            if Res_Bool then
               if Res_Value = "TRUE" then
                  Res_State := True;
               elsif Res_Value = "FALSE" then
                  Res_State := False;
               end if;
               J.Hard.Append (New_Resource (Name  => Res_Name,
                                            Value => Res_Value,
                                            Boolean_Valued => Res_Bool,
                                            State => Res_State));
            end if;
         end if;
      end loop;
   end Extract_Resource_List;

   ------------------------
   -- Extract_Queue_List --
   ------------------------

   procedure Extract_Queue_List (J : in out Job; Destin_Nodes : Node_List) is
      QR_Nodes     : Node_List;
      N, Q         : Node;
   begin
      for I in 1 .. Length (Destin_Nodes) loop
         N := Item (Destin_Nodes, I - 1);
         if Name (N) = "destin_ident_list" then
            QR_Nodes := Child_Nodes (N);
            for K in 1 .. Length (QR_Nodes) loop
               Q := Item (QR_Nodes, K - 1);
               if Name (Q) = "QR_name" then
                  J.Queue_List.Append (To_Unbounded_String (Value (First_Child (Q))));
               end if;
            end loop;
         end if;
      end loop;
   end Extract_Queue_List;

   ----------------------
   -- Extract_PE_Range --
   ----------------------

   procedure Extract_PE_Range (J : in out Job; Children : Node_List) is
      Ranges                           : Node_List;
      N, R                             : Node;
      Slots_Min, Slots_Step, Slots_Max : Natural;
   begin
      for I in 1 .. Length (Children) loop
         N := Item (Children, I - 1);
         if Name (N) = "ranges" then
            Ranges := Child_Nodes (N);
            for J in 1 .. Length (Ranges) loop
               R := Item (Ranges, J - 1);
               if Name (R) = "RN_min" then
                  Slots_Min := Integer'Value (Value (First_Child (R)));
               elsif Name (R) = "RN_max" then
                  Slots_Max := Integer'Value (Value (First_Child (R)));
               elsif Name (R) = "RN_step" then
                  Slots_Step := Integer'Value (Value (First_Child (R)));
               end if;
            end loop;
            J.Slot_List.Append (Slots.New_Range (Min  => Slots_Min,
                                                 Max  => Slots_Max,
                                                 Step => Slots_Step));
         end if;
      end loop;
   end Extract_PE_Range;

   -------------------
   -- Extract_Tasks --
   -------------------

   procedure Extract_Tasks (J : in out Job; Task_Nodes : Node_List) is
      Children                                 : Node_List;
      Messages                                 : Node_List;
      Task_List_Nodes, PE_Task_Nodes, JG_Nodes : Node_List;
      N, M                                     : Node;
      JA_Tasks                                 : Node;
      Sublist                                  : Node;
      PE_Task_Entry, Element_Node, JG_Entry    : Node;
   begin
      for H in 1 .. Length (Task_Nodes) loop
         JA_Tasks := Item (Task_Nodes, H - 1);
         if Name (JA_Tasks) = "ja_tasks"
           or else Name (JA_Tasks) = "ulong_sublist" then
            Children := Child_Nodes (JA_Tasks);
            HTML.Comment ("JA_Tasks """ & Name (JA_Tasks) & """" & Length (Children)'Img);
            for I in 1 .. Length (Children) loop
               N := Item (Children, I - 1);
               HTML.Comment (Name (N));
               if Name (N) = "JAT_message_list" then
                  Sublist := Item (Child_Nodes (N), 1);
                  if Name (Sublist) /= "ulong_sublist" then
                     raise Assumption_Error;
                  end if;
                  Messages := Child_Nodes (Sublist);
                  for K in 1 .. Length (Messages) loop
                     M := Item (Messages, K - 1);
                     if Name (M) = "QIM_message" then
                        J.Message_List.Append (To_Unbounded_String (Value (First_Child (M))));
                     end if;
                  end loop;
               elsif Name (N) = "JAT_task_list" then
                  HTML.Comment ("JAT_tast_list : ");
                  Task_List_Nodes := Child_Nodes (N);
                  HTML.Comment (Length (Task_List_Nodes)'Img);
                  for K in 1 .. Length (Task_List_Nodes) loop
                     HTML.Comment (Name (Item (Task_List_Nodes, K - 1)));
                     if Name (Item (Task_List_Nodes, K - 1)) = "pe_tasks" or else
                        Name (Item (Task_List_Nodes, K - 1)) = "element" then
                        PE_Task_Nodes := Child_Nodes (Item (Task_List_Nodes, K - 1));
                        HTML.Comment (Length (PE_Task_Nodes)'Img);
                        for L in 1 .. Length (PE_Task_Nodes) loop
                           PE_Task_Entry := Item (PE_Task_Nodes, L - 1);
                           if Name (PE_Task_Entry) = "PET_granted_destin_identifier_list" then
                              Element_Node := Item (Child_Nodes (PE_Task_Entry), 1);
                              if Name (Element_Node) /= "element" then
                                 raise Assumption_Error;
                              end if;
                              JG_Nodes := Child_Nodes (Element_Node);
                              HTML.Comment (Length (JG_Nodes)'Img);
                              for M in 1 .. Length (JG_Nodes) loop
                                 JG_Entry := Item (JG_Nodes, M - 1);
                                 HTML.Comment (Name (JG_Entry));
                                 if Name (JG_Entry) = "JG_qname" then
                                    HTML.Comment (Length (Child_Nodes (JG_Entry))'Img);
                                    J.Task_List.Append (To_Unbounded_String (Value (First_Child (JG_Entry))));
                                 end if;
                              end loop;
                           end if;
                        end loop;
                     end if;
                  end loop;
               end if;
            end loop;
         end if;
      end loop;
   exception
      when E : others =>
         HTML.Error ("Failed to parse job tasks: " & Exception_Message (E));
   end Extract_Tasks;

   -------------------
   -- Extract_Paths --
   -------------------

   procedure Extract_Paths (Path_List  : in out String_Lists.List;
                            List_Nodes  : Node_List) is
      List_Node, N : Node;
      Path_Nodes : Node_List;
   begin
      for I in 1 .. Length (List_Nodes) loop
         List_Node := Item (List_Nodes, I - 1);
         if Name (List_Node) = "path_list" then
            Path_Nodes := Child_Nodes (List_Node);
            for I in 1 .. Length (Path_Nodes) loop
               N := Item (Path_Nodes, I - 1);
               if Name (N) = "PN_path" then
                  Path_List.Append (To_Unbounded_String (Value (First_Child (N))));
               end if;
            end loop;
         end if;
      end loop;
   end Extract_Paths;


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
      elsif Field = "Ends In" or else
        Field = "Ends At" then
         Sorting_By_End.Sort (Job_List);
      else
         HTML.Error ("Sorting by " & Field & " unimplemented");
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
      elsif Left.Slot_Number < Right.Slot_Number then
         return True;
      elsif Left.Slot_Number > Right.Slot_Number then
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
      return Integer'Value (To_String (Left.Slot_Number)) < Integer'Value (To_String (Right.Slot_Number));
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

   ---------------------
   -- Precedes_By_End --
   ---------------------

   function Precedes_By_End (Left, Right : Job) return Boolean is
   begin
      return End_Time (Left) < End_Time (Right);
   end Precedes_By_End;


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

   procedure Put  (Cursor : Job_Lists.Cursor) is
      Res        : Resource_Lists.Cursor;
      Slot_Range : Slot_Lists.Cursor;
      Q, Msg     : String_Lists.Cursor;
      J          : Job := Job_Lists.Element (Cursor);

      procedure Put_Name is
      begin
         HTML.Begin_Div (Class => "job_name");
         HTML.Put_Paragraph ("Name", J.Name);
         Msg := J.Message_List.First;
         loop
            exit when Msg = String_Lists.No_Element;
            Ada.Text_IO.Put_Line ("<p class=""message"">"
                               & To_String (String_Lists.Element (Msg))
                               & "</p>");
            Msg := Next (Msg);
         end loop;
         HTML.End_Div (Class => "job_name");
      end Put_Name;

      procedure Put_Meta is
      begin
         HTML.Begin_Div (Class => "job_meta");
         HTML.Put_Paragraph ("ID", J.Number'Img);
         HTML.Put_Paragraph ("Owner", J.Owner);
         HTML.Put_Paragraph ("Group", J.Group);
         HTML.Put_Paragraph ("Account", J.Account);
         HTML.Put_Paragraph ("Array", J.Job_Array);
         Ada.Text_IO.Put ("<p>Reserve: ");
         HTML.Put (J.Reserve);
         Ada.Text_IO.Put_Line ("</p>");
         HTML.Put_Clearer;
         HTML.End_Div (Class => "job_meta");
      end Put_Meta;

      procedure Put_Queues is
      begin
         HTML.Begin_Div (Class => "job_queue");
         HTML.Put_Heading (Title => "Requested",
                            Level => 3);
         Q := J.Queue_List.First;
         loop
            exit when Q = String_Lists.No_Element;
            HTML.Put_Paragraph (Label    => "Queue",
                             Contents => String_Lists.Element (Q));
            Next (Q);
         end loop;

         HTML.Put_Paragraph ("PE", J.PE);
         Slot_Range := J.Slot_List.First;
         loop
            exit when Slot_Range = Slots.Slot_Lists.No_Element;
            Slots.Put (Slots.Slot_Lists.Element (Slot_Range));
            Next (Slot_Range);
         end loop;

         HTML.Put_Heading (Title => "Assigned",
                            Level => 3);
         Ada.Text_IO.Put ("<ul>");
         Q := J.Task_List.First;
         loop
            exit when Q = String_Lists.No_Element;
            Ada.Text_IO.Put_Line ("<li>" & To_String (String_Lists.Element (Q)) & "</li>");
            Next (Q);
         end loop;
         Ada.Text_IO.Put ("</ul>");
         HTML.Put_Clearer;
         HTML.End_Div (Class => "job_queue");
      end Put_Queues;

      procedure Put_Resources is
      begin
         HTML.Begin_Div (Class => "job_resources");
         Res := J.Hard.First;
         loop
            exit when Res = Resources.Resource_Lists.No_Element;
            Resources.Put (Resources.Resource_Lists.Element (Res));
            Res := Next (Res);
         end loop;
         Res := J.Soft.First;
         loop
            exit when Res = Resources.Resource_Lists.No_Element;
            Resources.Put (Resources.Resource_Lists.Element (Res));
            Res := Next (Res);
         end loop;
         HTML.End_Div (Class => "job_resources");
      end Put_Resources;

      procedure Put_Files is
      begin
            HTML.Begin_Div (Class => "job_files");
         HTML.Put_Paragraph ("Directory", J.Directory);
         HTML.Put_Paragraph ("Script", J.Script_File);
         HTML.Put_Paragraph ("Executable", J.Exec_File);
         Ada.Text_IO.Put ("<p>Merge StdErr: ");
         HTML.Put (J.Merge_Std_Err);
         Ada.Text_IO.Put_Line ("</p>");
         HTML.Put_Heading (Title => "StdOut",
                        Level => 3);
         Q := J.Std_Out_Paths.First;
         Ada.Text_IO.Put ("<ul>");
         loop
            exit when Q = String_Lists.No_Element;
            Ada.Text_IO.Put_Line ("<li>" & To_String (String_Lists.Element (Q)) & "</li>");
            Next (Q);
         end loop;
         Ada.Text_IO.Put ("</ul>");

         HTML.Put_Heading (Title => "StdErr",
                        Level => 3);
         Q := J.Std_Err_Paths.First;
         Ada.Text_IO.Put ("<ul>");
         loop
            exit when Q = String_Lists.No_Element;
            Ada.Text_IO.Put_Line ("<li>" & To_String (String_Lists.Element (Q)) & "</li>");
            Next (Q);
         end loop;
         Ada.Text_IO.Put ("</ul>");
         Ada.Text_IO.Put ("<p>Notify: ");
         HTML.Put (J.Notify);
         Ada.Text_IO.Put_Line ("</p>");
         HTML.End_Div (Class => "job_files");
      end Put_Files;

   begin
      HTML.Begin_Div (Class => "job_info");

      Put_Name;
      Put_Meta;
      Put_Queues;
      Put_Resources;
      Put_Files;

      HTML.Put_Clearer;
      HTML.End_Div (Class => "job_info");
      HTML.Put_Clearer;
   end Put;

end Jobs;
