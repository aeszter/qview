with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Calendar;   use Ada.Calendar;
with GNAT.Calendar.Time_IO;
with Ada.Calendar.Conversions;
with Resources;      use Resources; use Resources.Resource_Lists;
with Ranges;          use Ranges; use Ranges.Range_Lists;
with Utils;          use Utils; use Utils.String_Lists;
with Jobs;
with HTML;
with Parser;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Interfaces.C;
with Ada.Containers; use Ada.Containers;
with Ada.Calendar.Formatting;

package body Jobs is
   use Job_Lists;

   procedure Parse_JAT_Message_List (Message_List : Node; J : in out Job);
   procedure Put_State (State : Job_State);

   procedure Parse_JAT_Task_List
     (J             : in out Job;
      Task_List             : Node);

   procedure Parse_PET_Destinations
     (J             : in out Job;
      PE_Task_Entry : Node);

   procedure Parse_Usage
     (Usage_Data : in out Usage;
      Usage_Tree : Node;
      Cumulative : Boolean);

   -------------
   --  accessors
   -------------

   function Get_Task_Count (J : Job) return Natural is
   begin
      return Count (J.Task_IDs);
   end Get_Task_Count;

   function Get_ID (J : Job) return String is
   begin
      return J.Number'Img;
   end Get_ID;

   function Get_PE (J : Job) return Unbounded_String is
   begin
      return J.PE;
   end Get_PE;

   function Get_Slot_List (J : Job) return Ranges.Range_Lists.List is
   begin
      return J.Slot_List;
   end Get_Slot_List;

   function Get_Slot_Number (J : Job) return Unbounded_String is
   begin
      return J.Slot_Number;
   end Get_Slot_Number;

   function Get_Queue (J : Job) return Unbounded_String is
   begin
      return J.Queue;
   end Get_Queue;

   function Get_Hard_Resources (J : Job) return Resources.Hashed_List is
   begin
      return J.Hard;
   end Get_Hard_Resources;

   function Get_Soft_Resources (J : Job) return Resources.Hashed_List is
   begin
      return J.Soft;
   end Get_Soft_Resources;

   ----------------
   --  list-related
   ----------------

   function Find_Job (ID : Natural) return Job_Lists.Cursor is
      Pos : Job_Lists.Cursor := List.First;
      The_Job : Job;
   begin
      while Pos /= Job_Lists.No_Element loop
         The_Job := Job_Lists.Element (Pos);
         if The_Job.Number = ID then
            return Pos;
         end if;
         Next (Pos);
      end loop;
      return Job_Lists.No_Element;
   end Find_Job;

   procedure Sort is
   begin
      Sorting_By_Resources.Sort (List);
   end Sort;

   procedure Rewind is
   begin
      List_Cursor := List.First;
   end Rewind;

   function Empty return Boolean is
   begin
      return List.Is_Empty;
   end Empty;

   function Next return Job is
   begin
      Next (List_Cursor);
      return Job_Lists.Element (List_Cursor);
   end Next;

   function At_End return Boolean is
   begin
      if List_Cursor = Job_Lists.No_Element or else
        List_Cursor = List.Last then
         return True;
      end if;
      return False;
   end At_End;

   function Current return Job is
   begin
      return Job_Lists.Element (List_Cursor);
   end Current;


   -----------------
   -- Get_Summary --
   -----------------

   procedure Get_Summary (Tasks, Slots : out State_Count) is

      procedure Count (Position : Job_Lists.Cursor) is
         State : Job_State := Job_Lists.Element (Position).State;
         Slot_Number : Natural := Integer'Value (To_String (Job_Lists.Element (Position).Slot_Number));
      begin
         Tasks (State) := Tasks (State) + 1;
         Slots (State) := Slots (State) + Slot_Number;
      end Count;

   begin
      for S in Tasks'Range loop
         Tasks (S) := 0;
         Slots (S) := 0;
      end loop;
      List.Iterate (Process => Count'Access);
   end Get_Summary;

   procedure Put_Summary is
      Task_Summary, Slot_Summary : State_Count;
   begin
      Jobs.Get_Summary (Tasks => Task_Summary,
                        Slots => Slot_Summary);
      HTML.Begin_Div (ID => "job_summary");
      Ada.Text_IO.Put ("<ul>");
      for State in Task_Summary'Range loop
         if State /= unknown then
            Ada.Text_IO.Put ("<li>");
            Ada.Text_IO.Put (Task_Summary (State)'Img);
            if Slot_Summary (State) > 0 then
               Ada.Text_IO.Put ("(" & Ada.Strings.Fixed.Trim (Slot_Summary (State)'Img, Ada.Strings.Left) & ")");
            end if;
            Ada.Text_IO.Put (" ");
            Put_State (State => State);
            Ada.Text_IO.Put_Line ("</li>");
         end if;
      end loop;

      Ada.Text_IO.Put ("</ul>");
      HTML.End_Div (ID => "job_summary");
   end Put_Summary;

   procedure Put_State (State : Job_State) is
      S : String := To_String (State);
   begin
      Ada.Text_IO.Put ("<img src=""/icons/" & S & ".png"" ");
      Ada.Text_IO.Put ("alt=""" & S & """ title=""" & S & ": ");
      case State is
         when r =>
            Ada.Text_IO.Put ("running");
         when Rr =>
            Ada.Text_IO.Put ("restarted");
         when t =>
            Ada.Text_IO.Put ("in transit");
         when Rq =>
            Ada.Text_IO.Put ("requeued");
         when Eqw =>
            Ada.Text_IO.Put ("error");
         when hqw =>
            Ada.Text_IO.Put ("on hold");
         when qw =>
            Ada.Text_IO.Put ("waiting");
         when dr =>
            Ada.Text_IO.Put ("running, deletion pending");
         when dt =>
            Ada.Text_IO.Put ("in transit, deletion pending");
         when ERq =>
            Ada.Text_IO.Put ("requeued, with error");
         when unknown =>
            null;
      end case;
      Ada.Text_IO.Put (""" />");
   end Put_State;

   ---------------------
   -- State_As_String --
   ---------------------

   function State_As_String (J : Job) return String is
   begin
      return To_String (State => J.State);
   end State_As_String;

   function To_String (State : Job_State) return String is
   begin
      case State is
         when dt => return "dt";
         when dr => return "dr";
         when Eqw => return "Eqw";
         when t => return "t";
         when r => return "r";
         when Rr => return "Rr";
         when Rq => return "Rq";
         when qw => return "qw";
         when hqw => return "hqw";
            when ERq => return "ERq";
         when unknown => return "unknown";
      end case;
   end To_String;

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
      elsif State = "ERq" then
         return ERq;
      else
         return unknown;
      end if;
   end To_State;

   function To_Memory (Amount : Usage_Integer) return String is
   begin
      if Amount > 2 ** 34 then
         return Usage_Integer'Image (Amount / 2 ** 30) & " GB";
      elsif Amount > 2 ** 24 then
         return Usage_Integer'Image (Amount / 2 ** 20) & " MB";
      else
         return Usage_Integer'Image (Amount / 2** 10) & " kB";
      end if;
   end To_Memory;


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
        + Ada.Real_Time.To_Duration (Ada.Real_Time.Seconds (
        J.Hard.Numerical ("h_rt")));
   exception
      when Constraint_Error =>
         raise Resource_Error with "Unable to compute end time";
   end End_Time;

   --------------------
   -- Remaining_Time --
   --------------------

   function Remaining_Time (J : Job) return Duration is
   begin
      return End_Time (J) - Ada.Calendar.Clock;
   end Remaining_Time;


   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Nodes : Node_List) is
      N : Node;
   begin
      for Index in 1 .. Length (Nodes) loop
         N := Item (Nodes, Index - 1);
         if Name (N) /= "#text" then
            List.Append (New_Job (Child_Nodes (N)));
         end if;
      end loop;
   exception
      when E : others
         => HTML.Error ("Unable to read job info: " & Exception_Message (E));
   end Append_List;

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Nodes                    : Node_List;
                          PE, Queue, Hard_Requests,
                          Soft_Requests,
                          Slot_Number, Slot_Ranges : Unbounded_String) is
            N : Node;
            J : Job;
   begin
      for Index in 1 .. Length (Nodes) loop
         N := Item (Nodes, Index - 1);
         if Name (N) /= "#text" then
            J := New_Job (Child_Nodes (N));
            if J.PE /= PE then
               HTML.Comment (J.Number'Img & ":" & J.PE & " /= " & PE);
            elsif J.Queue /= Queue then
               HTML.Comment (J.Number'Img & ":" & J.Queue & " /= " & Queue);
            elsif J.Hard.Hash /= Hard_Requests then
               HTML.Comment (J.Number'Img & ":" & J.Hard.To_String & "(" & J.Hard.Hash & ")"
                             & " /= " & Hard_Requests);
            elsif J.Soft.Hash /= Soft_Requests then
               HTML.Comment (J.Number'Img & ":" & J.Soft.To_String & "(" & J.Soft.Hash & ")"
                             & " /= " & Soft_Requests);
            else -- all equal
               Update_Job_From_Qstat_J (J);
               if Hash_Type'Value (To_String (Slot_Ranges)) = 0 then
                  --  checking against a string (i.e. " 0") would be too brittle,
                  --  since any change in leading blanks would break this code
                  if J.Slot_Number = Slot_Number then
                     List.Append (J);
                  else
                     HTML.Comment (J.Number'Img & ":" & J.Slot_Number & " /= "
                                     & Slot_Number);
                  end if;
               else
                  if Hash (J.Slot_List) = Slot_Ranges then
                     List.Append (J);
                  else
                     HTML.Comment (J.Number'Img & ":" & Hash (J.Slot_List) & " /= "
                                     & Slot_Ranges);
                  end if;
               end if;
            end if;
         end if;
      end loop;
   exception
      when E : others
         => HTML.Error ("Unable to read job info: " & Exception_Message (E));
   end Append_List;

   ------------------------------
   -- Update_List_From_Qstat_J --
   ------------------------------

   procedure Update_List_From_Qstat_J is
      Pos : Job_Lists.Cursor;
   begin
      Pos := List.First;
      while Pos /= Job_Lists.No_Element loop
         List.Update_Element (Position => Pos,
                              Process  => Update_Job_From_Qstat_J'Access);
         Next (Pos);
      end loop;
   end Update_List_From_Qstat_J;

   -----------------------------
   -- Update_Job_From_Qstat_J --
   -----------------------------

   procedure Update_Job_From_Qstat_J (J : in out Job) is
      SGE_Out : Parser.Tree;
      Nodes   : Node_List;
      N       : Node;
   begin
      SGE_Out := Parser.Setup (Selector => "-j " & J.Number'Img);

      --  Fetch Jobs
      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "djob_info");

      for Index in 1 .. Length (Nodes) loop
         N := Item (Nodes, Index - 1);
         if Name (N) = "djob_info" then
            N := Item (Child_Nodes (N), 1);
            if Name (N) /= "element" then
               raise Assumption_Error with "Expected ""element"" Found """
                 & Name (N) & """";
            end if;
         end if;
         Update_Job (J, Child_Nodes (N));
      end loop;
      Parser.Free;
   exception
      when E : others
         => HTML.Error ("Unable to read job info (" & J.Number'Img & "):"
                        & Exception_Message (E));
               Parser.Free;
   end Update_Job_From_Qstat_J;

   -------------------
   -- Update_Status --
   -------------------

   procedure Update_Status is
      Pos : Job_Lists.Cursor;
   begin
      Pos := List.First;
      while Pos /= Job_Lists.No_Element loop
         List.Update_Element (Position => Pos,
                              Process  => Update_Status'Access);
         Next (Pos);
      end loop;
   end Update_Status;

   procedure Update_Status (J : in out Job) is
      SGE_Out     : Tree;
      Nodes       : Node_List;
      Field_Nodes : Node_List;
      Field       : Node;
      Number      : Positive;
      State       : Job_State;
   begin
      SGE_Out := Parser.Setup (Selector => "-u " & To_String (J.Owner));

      --  Fetch Jobs
      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "job_list");

      Jobs :
      for Index in 1 .. Length (Nodes) loop
         Field_Nodes := Child_Nodes (Item (Nodes, Index - 1));
         Fields :
         for Field_Index in 1 .. Length (Field_Nodes) loop
            Field := Item (Field_Nodes, Field_Index - 1);
            if Name (Field) = "JB_job_number" then
               Number := Integer'Value (Value (First_Child (Field)));
            elsif Name (Field) = "state" then
               State := To_State (Value (First_Child (Field)));
            end if;
         end loop Fields;
         if Number = J.Number then
            J.State := State;
            exit Jobs;
         end if;
      end loop Jobs;

      Parser.Free;
   exception
      when E : others
         => HTML.Error ("Unable to read job status (" & J.Number'Img & "):"
                        & Exception_Message (E));

      Parser.Free;
   end Update_Status;

   -------------------
   -- Search_Queues --
   -------------------

   procedure Search_Queues is
      SGE_Out                : Tree;
      Job_Nodes, Value_Nodes : Node_List;
      Job_Node, Value_Node   : Node;
      A                      : Attr;
      Pos                    : Job_Lists.Cursor;
      Number_Found           : Natural;
      The_Queue              : Unbounded_String;

      procedure Record_Queue (Element : in out Job) is
         Success : Boolean;
         Where : String_Sets.Cursor;
      begin
         Element.Detected_Queues.Insert (New_Item => The_Queue,
                                         Position => Where,
                                         Inserted => Success);
      end Record_Queue;

   begin
      SGE_Out := Parser.Setup (Command  => "qhost",
                               Selector => "-j");

      --  Fetch Jobs
      Job_Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "job");
      for I in 0 .. Length (Job_Nodes) - 1 loop
         Job_Node := Item (List  => Job_Nodes,
                           Index => I);
         A := Get_Attr (Job_Node, "name");
         Number_Found := Integer'Value (Value (A));
         Pos := Find_Job (ID => Number_Found);
         --  in theory, this scaling is less than ideal:
         --  O(number of jobs in Job_List times number of jobs returned by qhost)
         --  however, in the present implementation, there is only one job in the list,
         --  so this should not be a problem
         --  if we ever change the code to produce more than one or two jobs at this
         --  point, we should change the job list to a map with the ID as a key
         if Pos /= Job_Lists.No_Element then
            Value_Nodes := Child_Nodes (Job_Node);
            for J in 0 .. Length (Value_Nodes) - 1 loop
               Value_Node := Item (List  => Value_Nodes,
                                   Index => J);
               if Name (Value_Node) = "jobvalue" then
                  A := Get_Attr (Value_Node, "name");
                  if Value (A) = "qinstance_name" then
                     HTML.Comment ("Detected " & The_Queue);
                     The_Queue := To_Unbounded_String (Value (First_Child (Value_Node)));
                  elsif Value (A) = "taskid" then
                     HTML.Comment ("taskid " & Value (First_Child (Value_Node)));
                  end if;
               end if;
            end loop;
            List.Update_Element (Position => Pos,
                                 Process  => Record_Queue'Access);
         end if;
      end loop;
      Parser.Free;
   exception
      when E : others => HTML.Error ("Error while searching for queues: "
                                       & Exception_Message (E));
      Parser.Free;
   end Search_Queues;

   -------------
   -- New_Job --
   -------------

   function New_Job (List : Node_List) return Job is
      J           : Job;
   begin
      J.Merge_Std_Err := Undecided;
      J.Reserve := Undecided;
      J.State := unknown;
      J.Mem := 0.0;
      J.IO := 0.0;
      J.CPU := 0.0;
      Update_Job (J => J, List => List);
      return J;
   end New_Job;

   ----------------
   -- Update_Job --
   ----------------

   procedure Update_Job (J : in out Job; List : Node_List) is
      C           : Node;
      A           : Attr;
      Time_Buffer : String (1 .. 19);
      Inserted    : Boolean;
      Inserted_At : Resource_Lists.Cursor;
   begin
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
            J.Posix_Priority := Posix_Priority_Type'Value (Value (First_Child (C)));
         elsif Name (C) = "hard_req_queue" then
            J.Queue := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "full_job_name" then
            null; -- ignore
         elsif Name (C) = "requested_pe" then
            A := Get_Attr (C, "name");
            J.PE := To_Unbounded_String (Value (A));
         elsif Name (C) = "hard_request" then
            A := Get_Attr (C, "name");
            J.Hard.Insert (Key      => To_Unbounded_String (Value (A)),
                           New_Item => New_Resource (Name  => Value (A),
                                                     Value => Value (First_Child (C))),
                           Position => Inserted_At,
                           Inserted => Inserted);
         elsif Name (C) = "soft_request" then
            A := Get_Attr (C, "name");
            J.Soft.Insert (Key      => To_Unbounded_String (Value (A)),
                           New_Item => New_Resource (Name  => Value (A),
                                                     Value => Value (First_Child (C))),
                           Position => Inserted_At,
                           Inserted => Inserted);
         elsif Name (C) = "predecessor_jobs" or else
            Name (C) = "ad_predecessor_jobs" then
            J.Predecessors.Include (New_Item => Natural'Value (Value (First_Child (C))));
         elsif Name (C) = "predecessor_jobs_req" or else
           Name (C) = "ad_predecessor_jobs_req" then
            J.Predecessor_Request.Append (To_Unbounded_String (Value (First_Child (C))));
         elsif Name (C) = "JB_hard_resource_list" then
            Extract_Resource_List (J, Child_Nodes (C));
         elsif Name (C) = "JB_soft_resource_list" then
            Extract_Resource_List (J, Child_Nodes (C), Soft => True);
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
         elsif Name (C) = "JB_job_args" then
            Extract_Args (J, Child_Nodes (C));
         elsif Name (C) = "tasks" then
            J.Task_IDs := To_Step_Range_List (Value (First_Child (C)));
         elsif Name (C) = "granted_pe" then
            null;
         elsif Name (C) = "JB_jid_predecessor_list" then
            Extract_Hold_ID_List (J.Predecessors, Child_Nodes (C));
         elsif Name (C) = "JB_jid_successor_list" then
            Extract_Hold_ID_List (J.Successors, Child_Nodes (C));
         elsif Name (C) = "JB_urg" or else
           Name (C) = "JB_dlcontr" or else
           Name (C) = "JAT_ntix" or else
           Name (C) = "JAT_share" or else
           Name (C) = "JB_jobshare" or else
           Name (C) = "JB_jid_request_list" or else
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

   exception
      when E : others =>
         HTML.Error ("Failed to parse job: " & Exception_Message (E));
         HTML.Error ("Node type: """ & Name (C)
                     & """ Value: """ & Value (First_Child (C)) & """");

   end Update_Job;

   ---------------------------
   -- Extract_Resource_List --
   ---------------------------

   procedure Extract_Resource_List (J              : in out Job;
                                    Resource_Nodes : Node_List;
                                    Soft : Boolean := False) is
      Resource_Tags      : Node_List;
      N, R               : Node;
      Res_Value          : Unbounded_String;
      Res_Name           : Unbounded_String;
      Res_Bool           : Boolean;
      Res_State          : Tri_State;
      Inserted           : Boolean;
      Inserted_At        : Resource_Lists.Cursor;

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
               if Res_Value = "TRUE" or else
                 Res_Value = "true" or else
                 Res_Value = "1" then
                  Res_State := True;
               elsif Res_Value = "FALSE" or else
                 Res_Value = "false" or else
                  Res_Value = "0" then
                  Res_State := False;
               else
                  raise Constraint_Error
                    with  """" & To_String (Res_Value) & """ is not boolean";
               end if;
            end if;
            if Soft then
               J.Soft.Insert (Key      => Res_Name,
                           New_Item => New_Resource (Name  => To_String (Res_Name),
                                                     Value => Res_Value,
                                                     Boolean_Valued => Res_Bool,
                                                     State => Res_State),
                           Position => Inserted_At,
                           Inserted => Inserted);
            else
               J.Hard.Insert (Key      => Res_Name,
                           New_Item => New_Resource (Name  => To_String (Res_Name),
                                                     Value => Res_Value,
                                                     Boolean_Valued => Res_Bool,
                                                     State => Res_State),
                           Position => Inserted_At,
                           Inserted => Inserted);
            end if;
         end if;
      end loop;
   end Extract_Resource_List;

   ------------------
   -- Extract_Args --
   ------------------

   procedure Extract_Args (J : in out Job;
                           Arg_Nodes : Node_List) is
      N, ST : Node;
      Sub_Elements : Node_List;
   begin
      for I in 1 .. Length (Arg_Nodes) loop
         HTML.Comment ("Arg " & I'Img);
         N := Item (Arg_Nodes, I - 1);
         if Name (N) = "element" then
            HTML.Comment ("element");
            Sub_Elements := Child_Nodes (N);
            if Length (Sub_Elements) < 2 then
               raise Assumption_Error with "too few sub-elements";
            end if;
            ST := Item (Sub_Elements, 1);
            if Name (ST) /= "ST_name" then
               raise Assumption_Error with "Expected ""ST_name"" but found """
                 & Name (ST) & """ instead";
            else
               HTML.Comment ("ST_name");
               J.Args.Append (New_Item => To_Unbounded_String (Value (First_Child (ST))));
            end if;
         end if;

      end loop;
   end Extract_Args;

   ------------------------------
   -- Extract_Predecessor_List --
   ------------------------------

   procedure Extract_Hold_ID_List (ID_List         : in out Utils.ID_List;
                                   Sub_Nodes       : Node_List) is
      N, ID_Node : Node;
   begin
      for I in 0 .. Length (Sub_Nodes) - 1 loop
         N := Item (Sub_Nodes, I);
         if Name (N) = "job_predecessors" or else
            Name (N) = "ulong_sublist" then
            ID_Node := Item (Child_Nodes (N), 1);
            if Name (ID_Node) /= "JRE_job_number" then
               raise Assumption_Error with
                 "Expected ""JRE_job_number"" but found """
                 & Name (ID_Node) & """ instead";
            end if;
            ID_List.Include (New_Item => Natural'Value (Value (First_Child (ID_Node))));
         end if;
      end loop;
   end Extract_Hold_ID_List;

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
                  if J.Queue = Null_Unbounded_String then
                     J.Queue := To_Unbounded_String (Value (First_Child (Q)));
                  end if;
               end if;
            end loop;
         end if;
      end loop;
   end Extract_Queue_List;

   ----------------------
   -- Extract_PE_Range --
   ----------------------

   procedure Extract_PE_Range (J : in out Job; Children : Node_List) is
      Range_Nodes                      : Node_List;
      N, R                             : Node;
      Slots_Min, Slots_Step, Slots_Max : Natural;
   begin
      for I in 1 .. Length (Children) loop
         N := Item (Children, I - 1);
         if Name (N) = "ranges" then
            Range_Nodes := Child_Nodes (N);
            for J in 1 .. Length (Range_Nodes) loop
               R := Item (Range_Nodes, J - 1);
               if Name (R) = "RN_min" then
                  Slots_Min := Integer'Value (Value (First_Child (R)));
               elsif Name (R) = "RN_max" then
                  Slots_Max := Integer'Value (Value (First_Child (R)));
               elsif Name (R) = "RN_step" then
                  Slots_Step := Integer'Value (Value (First_Child (R)));
               end if;
            end loop;
            J.Slot_List.Append (Ranges.New_Range (Min  => Slots_Min,
                                                       Max  => Slots_Max,
                                                       Step => Slots_Step));
         end if;
      end loop;
   end Extract_PE_Range;

   procedure Parse_Usage
     (Usage_Data : in out Usage;
      Usage_Tree : Node;
      Cumulative : Boolean)
   is
      Usage_Entries, Quantity_Nodes : Node_List;
      Usage_Entry, Quantity_Field   : Node;
      Usage_Kind                    : Usage_Type;
      Usage_Value                   : Usage_Number;
   begin
      Usage_Entries := Child_Nodes (Usage_Tree);
      Over_Usage_Entries :
      for I in 0 .. Length (Usage_Entries) - 1 loop
         Usage_Entry := Item (Usage_Entries, I);
         if Name (Usage_Entry) = "element" or else
         Name (Usage_Entry) = "scaled" then
            Quantity_Nodes := Child_Nodes (Usage_Entry);
            begin
               Over_Quantity_Nodes :
               for K in 0 .. Length (Quantity_Nodes) - 1 loop
                  Quantity_Field := Item (Quantity_Nodes, K);
                  if Name (Quantity_Field) = "UA_name" then
                     Usage_Kind := Usage_Type'Value (Value (First_Child (Quantity_Field)));
                  elsif Name (Quantity_Field) = "UA_value" then
                     Usage_Value := Usage_Number'Value (Value (First_Child (Quantity_Field)));
                  end if;
               end loop Over_Quantity_Nodes;
               if Cumulative then
                  Usage_Data (Usage_Kind) := Usage_Data (Usage_Kind) + Usage_Value;
               else
                  Usage_Data (Usage_Kind) := Usage_Value;
               end if;
               exception
               when E : Constraint_Error =>
                  declare
                     Quantity : String := Value (First_Child (Quantity_Field));
                  begin
                     if Quantity'Length >= 13
                       and then Quantity (Quantity'First .. Quantity'First + 12) = "binding_inuse" then
                        null; -- ignore; binding has nothing to do with usage, and
                              --  the format is utterly insane;
                              --  there is no way we can handle this crappy xml without
                              --  writing equally crappy code, and it is not that important
                     else
                        HTML.Error ("Unable to parse usage (QF => """
                        & Quantity & """): "
                        & Exception_Message (E));
                     end if;
                  end;
            end;

         end if;
      end loop Over_Usage_Entries;
   end Parse_Usage;

   ----------------------------
   -- Parse_PET_Destinations --
   ----------------------------

   procedure Parse_PET_Destinations
     (J             : in out Job;
      PE_Task_Entry : Node)
   is
      JG_Nodes : Node_List;
      JG_Entry : Node;
      Element_Node : Node;
   begin
      Element_Node := Item (Child_Nodes (PE_Task_Entry), 1);
      if Name (Element_Node) /= "element" then
         raise Assumption_Error;
      end if;
      JG_Nodes := Child_Nodes (Element_Node);
      HTML.Comment (Length (JG_Nodes)'Img);
      Over_JG_Nodes :
      for M in 1 .. Length (JG_Nodes) loop
         JG_Entry := Item (JG_Nodes, M - 1);
         HTML.Comment (Name (JG_Entry));
         if Name (JG_Entry) = "JG_qname" then
            HTML.Comment (Length (Child_Nodes (JG_Entry))'Img);
            J.Task_List.Append (To_Unbounded_String (Value (First_Child (JG_Entry))));
         end if;
      end loop Over_JG_Nodes;
   end Parse_PET_Destinations;

   -------------------------
   -- Parse_JAT_Task_List --
   -------------------------

   procedure Parse_JAT_Task_List
     (J             : in out Job;
      Task_List             : Node)
   is
      Task_List_Nodes : Node_List;
      PE_Task_Nodes   : Node_List;
      PE_Task_Entry   : Node;
   begin
      Task_List_Nodes := Child_Nodes (Task_List);
      Over_Task_List_Nodes :
      for K in 1 .. Length (Task_List_Nodes) loop
         if Name (Item (Task_List_Nodes, K - 1)) = "pe_tasks" or else
           Name (Item (Task_List_Nodes, K - 1)) = "element" then
            PE_Task_Nodes := Child_Nodes (Item (Task_List_Nodes, K - 1));
            Over_PE_Task_Nodes :
            for L in 1 .. Length (PE_Task_Nodes) loop
               PE_Task_Entry := Item (PE_Task_Nodes, L - 1);
               if Name (PE_Task_Entry) = "PET_granted_destin_identifier_list" then
                  Parse_PET_Destinations (J, PE_Task_Entry);
               elsif Name (PE_Task_Entry) = "PET_usage" then
                  Parse_Usage (Usage_Data => J.PET_Usage, Usage_Tree => PE_Task_Entry, Cumulative => True);
               end if;
            end loop Over_PE_Task_Nodes;
         end if;
      end loop Over_Task_List_Nodes;
   end Parse_JAT_Task_List;

   ----------------------------
   -- Parse_JAT_Message_List --
   ----------------------------

   procedure Parse_JAT_Message_List (Message_List : Node; J : in out Job) is
      Messages  : Node_List;
      Sublist, M : Node;
   begin
      Sublist := Item (Child_Nodes (Message_List), 1);
      if Name (Sublist) /= "ulong_sublist" then
         raise Assumption_Error;
      end if;
      Messages := Child_Nodes (Sublist);
      Over_Messages :
      for K in 1 .. Length (Messages) loop
         M := Item (Messages, K - 1);
         if Name (M) = "QIM_message" then
            J.Message_List.Append (To_Unbounded_String (Value (First_Child (M))));
         end if;
      end loop Over_Messages;
   end Parse_JAT_Message_List;

   -------------------
   -- Extract_Tasks --
   -------------------

   procedure Extract_Tasks (J : in out Job; Task_Nodes : Node_List) is
      Children      : Node_List;
      N             : Node;
      JA_Tasks      : Node;
   begin
      Over_Task_Nodes :
      for H in 1 .. Length (Task_Nodes) loop
         JA_Tasks := Item (Task_Nodes, H - 1);
         if Name (JA_Tasks) = "ja_tasks"
           or else Name (JA_Tasks) = "ulong_sublist" then
            Children := Child_Nodes (JA_Tasks);
            Task_Entries :
            for I in 1 .. Length (Children) loop
               N := Item (Children, I - 1);
               if Name (N) = "JAT_message_list" then
                  Parse_JAT_Message_List (Message_List => N, J => J);
               elsif Name (N) = "JAT_task_list" then
                  Parse_JAT_Task_List (J, N);
               elsif Name (N) = "JAT_scaled_usage_list" then
                  Parse_Usage (Usage_Data => J.JAT_Usage, Usage_Tree => N, Cumulative => False);
               end if;
            end loop Task_Entries;
         end if;
      end loop Over_Task_Nodes;
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


   ----------------
   -- Prune_List --
   ----------------


   procedure Prune_List_By_Slots (Slots : String) is
      Temp : Job_Lists.List;
      Pos  : Job_Lists.Cursor := List.First;
      J    : Job;

   begin
      loop
         exit when Pos = Job_Lists.No_Element;
         J := Job_Lists.Element (Pos);
         if Hash (J.Slot_List) = Slots then
            Temp.Append (J);
         else
            HTML.Comment (J.Number'Img);
            if Hash (J.Slot_List) /= Slots then
               HTML.Comment (J.Number'Img & ": " & Hash (J.Slot_List) & " /= " & Slots);
            end if;
         end if;
         Next (Pos);
      end loop;
      HTML.Comment (Temp.Length'Img);
      List := Temp;
   end Prune_List_By_Slots;

   ------------------
   -- Sort_By      --
   --  Purpose:  Sort the job list by any column/field
   --  Parameter Field: Title of the column to sort by
   ------------------

   procedure Sort_By (Field : String; Direction : String) is
   begin
      if Field = "Number" then
         Sorting_By_Number.Sort (List);
      elsif Field = "Name" then
         Sorting_By_Name.Sort (List);
      elsif Field = "Owner" then
         Sorting_By_Owner.Sort (List);
      elsif Field = "Priority" then
         Sorting_By_Priority.Sort (List);
      elsif Field = "Submitted" then
         Sorting_By_Submission_Time.Sort (List);
      elsif Field = "Slots" then
         Sorting_By_Slots.Sort (List);
      elsif Field = "State" then
         Sorting_By_State.Sort (List);
      elsif Field = "CPU" then
         Sorting_By_CPU_Used.Sort (List);
      elsif Field = "Memory" then
         Sorting_By_Memory_Used.Sort (List);
      elsif Field = "IO" then
         Sorting_By_IO_Used.Sort (List);
      elsif Field = "Priority" then
         Sorting_By_Priority.Sort (List);
      elsif Field = "O" then
         Sorting_By_Override.Sort (List);
      elsif Field = "S" then
         Sorting_By_Share.Sort (List);
      elsif Field = "F" then
         Sorting_By_Functional.Sort (List);
      elsif Field = "Urgency" then
         Sorting_By_Urgency.Sort (List);
      elsif Field = "Resource" then
         Sorting_By_Resource_Contrib.Sort (List);
      elsif Field = "Waiting" then
         Sorting_By_Waiting_Contrib.Sort (List);
      elsif Field = "Custom" then
         Sorting_By_Posix_Priority.Sort (List);
      elsif Field = "Ends In" or else
        Field = "Ends At" then
         Sorting_By_End.Sort (List);
      else
         HTML.Error ("Sorting by " & Field & " unimplemented");
      end if;
      if Direction = "dec" then
         List.Reverse_Elements;
      end if;
   end Sort_By;

   ------------------------
   -- Precedes_By_Resources   --
   --  Purpose: Check whether one job should precede another when sorted by
   --           various resources
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
   exception
      when Resource_Error =>
         return True;
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


   ---------------------
   -- Put_Predecessor --
   ---------------------

   procedure Put_Predecessor (Position : Utils.ID_Lists.Cursor) is
      ID : String := Ada.Strings.Fixed.Trim (Source => Utils.ID_Lists.Element (Position)'Img,
                                             Side   => Ada.Strings.Left);
   begin
      HTML.Put_Job_ID (Label    => "Predecessor",
                          ID    => ID);
   end Put_Predecessor;

   ---------------------
   -- Put_Successor --
   ---------------------

   procedure Put_Successor (Position : Utils.ID_Lists.Cursor) is
      ID : String := Ada.Strings.Fixed.Trim (Source => Utils.ID_Lists.Element (Position)'Img,
                                             Side   => Ada.Strings.Left);
   begin
      HTML.Put_Job_ID (Label => "Successor",
                       ID    => ID);
   end Put_Successor;

   -----------------
   -- Put_Request --
   -----------------

   procedure Put_Request (Position : Utils.String_Lists.Cursor) is
      Verbatim : String := To_String (Utils.String_Lists.Element (Position));
   begin
      HTML.Put_Paragraph (Label    => "requested",
                          Contents => Verbatim);
   end Put_Request;


   --------------
   -- Put_List --
   --------------

   procedure Put_List is
   begin
      List.Iterate (Jobs.Put_Res_Line'Access);
   end Put_List;

   -------------------
   -- Put_Time_List --
   -------------------

   procedure Put_Time_List is
   begin
      List.Iterate (Jobs.Put_Time_Line'Access);
   end Put_Time_List;

   --------------------
   -- Put_Bunch_List --
   --------------------

   procedure Put_Bunch_List is
   begin
      List.Iterate (Jobs.Put_Bunch_Line'Access);
   end Put_Bunch_List;


   -----------------
   -- Put_Details --
   -----------------

   procedure Put_Details is
   begin
      List.Iterate (Jobs.Put'Access);
   end Put_Details;

   ---------
   -- Put --
   ---------

   procedure Put  (Cursor : Job_Lists.Cursor) is
      Res         : Resource_Lists.Cursor;
      Slot_Range  : Range_Lists.Cursor;
      Q, Msg : String_Lists.Cursor;
      J           : Job := Job_Lists.Element (Cursor);

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
         HTML.Put_Paragraph (Label    => "Submitted",
                             Contents => J.Submission_Time);
         J.Predecessors.Iterate (Process => Put_Predecessor'Access);
         J.Predecessor_Request.Iterate (Process => Put_Request'Access);
         J.Successors.Iterate (Process => Put_Successor'Access);
         HTML.Put_Paragraph ("Array Task", J.Job_Array);
         Ada.Text_IO.Put ("<p>Reserve: ");
         HTML.Put (J.Reserve);
         Ada.Text_IO.Put_Line ("</p>");
         Ada.Text_IO.Put ("<p>State: ");
         Put_State (J.State);
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
            exit when Slot_Range = Ranges.Range_Lists.No_Element;
            Ranges.Put (Ranges.Range_Lists.Element (Slot_Range));
            Next (Slot_Range);
         end loop;

         HTML.Put_Heading (Title => "Assigned",
                           Level => 3);
         HTML.Put_List (J.Task_List);

         HTML.Put_Heading (Title => "Detected",
                           Level => 3);
         HTML.Put_List (J.Detected_Queues);

         HTML.Put_Clearer;
         HTML.End_Div (Class => "job_queue");
      end Put_Queues;

      procedure Put_Resources is
      begin
         HTML.Begin_Div (Class => "job_resources");
         HTML.Put_Heading (Title => "Hard",
                           Level => 3);
         Res := J.Hard.First;
         while Res /= Resources.Resource_Lists.No_Element loop
            Resources.Put (Res);
            Next (Res);
         end loop;

         HTML.Put_Heading (Title => "Soft",
                           Level => 3);
         Res := J.Soft.First;
         while Res /= Resources.Resource_Lists.No_Element loop
            Resources.Put (Res);
            Next (Res);
         end loop;
         HTML.End_Div (Class => "job_resources");
      end Put_Resources;

      procedure Put_Usage is
      begin
         HTML.Begin_Div (Class => "job_usage");
         HTML.Put_Heading (Title => "JAT",
                           Level => 3);
         for T in J.JAT_Usage'Range loop
            Put_Usage (T, J.JAT_Usage (T));
         end loop;
         HTML.Put_Heading (Title => "PET",
                           Level => 3);
         for T in J.PET_Usage'Range loop
            Put_Usage (T, J.PET_Usage (T));
         end loop;
         HTML.End_Div (Class => "job_usage");
      end Put_Usage;

      procedure Put_Files is
      begin
         HTML.Begin_Div (Class => "job_files");
         HTML.Put_Paragraph ("Directory", J.Directory);
         HTML.Put_Paragraph ("Script", J.Script_File);
         HTML.Put_Heading (Title => "Job Args",
                           Level => 3);
         HTML.Put_List (J.Args);

         HTML.Put_Paragraph ("Executable", J.Exec_File);
         Ada.Text_IO.Put ("<p>Merge StdErr: ");
         HTML.Put (J.Merge_Std_Err);
         Ada.Text_IO.Put_Line ("</p>");
         HTML.Put_Heading (Title => "StdOut",
                           Level => 3);
         HTML.Put_List (J.Std_Out_Paths);

         HTML.Put_Heading (Title => "StdErr",
                           Level => 3);
         HTML.Put_List (J.Std_Err_Paths);
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
      Put_Usage;
      Put_Files;

      HTML.Put_Clearer;
      HTML.End_Div (Class => "job_info");
      HTML.Put_Clearer;
   end Put;

   -------------------
   -- Put_Core_Line --
   --  Purpose: Output standard data for one job as a number of table cells (td).
   --  This included ID, name, and owner
   -------------------

   procedure Put_Core_Line (J : Job) is
   begin
      if Is_Empty (J.Task_IDs) or else not Is_Collapsed (J.Task_IDs) then
         HTML.Put_Cell (Data       => Ada.Strings.Fixed.Trim (J.Number'Img, Ada.Strings.Left),
                        Link_Param => "job_id");
      else
         HTML.Put_Cell (Data       => Ada.Strings.Fixed.Trim (J.Number'Img, Ada.Strings.Left)
                                      & "-" & Ada.Strings.Fixed.Trim (Min (J.Task_IDs)'Img, Ada.Strings.Left),
                        Link_Param => "job_id");
      end if;
      HTML.Put_Cell (Data => J.Owner, Link_Param => "user");
      if J.Name_Truncated then
         HTML.Put_Cell (Data => "<acronym title=""" & J.Full_Name & """>"
                        & J.Name & "</acronym>");
      else
         HTML.Put_Cell (Data => J.Name);
      end if;
   end Put_Core_Line;

   -------------------
   -- Put_Time_Line --
   --  Purpose: Output one Job, including prospective end time, as a table row (tr).
   -------------------

   procedure Put_Time_Line (Pos : Job_Lists.Cursor) is
      J : Job := Jobs.Job_Lists.Element (Pos);
   begin
      Ada.Text_IO.Put ("<tr>");
      Put_Core_Line (J);

      HTML.Put_Cell (Data => J.Slot_Number, Tag => "td class=""right""");
      begin
         HTML.Put_Duration_Cell (Remaining_Time (J));
      exception
         when Resource_Error =>
            HTML.Put_Cell (Data => "<i>unknown</i>", Class => "right");
      end;
      begin
         HTML.Put_Time_Cell (End_Time (J));
      exception
         when Resource_Error =>
            HTML.Put_Cell (Data => "<i>unknown</i>");
      end;
      HTML.Put_Img_Cell (State_As_String (J));
      Ada.Text_IO.Put ("</tr>");
   exception
      when E :
         others => HTML.Error (Message => "Error while outputting job: "
                                     & Exception_Message (E));
         Ada.Text_IO.Put ("</tr>");
   end Put_Time_Line;

   -------------------
   -- Put_Bunch_Line --
   --  Purpose: Output one Job
   -------------------

   procedure Put_Bunch_Line (Pos : Job_Lists.Cursor) is
      J : Job := Jobs.Job_Lists.Element (Pos);
   begin
      Ada.Text_IO.Put ("<tr>");
      Put_Core_Line (J);
      HTML.Put_Cell (Data => J.PE);
      Ranges.Put_Cell (Data => J.Slot_List, Class => "right");
      HTML.Put_Cell (Data       => To_Unbounded_String (J.Hard));
      HTML.Put_Cell (Data       => To_Unbounded_String (J.Soft));
      HTML.Put_Img_Cell (State_As_String (J));
      Ada.Text_IO.Put ("</tr>");
   exception
      when E :
         others => HTML.Error (Message => "Error while outputting job: "
                                     & Exception_Message (E));
         Ada.Text_IO.Put ("</tr>");
   end Put_Bunch_Line;

   ------------------
   -- Put_Res_Line --
   --  Purpose: Output one Job, including resource usage, as a table row (tr)
   ------------------

   procedure Put_Res_Line (Pos : Job_Lists.Cursor) is
      J : Job := Jobs.Job_Lists.Element (Pos);
   begin
      Ada.Text_IO.Put ("<tr>");
      Put_Core_Line (J);

      HTML.Put_Time_Cell (J.Submission_Time);
      HTML.Put_Cell (Data => J.Slot_Number, Tag => "td class=""right""");
      HTML.Put_Img_Cell (State_As_String (J));
      if J.CPU > 0.1 then
         HTML.Put_Duration_Cell (Integer (J.CPU));
      else
         HTML.Put_Cell ("");
      end if;
      if J.Mem > 3_600.0 then
         HTML.Put_Cell (Data  => Integer'Image (Integer (J.Mem / 3_600.0)),
                        Class => "right");
      elsif J.Mem > 1.0 then
         HTML.Put_Cell (Data  => HTML.Encode ("<1"),
                        Class => "right");
      else
         HTML.Put_Cell ("");
      end if;
      if J.IO > 1.0 then
         HTML.Put_Cell (Data  => Integer'Image (Integer (J.IO)),
                        Class => "right");
      elsif J.IO > 0.01 then
         HTML.Put_Cell (Data  => "<1",
                        Class => "right");
      else
         HTML.Put_Cell ("");
      end if;
      HTML.Put_Cell (Data => J.Priority'Img);
      HTML.Put_Cell (Data => J.Override_Tickets'Img, Class => "right");
      HTML.Put_Cell (Data => J.Share_Tickets'Img, Class => "right");
      HTML.Put_Cell (Data => J.Functional_Tickets'Img, Class => "right");
      HTML.Put_Cell (Data => J.Urgency'Img, Class => "right");
      HTML.Put_Cell (Data => J.Resource_Contrib'Img, Class => "right");
      HTML.Put_Cell (Data => J.Waiting_Contrib'Img, Class => "right");
      HTML.Put_Cell (Data => J.Posix_Priority'Img, Class => "right");
      Ada.Text_IO.Put ("</tr>");
   exception
      when E : others => HTML.Error (Message => "Error while outputting job: "
                                     & Exception_Message (E));
      Ada.Text_IO.Put ("</tr>");
   end Put_Res_Line;

   procedure Put_Usage (Kind : Usage_Type; Amount : Usage_Number) is
   begin
      case Kind is
         when cpu =>
            declare
               Days : Natural;
               Dur  : Duration;
            begin
               Days := Natural (Amount / 86400.0);
               Dur  := Ada.Real_Time.To_Duration
                       (Ada.Real_Time.Seconds (Natural (Amount - Days * 86_400.0)));
               if Days > 0 then
                  HTML.Put_Paragraph
                  (Contents  => Days'Img & "d " & Ada.Calendar.Formatting.Image (Dur),
                   Label => Kind'Img);
               elsif Amount >= 1.0 then
                  HTML.Put_Paragraph (Contents  => Ada.Calendar.Formatting.Image (Dur),
                                 Label     => Kind'Img);
               elsif Amount > 0.0 then
                  HTML.Put_Paragraph (Label => "CPU", Contents => "< 1s");
               else
                  HTML.Put_Paragraph (Label    => "CPU",
                                 Contents => "0");
               end if;
            exception
               when Constraint_Error =>
                  HTML.Put_Paragraph (Label    => Kind'Img,
                                 Contents => "<i>out of range</i>");
            end;
         when mem =>
            HTML.Put_Paragraph (Label    => "Memory",
                           Contents => Natural (Amount)'Img & " "
                           & HTML.Acronym (Short => "GBs",
                                            Long => "Gigabytes times seconds"));
         when io =>
            HTML.Put_Paragraph (Label    => "I/O",
                           Contents => Amount'Img);
         when vmem =>
            HTML.Put_Paragraph (Label    => "vMem",
                           Contents => To_Memory (Usage_Integer (Amount)));
         when maxvmem =>
            HTML.Put_Paragraph (Label    => "Maximum vMem",
                           Contents => To_Memory (Usage_Integer (Amount)));
         when iow =>
            null; -- iow is suppressed in qstat -j without -xml as well
         when submission_time =>
            null; -- ignore for now; note that this is also treated as cumulative
                  --  in Jobs.Extract_Tasks, although it represents a point in time
         when priority =>
            null;
            --  ignore for now. It is unclear how one can "use" priority
            --  Put_Paragraph (Label => "Priority",
            --               Contents => Amount'Img);
      end case;
   end Put_Usage;

end Jobs;
