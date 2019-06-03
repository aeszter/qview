with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Calendar;   use Ada.Calendar;
--  with Resources;      use Resources;
with HTML;
--  with Parser; use Parser;
--  with SGE.Ranges; use SGE.Ranges;
--  with Ranges; use Ranges;
--  with SGE.Resources;
--  with SGE.Utils;
--  with SGE.Context;
with Slurm.Jobs; use Slurm.Jobs;
with Slurm.Utils; use Slurm.Utils;

package body Jobs is

--     List : SGE.Jobs.List;
   procedure Put (Position : Slurm.Jobs.Cursor);
   procedure Put_Reason (Flag : Slurm.Jobs.state_reasons);
   procedure Put_State (Flag : Slurm.Jobs.states);
   procedure Put_State (J : Job);
   procedure Put_State_Cell (J : Job);
   procedure Put_Core_Header;
   procedure Put_Core_Line (J : Job);
--     procedure Put_Prio_Core (J : Job);
   procedure Start_Row (J : Job);
   procedure Finish_Row (J : Job);
--     procedure Try_Put_Paragraph (Label  : String;
--                                  Getter : not null access function (J : Job) return String;
--                                  J     : Job);
--     procedure Try_Put_Paragraph (Label  : String;
--                                  Getter : not null access function (J : Job) return Time;
--                                  J     : Job);
--
--     procedure Bunch (Result : out SGE.Bunches.List) is
--     begin
--        Sort (List);
--        SGE.Bunches.Initialize (Job_List   => List,
--                                Bunch_List => Result);
--     end Bunch;

   procedure Finish_Row (J : Job) is
      pragma Unreferenced (J);
      procedure Put_Error (Message : String);

      procedure Put_Error (Message : String) is
      begin
         HTML.Comment (Message);
      end Put_Error;

   begin
      Ada.Text_IO.Put_Line ("</tr>");
--        Iterate_Errors (J, Put_Error'Access);
   end Finish_Row;

   function Name_As_HTML (J : Job) return String is
      Name : constant String := Get_Name (J);
   begin
      if Name'Length > 20 then
         return "<acronym title=""" & Name & """>"
                           & Name (Name'First .. Name'First + 19) & "</acronym>";
      else
         return Name;
      end if;
   end Name_As_HTML;

--
--
--     procedure Append_List (Nodes : Node_List; Fix_Posix_Prio : Boolean := False) is
--     begin
--        SGE.Jobs.Append (Collection     => List,
--                         Nodes          => Nodes,
--                         Fix_Posix_Prio => Fix_Posix_Prio);
--     exception
--        when E : others
--           => HTML.Error ("Unable to read job info (Append_List): " & Exception_Message (E));
--     end Append_List;
--
--     procedure Update_Messages (Nodes : Node_List) is
--     begin
--        SGE.Jobs.Update_Messages (Collection => List,
--                                  Nodes      => Nodes);
--     exception
--        when E : others
--           => HTML.Error ("Unable to update job messages: " & Exception_Message (E));
--     end Update_Messages;
--
--     procedure Prune_List is
--     begin
--        HTML.Bug_Ref (Bug_ID => 1830,
--                      Info   => "Jobs.Prune_List called");
--  --      SGE.Jobs.Prune_List (PE            => PE,
--  --                           Queue         => Queue,
--  --                           Hard_Requests => Hard_Requests,
--  --                           Soft_Requests => Soft_Requests,
--  --                           Slot_Number   => Slot_Number,
--  --                           Slot_Ranges   => Slot_Ranges);
--     end Prune_List;
--
--     -----------------
--     -- Append_List --
--     -----------------
--
--     procedure Prune_List (PE, Queue, Hard_Requests,
--                           Soft_Requests,
--                           Slot_Number, Slot_Ranges : Unbounded_String) is
--     begin
--        HTML.Bug_Ref (Bug_ID => 1830,
--                      Info   => "Jobs.Prune_List called");
--        SGE.Jobs.Prune (Collection    => List,
--                        PE            => PE,
--                        Queue         => Queue,
--                        Hard_Requests => Hard_Requests,
--                        Soft_Requests => Soft_Requests,
--                        Slot_Number   => Slot_Number,
--                        Slot_Ranges   => Slot_Ranges);
--     end Prune_List;
--
--     -------------------
--     -- Update_Status --
--     -------------------
--
--     procedure Update_Status is
--     begin
--        SGE.Jobs.Update_Status (List);
--     end Update_Status;
--
--
--     -------------------
--     -- Search_Queues --
--     -------------------
--
--     procedure Search_Queues is
--     begin
--        SGE.Jobs.Search_Queues (List);
--     exception
--        when E : others => HTML.Error ("Error while searching for queues: "
--                                         & Exception_Message (E));
--     end Search_Queues;
--
--     ------------------
--     -- Sort_By      --
--     --  Purpose:  Sort the job list by any column/field
--     --  Parameter Field: Title of the column to sort by
--     ------------------
--
--     procedure Sort_By (Field : String; Direction : String) is
--     begin
--        SGE.Jobs.Sort_By (Collection => List,
--                          Field      => Field,
--                          Direction => Direction);
--     end Sort_By;
--
--     ---------------------
--     -- Put_Predecessor --
--     ---------------------
--
--     procedure Put_Predecessor (ID : Natural) is
--        S : constant String := Ada.Strings.Fixed.Trim (Source => ID'Img,
--                                              Side   => Ada.Strings.Left);
--     begin
--        HTML.Put_Link (Label      => "Predecessor",
--                       ID         => S,
--                       Link_Param => "job_id");
--     end Put_Predecessor;
--
--     ---------------------
--     -- Put_Successor --
--     ---------------------
--
--     procedure Put_Successor (ID : Natural) is
--        S : constant String := Ada.Strings.Fixed.Trim (Source => ID'Img,
--                                              Side   => Ada.Strings.Left);
--     begin
--        HTML.Put_Link (Label      => "Successor",
--                       ID         => S,
--                       Link_Param => "job_id");
--     end Put_Successor;
--
--     -----------------
--     -- Put_Request --
--     -----------------
--
--     procedure Put_Request (S : String) is
--     begin
--        HTML.Put_Paragraph (Label    => "requested",
--                            Contents => S);
--     end Put_Request;
--
--

   procedure Put (Position : Slurm.Jobs.Cursor) is
      use Slurm.Jobs;
      J : Job;
   begin
      if Has_Element (Position) then
         J := Element (Position);
      else
         raise Constraint_Error with "no such job";
      end if;
      Start_Row (J);
      Put_Core_Line (J);
      HTML.Put_Time_Cell (Get_Submission_Time (J));
      HTML.Put_Cell (Data  => Get_Tasks (J)'Img,
                Class => "right");
      Put_State_Cell (J);
      HTML.Put_Cell (Data => Get_Gres (J));
      HTML.Put_Duration_Cell (Walltime (J));
      HTML.Put_Cell (Data => Get_Priority (J)'Img,
                     Class => "right");
      if Has_Start_Time (J) then
         HTML.Put_Time_Cell (Get_Start_Time (J));
      else
         HTML.Put_Cell ("");
      end if;
      Finish_Row (J);
   end Put;

   procedure Put_Core_Header is
   begin
      HTML.Put_Header_Cell (Data => "Number");
      HTML.Put_Header_Cell (Data => "Project");
      HTML.Put_Header_Cell (Data => "Owner");
      HTML.Put_Header_Cell (Data => "Name");
   end Put_Core_Header;

   procedure Put_Core_Line (J : Job) is
--        Task_IDs : constant SGE.Ranges.Step_Range_List := Get_Task_IDs (J);
   begin
--        if Is_Empty (Task_IDs) or else not Is_Collapsed (Task_IDs) then
         HTML.Put_Cell (Data       => Ada.Strings.Fixed.Trim (Get_ID (J)'Img, Ada.Strings.Left),
                        Link_Param => "job_id");
--        else
--           HTML.Put_Cell (Data       => Ada.Strings.Fixed.Trim (Get_ID (J), Ada.Strings.Left)
--                                        & "-" & Ada.Strings.Fixed.Trim (Min (Task_IDs)'Img, Ada.Strings.Left),
--                          Link_Param => "job_id");
--        end if;
      HTML.Put_Cell (Data => Get_Project (J));
      HTML.Put_Cell (Data => To_String (Get_Owner (J)), Link_Param => "user");
      HTML.Put_Cell (Data => Name_As_HTML (J));
   end Put_Core_Line;

   procedure Put_Details (ID : Natural) is
      The_List : constant Slurm.Jobs.List := Slurm.Jobs.Load_Jobs;
      J        : constant Job := Slurm.Jobs.Get_Job (The_List, ID);

--        procedure Put_Actions;
      procedure Put_Files;
      procedure Put_Meta;
      procedure Put_Name;
      procedure Put_Queues;
      procedure Put_Resources;
      procedure Put_Usage;

--        procedure Put_Actions is
--        begin
--           HTML.Begin_Div (ID => "job_actions");
--           HTML.Put_Img (Name => "kill",
--                         Text => "Kill job",
--                         Link => HTML.Get_Action_URL (Action => "k",
--                                                      Params => "j=" & Get_ID (J)));
--           HTML.End_Div (ID => "job_actions");
--        end Put_Actions;

      procedure Put_Files is
      begin
         HTML.Begin_Div (Class => "job_files");
         HTML.Put_Paragraph ("Directory", Get_Working_Directory (J));
         HTML.Put_Paragraph ("Command", Get_Command (J));
         HTML.Put_Paragraph ("StdIn", Get_Std_In (J));
         HTML.Put_Paragraph ("StdOut", Get_Std_Out (J));
         HTML.Put_Paragraph ("StdErr", Get_Std_Err (J));
         HTML.End_Div (Class => "job_files");
      end Put_Files;

      procedure Put_Meta is
      begin
         HTML.Begin_Div (Class => "job_meta");
         HTML.Put_Paragraph ("ID", Get_ID (J)'Img);
         HTML.Put_Paragraph ("Owner",  To_String (Get_Owner (J)));
         HTML.Put_Paragraph ("Group", To_String (Get_Group (J)));
         HTML.Put_Paragraph ("Project", Get_Project (J));
         HTML.Put_Paragraph (Label    => "Submitted",
                             Contents => Get_Submission_Time (J));
         HTML.Put_Paragraph (Label    => "Starts",
                             Contents => Get_Start_Time (J));
         HTML.Put_Paragraph (Label    => "Ends",
                             Contents => Get_End_Time (J));
         HTML.Put_Paragraph ("Dependency", Get_Dependency (J));
         HTML.Put_Paragraph ("Reservation", Get_Reservation (J));
         HTML.Put_Paragraph ("Submitted on", Get_Alloc_Node (J));
         Ada.Text_IO.Put ("<p>State: ");
         Put_State (J);
         Ada.Text_IO.Put_Line ("</p>");
         Ada.Text_IO.Put ("<p>Reason: ");
         if Get_State_Reason (J) /= WAIT_NO_REASON then
            Put_Reason (Get_State_Reason (J));
         end if;
         Ada.Text_IO.Put_Line ("</p>");
         Ada.Text_IO.Put_Line (Get_State_Description (J));
         HTML.Put_Clearer;
         HTML.End_Div (Class => "job_meta");
      end Put_Meta;

      procedure Put_Name is
      begin
         HTML.Begin_Div (Class => "job_name");
         Ada.Text_IO.Put ("<p>");
--           HTML.Put_Img (Name => "hand.right",
--                         Text => "unlock job manipulation",
--                         Link => "#",
--                         Extra_Args => "onclick=""document.getElementById('job_actions').style.display = 'block' """);
         --         HTML.Put_Paragraph ("Name", Get_Name (J));

         Ada.Text_IO.Put_Line ("Name: " & Get_Name (J) & "</p>");
         if Has_Comment (J) then
            Ada.Text_IO.Put_Line ("<p class=""message"">Comment: "
                                  & Get_Comment (J) & "</p>");
         end if;
         if Has_Admin_Comment (J) then
            Ada.Text_IO.Put_Line ("<p class=""message"">Comment: "
                                  & Get_Admin_Comment (J) & "</p>");
         end if;
         HTML.End_Div (Class => "job_name");
      end Put_Name;

      procedure Put_Queues is
      begin
         HTML.Begin_Div (Class => "job_queue");

         HTML.Put_Paragraph ("Partition", Get_Partition (J));
         HTML.Put_Paragraph ("Nodes", Get_Nodes (J));
         HTML.Put_Paragraph ("CPUs", Get_CPUs (J)'Img);

         HTML.Put_Clearer;
         HTML.End_Div (Class => "job_queue");
      end Put_Queues;

      procedure Put_Resources is
      begin
         HTML.Begin_Div (Class => "job_resources");
         HTML.Put_Paragraph ("Share", Has_Share (J));
         Ada.Text_IO.Put_Line (Get_Gres (J));
         HTML.End_Div (Class => "job_resources");
      end Put_Resources;

      procedure Put_Usage is
      begin
         HTML.Begin_Div (Class => "job_usage");
         Ada.Text_IO.Put_Line ("unimplemented");
         HTML.End_Div (Class => "job_usage");
      end Put_Usage;

   begin
      HTML.Begin_Div (Class => "job_info");

      HTML.Begin_Div (Class => "action_and_name");
--      Put_Actions;
      Put_Name;
      HTML.Put_Clearer;
      HTML.End_Div (Class => "action_and_name");
      Put_Meta;
      Put_Queues;
      HTML.Begin_Div (Class => "res_and_context");
      Put_Resources;
      HTML.End_Div (Class => "res_and_context");
      Put_Usage;
      Put_Files;

      HTML.Put_Clearer;
      HTML.End_Div (Class => "job_info");
      HTML.Put_Clearer;
   end Put_Details;

   procedure Put_Global_List is
   begin
      Put_List (Slurm.Jobs.Load_Jobs);
   end Put_Global_List;

   procedure Put_List (List : Slurm.Jobs.List) is
      use Slurm.Jobs;
   begin
      Put_Summary (List);
      HTML.Begin_Div (Class => "job_list");
      Ada.Text_IO.Put ("<table><tr>");
      Put_Core_Header;
      HTML.Put_Header_Cell (Data => "Submitted");
      HTML.Put_Header_Cell (Data => "Tasks");
      HTML.Put_Header_Cell (Data => "State");
      HTML.Put_Header_Cell (Data => "Res");
      HTML.Put_Header_Cell (Data => "Walltime");
      HTML.Put_Header_Cell (Data => "Priority");
      HTML.Put_Header_Cell (Data => "Start");

      Ada.Text_IO.Put ("</tr>");
      Iterate (List, Put'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "job_list");
   end Put_List;

   procedure Put_Pending_List is
      use Slurm.Jobs;
      Pending_List : List;
   begin
      Pending_List := Extract (Source => Load_Jobs, Selector => Is_Pending'Access);
      Put_List (Pending_List);
   end Put_Pending_List;

   procedure Put_Reason (Flag : Slurm.Jobs.state_reasons) is
      procedure Put (What : String) renames Ada.Text_IO.put;
   begin
      Put ("<img src=""/icons/" & Flag'Img & ".png"" ");
      Put ("alt=""" & Flag'Img & """ title=""" & Flag'Img & """ />");
   end Put_Reason;

   procedure Put_Running_List is
      use Slurm.Jobs;
      Running_List : List;
   begin
      Running_List := Extract (Source => Load_Jobs, Selector => Is_Running'Access);
      Put_List (Running_List);
   end Put_Running_List;

   procedure Put_State (Flag : Slurm.Jobs.states) is
      procedure Put (What : String) renames Ada.Text_IO.put;
   begin
      Put ("<img src=""/icons/" & Flag'Img & ".png"" ");
      Put ("alt=""" & Flag'Img & """ title=""" & Flag'Img & """ />");
   end Put_State;

   procedure Put_State (J : Job) is
      procedure Put (What : String) renames Ada.Text_IO.put;
   begin
      Put ("<img src=""/icons/" & Get_State (J) & ".png"" ");
      Put ("alt=""" & Get_State (J) & """ title=""" & Get_State (J) & ": ");
      if Is_Running (J) then
         Put ("running");
      end if;
--        if On_Hold (J) then
--           Put ("on hold");
--        end if;
--        if Has_Error (J) then
--           Put ("Error");
--        end if;
      Put (""" />");
--        if Has_Error (J) then
--           Put (" <a href=""" &
--                  HTML.Get_Action_URL (Action => "cj", Params => "j=" & Get_ID (J)) &
--                  """>clear error</a>");
--        end if;
   end Put_State;

   procedure Put_State_Cell (J : Job) is
   begin
      if Has_Error (J) then
         HTML.Put_Img_Cell (Get_State (J),
                            Extra_Text => " <a href=""" &
                              HTML.Get_Action_URL (Action => "cj",
                                                   Params => "j="
                                                   & Get_ID (J)'Img) & """>clear error</a>");
      else
         if Get_State (J) = JOB_PENDING and then
           Get_State_Reason (J) = WAIT_DEPENDENCY
         then
            HTML.Put_Img_Cell ("WAIT_DEPENDENCY");
         else
            HTML.Put_Img_Cell (Get_State (J));
         end if;
      end if;
   end Put_State_Cell;

   procedure Put_Summary (List : Slurm.Jobs.List) is
      Job_Summary, Task_Summary : State_Count;
   begin
      Slurm.Jobs.Get_Summary (Collection => List,
                            Jobs => Job_Summary,
                            Tasks => Task_Summary);
      HTML.Begin_Div (ID => "job_summary");
      Ada.Text_IO.Put ("<ul>");
      for State in Job_Summary'Range loop
         Ada.Text_IO.Put ("<li>");
         Ada.Text_IO.Put (Job_Summary (State)'Img);
         if Task_Summary (State) > 0 then
            Ada.Text_IO.Put ("(" & Ada.Strings.Fixed.Trim (
      Task_Summary (State)'Img, Ada.Strings.Left) & ")");
         end if;
         Ada.Text_IO.Put (" ");
         Put_State (Flag => State);
         Ada.Text_IO.Put_Line ("</li>");
      end loop;

      Ada.Text_IO.Put ("</ul>");
      HTML.End_Div (ID => "job_summary");
   end Put_Summary;
--
--     -------------------
--     -- Put_Time_List --
--     -------------------
--
--     procedure Put_Time_List is
--     begin
--        HTML.Begin_Div (Class => "job_list");
--        Ada.Text_IO.Put ("<table><tr>");
--        Put_Core_Header;
--        HTML.Put_Header_Cell (Data => "Slots");
--        HTML.Put_Header_Cell (Data => "Ends In");
--        HTML.Put_Header_Cell (Data => "Ends At");
--        HTML.Put_Header_Cell (Data => "State");
--        Ada.Text_IO.Put ("</tr>");
--        Iterate (List, Jobs.Put_Time_Line'Access);
--           --  Table Footer
--        Ada.Text_IO.Put_Line ("</table>");
--        HTML.End_Div (Class => "job_list");
--     end Put_Time_List;
--
--     --------------------
--     -- Put_Bunch_List --
--     --------------------
--
--     procedure Put_Bunch_List is
--     begin
--        HTML.Put_Heading (Title => "Jobs",
--                          Level => 2);
--        HTML.Begin_Div (Class => "job_list");
--        Ada.Text_IO.Put_Line ("<table><tr>");
--        Put_Core_Header;
--        HTML.Put_Header_Cell (Data     => "PE");
--        HTML.Put_Header_Cell (Data     => "Slots");
--        HTML.Put_Header_Cell (Data     => "Hard");
--        HTML.Put_Header_Cell (Data     => "Soft");
--        HTML.Put_Header_Cell (Data     => "State");
--        Ada.Text_IO.Put ("</tr>");
--        Iterate (List, Jobs.Put_Bunch_Line'Access);
--
--        --  Table Footer
--        Ada.Text_IO.Put_Line ("</table>");
--        HTML.End_Div (Class => "job_list");
--     end Put_Bunch_List;
--
--
--     ---------
--     -- Put --
--     ---------
--
--
--
--     -------------------
--     -- Put_Prio_Core --
--     -------------------
--
--     procedure Put_Prio_Core (J : Job) is
--     begin
--        HTML.Put_Cell (Data => Get_Priority (J)'Img);
--        HTML.Put_Cell (Data => Get_Override_Tickets (J)'Img, Class => "right");
--        HTML.Put_Cell (Data => Get_Share_Tickets (J)'Img, Class => "right");
--        HTML.Put_Cell (Data => Get_Functional_Tickets (J)'Img, Class => "right");
--        HTML.Put_Cell (Data => Get_Urgency (J)'Img, Class => "right");
--        HTML.Put_Cell (Data => Get_Resource_Contrib (J)'Img, Class => "right");
--        HTML.Put_Cell (Data => Get_Waiting_Contrib (J)'Img, Class => "right");
--        HTML.Put_Cell (Data => Get_Posix_Priority (J)'Img, Class => "right");
--     end Put_Prio_Core;
--
--
--     -------------------
--     -- Put_Time_Line --
--     --  Purpose: Output one Job, including prospective end time, as a table row (tr).
--     -------------------
--
--     procedure Put_Time_Line (J : Job) is
--     begin
--        Start_Row (J);
--        Put_Core_Line (J);
--
--        HTML.Put_Cell (Data => Get_Slot_Number (J), Tag => "td class=""right""");
--        begin
--           HTML.Put_Duration_Cell (Remaining_Time (J));
--        exception
--           when Resources.Error =>
--              HTML.Put_Cell (Data => "<i>unknown</i>", Class => "right");
--        end;
--        begin
--           HTML.Put_Time_Cell (End_Time (J));
--        exception
--           when Resources.Error =>
--              HTML.Put_Cell (Data => "<i>unknown</i>");
--        end;
--        Put_State_Cell (J);
--        Finish_Row (J);
--     exception
--        when E :
--           others => HTML.Error (Message => "Error while outputting job: "
--                                       & Exception_Message (E));
--           Finish_Row (J);
--     end Put_Time_Line;
--
--     -------------------
--     -- Put_Bunch_Line --
--     --  Purpose: Output one Job
--     -------------------
--
--     procedure Put_Bunch_Line (J : Job) is
--     begin
--        Start_Row (J);
--        Put_Core_Line (J);
--        HTML.Put_Cell (Data   => Get_PE (J));
--        Ranges.Put_Cell (Data => Get_Slot_List (J), Class => "right");
--        HTML.Put_Cell (Data   => Get_Hard_Resources (J));
--        HTML.Put_Cell (Data   => Get_Soft_Resources (J));
--        Put_State_Cell (J);
--        Finish_Row (J);
--     exception
--        when E :
--           others => HTML.Error (Message => "Error while outputting job: "
--                                       & Exception_Message (E));
--           Finish_Row (J);
--     end Put_Bunch_Line;
--
--
--     ------------------
--     -- Put_Res_Line --
--     --  Purpose: Output one Job, including resource usage, as a table row (tr)
--     ------------------
--
--     procedure Put_Res_Line (J : Job) is
--     begin
--        Start_Row (J);
--        Put_Core_Line (J);
--
--        HTML.Put_Time_Cell (Get_Submission_Time (J));
--        HTML.Put_Cell (Data => Get_Slot_Number (J), Tag => "td class=""right""");
--        Put_State_Cell (J);
--        if Get_CPU (J) > 0.1 then
--           HTML.Put_Duration_Cell (Integer (Get_CPU (J)));
--        else
--           HTML.Put_Cell ("");
--        end if;
--        if Get_Mem (J) > 3_600.0 then
--           HTML.Put_Cell (Data  => Integer'Image (Integer (Get_Mem (J) / 3_600.0)),
--                          Class => "right");
--        elsif Get_Mem (J) > 1.0 then
--           HTML.Put_Cell (Data  => HTML.Encode ("<1"),
--                          Class => "right");
--        else
--           HTML.Put_Cell ("");
--        end if;
--        if Get_IO (J) > 1.0 then
--           HTML.Put_Cell (Data  => Integer'Image (Integer (Get_IO (J))),
--                          Class => "right");
--        elsif Get_IO (J) > 0.01 then
--           HTML.Put_Cell (Data  => "<1",
--                          Class => "right");
--        else
--           HTML.Put_Cell ("");
--        end if;
--        Put_Prio_Core (J);
--        Finish_Row (J);
--     exception
--        when E : others => HTML.Error (Message => "Error while outputting job: "
--                                       & Exception_Message (E));
--        Finish_Row (J);
--     end Put_Res_Line;
--
--     procedure Put_Prio_Line (J : Job) is
--     begin
--        Start_Row (J);
--        Put_Core_Line (J);
--
--        HTML.Put_Time_Cell (Get_Submission_Time (J));
--        HTML.Put_Cell (Data  => To_String (Get_Slot_List (J), Short => True),
--                       Tag   => "td",
--                       Class => "right");
--        Put_State_Cell (J);
--        Ada.Text_IO.Put ("<td>");
--        HTML.Put (Has_Reserve (J));
--        Ada.Text_IO.Put ("</td>");
--        Put_Prio_Core (J);
--        Finish_Row (J);
--     exception
--        when E : others => HTML.Error (Message => "Error while outputting job: "
--                                       & Exception_Message (E));
--        Finish_Row (J);
--     end Put_Prio_Line;
--
--     procedure Put_Usage (Kind : Usage_Type; Amount : Usage_Number) is
--        procedure Put_Memory (Label : String; Memory : Usage_Number);
--
--        procedure Put_Memory (Label : String; Memory : Usage_Number) is
--        begin
--           HTML.Put_Paragraph (Label    => Label,
--                      Contents => To_Memory (Usage_Integer (Memory)));
--        exception
--           when Constraint_Error =>
--              HTML.Put_Paragraph (Label    => Label,
--                             Contents => "&gtr; 1 TB");
--        end Put_Memory;
--
--     begin
--        case Kind is
--           when cpu =>
--              declare
--                 Days : Natural;
--                 Dur  : Duration;
--              begin
--                 Days := Natural (Amount / 86400.0);
--                 Dur  := Ada.Real_Time.To_Duration
--                         (Ada.Real_Time.Seconds (Natural (Amount - Days * 86_400.0)));
--                 if Days > 0 then
--                    HTML.Put_Paragraph
--                    (Contents  => Days'Img & "d " & Ada.Calendar.Formatting.Image (Dur),
--                     Label => Kind'Img);
--                 elsif Amount >= 1.0 then
--                    HTML.Put_Paragraph (Contents  => Ada.Calendar.Formatting.Image (Dur),
--                                   Label     => Kind'Img);
--                 elsif Amount > 0.0 then
--                    HTML.Put_Paragraph (Label => "CPU", Contents => "< 1s");
--                 else
--                    HTML.Put_Paragraph (Label    => "CPU",
--                                   Contents => "0");
--                 end if;
--              exception
--                 when Constraint_Error =>
--                    HTML.Put_Paragraph (Label    => Kind'Img,
--                                   Contents => "<i>out of range</i>");
--              end;
--           when mem =>
--              HTML.Put_Paragraph (Label    => "Memory",
--                             Contents => Natural (Amount)'Img & " "
--                             & HTML.Acronym (Short => "GBs",
--                                              Long => "Gigabytes times seconds"));
--           when io =>
--              HTML.Put_Paragraph (Label    => "I/O",
--                             Contents => Amount'Img);
--           when vmem =>
--              Put_Memory (Label => "vMem",
--                          Memory => Amount);
--           when maxvmem =>
--              Put_Memory (Label => "Maximum vMem",
--                          Memory => Amount);
--           when iow =>
--              null; -- iow is suppressed in qstat -j without -xml as well
--           when submission_time =>
--              null; -- ignore for now; note that this is also treated as cumulative
--                    --  in Jobs.Extract_Tasks, although it represents a point in time
--           when start_time =>
--              null; -- likewise
--           when end_time =>
--              null; -- likewise
--           when priority =>
--              null;
--              --  ignore for now. It is unclear how one can "use" priority
--              --  Put_Paragraph (Label => "Priority",
--              --               Contents => Amount'Img);
--           when exit_status =>
--              null;
--              --  ignore as well
--           when signal =>
--              null;
--              --  ignore
--           when ru_wallclock =>
--              null; -- Bug #1752
--           when ru_utime =>
--              null; -- likewise
--           when ru_stime =>
--              null; -- likewise
--           when ru_maxrss =>
--              null; -- likewise
--           when ru_ixrss =>
--              null; -- likewise
--        end case;
--     end Put_Usage;
--

   procedure Put_User_List (User : String) is
   begin
      Put_List (Slurm.Jobs.Load_User (User));
   end Put_User_List;

   procedure Start_Row (J : Job) is
      pragma Unreferenced (J);
   begin
      Ada.Text_IO.Put ("<tr");
--        if Has_Error_Log_Entries (J) then
--           Ada.Text_IO.Put (" class=""program_error""");
--        elsif Quota_Inhibited (J) and then not Is_Running (J) then
--           Ada.Text_IO.Put (" class=""job-quota""");
--        end if;
      Ada.Text_IO.Put_Line (">");
   end Start_Row;

--     procedure Try_Put_Paragraph (Label  : String;
--                                  Getter : not null access function (J : Job) return String;
--                                  J      : Job) is
--     begin
--        HTML.Put_Paragraph (Label    => Label,
--                       Contents => Getter (J));
--     exception
--        when others => null;
--     end Try_Put_Paragraph;
--
--     procedure Try_Put_Paragraph (Label  : String;
--                                  Getter : not null access function (J : Job) return Time;
--                                  J      : Job) is
--        function Wrapper (J : Job) return String;
--
--        function Wrapper (J : Job) return String is
--        begin
--           return HTML.To_String (Getter (J));
--        end Wrapper;
--
--     begin
--        Try_Put_Paragraph (Label  => Label,
--                           Getter => Wrapper'Access,
--                           J      => J);
--     exception
--        when others =>
--           HTML.Put_Paragraph (Label    => Label,
--                               Contents => "<em>never</em>");
--     end Try_Put_Paragraph;
--
--     --------------
--     -- Overlays --
--     --------------
--
--     procedure Create_Overlay (Nodes : Node_List) is
--     begin
--        SGE.Jobs.Create_Overlay (Nodes);
--     exception
--        when E : others
--           => HTML.Error ("Unable to read job info (Create_Overlay): " & Exception_Message (E));
--     end Create_Overlay;
--
--     procedure Apply_Overlay is
--     begin
--        SGE.Jobs.Apply_Overlay (List);
--     end Apply_Overlay;
--
--     procedure Update_Quota is
--     begin
--        SGE.Jobs.Update_Quota (List);
--     end Update_Quota;
--
--     procedure Iterate (Process : not null access procedure (J : Job)) is
--     begin
--        SGE.Jobs.Iterate (Collection => List,
--                          Process    => Process);
--     end Iterate;

end Jobs;
