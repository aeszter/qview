with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Calendar;   use Ada.Calendar;
with Resources;      use Resources;
with Jobs;
with HTML;
with Parser; use Parser;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Ranges; use SGE.Ranges;
with Ranges; use Ranges;
with SGE.Resources;
with SGE.Utils;
with SGE.Context;



package body Jobs is

   procedure Put_State (Flag : SGE.Jobs.State_Flag);
   procedure Put_State (J : Job);
   procedure Put_State_Cell (J : Job);
   procedure Put_Core_Header;
   procedure Start_Row (J : Job);
   procedure Finish_Row (J : Job);
   procedure Try_Put_Paragraph (Label  : String;
                                Getter : not null access function (J : Job) return String;
                                J     : Job);
   procedure Try_Put_Paragraph (Label  : String;
                                Getter : not null access function (J : Job) return Time;
                                J     : Job);


   procedure Put_Summary is
      Task_Summary, Slot_Summary : State_Count;
   begin
      SGE.Jobs.Get_Summary (Tasks => Task_Summary,
                        Slots => Slot_Summary);
      HTML.Begin_Div (ID => "job_summary");
      Ada.Text_IO.Put ("<ul>");
      for State in Task_Summary'Range loop
         Ada.Text_IO.Put ("<li>");
         Ada.Text_IO.Put (Task_Summary (State)'Img);
         if Slot_Summary (State) > 0 then
            Ada.Text_IO.Put ("(" & Ada.Strings.Fixed.Trim (Slot_Summary (State)'Img, Ada.Strings.Left) & ")");
         end if;
         Ada.Text_IO.Put (" ");
         Put_State (Flag => State);
         Ada.Text_IO.Put_Line ("</li>");
      end loop;

      Ada.Text_IO.Put ("</ul>");
      HTML.End_Div (ID => "job_summary");
   end Put_Summary;

   procedure Put_State (Flag : SGE.Jobs.State_Flag) is
      procedure Put (What : String) renames Ada.Text_IO.put;
   begin
      Put ("<img src=""/icons/" & To_Abbrev (Flag) & ".png"" ");
      Put ("alt=""" & To_Abbrev (Flag) & """ title=""" & To_Abbrev (Flag) & ": ");
      Put (To_String (Flag) & """ />");
   end Put_State;

   procedure Put_State (J : Job) is
      procedure Put (What : String) renames Ada.Text_IO.put;
   begin
      Put ("<img src=""/icons/" & Get_State (J) & ".png"" ");
      Put ("alt=""" & Get_State (J) & """ title=""" & Get_State (J) & ": ");
      if Is_Running (J) then
         Put ("running");
      end if;
      if On_Hold (J) then
         Put ("on hold");
      end if;
      if Has_Error (J) then
         Put ("Error");
      end if;
      Put (""" />");
      if Has_Error (J) then
         Put (" <a href=""" &
                HTML.Get_Action_URL (Action => "cj", Params => "j=" & Get_ID (J)) &
                """>clear error</a>");
      end if;
   end Put_State;

   ------------------
   -- Name_As_HTML --
   ------------------

   function Name_As_HTML (J : Job) return String is
   begin
      if Is_Name_Truncated (J) then
         return "<acronym title=""" & Get_Full_Name (J) & """>"
                           & Get_Name (J) & "</acronym>";
      else
         return Get_Name (J);
      end if;
   end Name_As_HTML;

   procedure Append_List (Nodes : Node_List) is
   begin
      HTML.Bug_Ref (Bug_ID => 1830,
                    Info   => "Jobs.Append_List called");
      SGE.Jobs.Append_List (Nodes);
   exception
      when E : others
         => HTML.Error ("Unable to read job info (Append_List): " & Exception_Message (E));
   end Append_List;

   procedure Update_Messages (Nodes : Node_List) is
   begin
      SGE.Jobs.Update_Messages (Nodes);
   exception
      when E : others
         => HTML.Error ("Unable to update job messages: " & Exception_Message (E));
   end Update_Messages;

   procedure Prune_List is
   begin
      HTML.Bug_Ref (Bug_ID => 1830,
                    Info   => "Jobs.Prune_List called");
--      SGE.Jobs.Prune_List (PE            => PE,
--                           Queue         => Queue,
--                           Hard_Requests => Hard_Requests,
--                           Soft_Requests => Soft_Requests,
--                           Slot_Number   => Slot_Number,
--                           Slot_Ranges   => Slot_Ranges);
   end Prune_List;

   -----------------
   -- Append_List --
   -----------------

   procedure Prune_List (PE, Queue, Hard_Requests,
                         Soft_Requests,
                         Slot_Number, Slot_Ranges : Unbounded_String) is
   begin
      HTML.Bug_Ref (Bug_ID => 1830,
                    Info   => "Jobs.Prune_List called");
      SGE.Jobs.Prune_List (PE            => PE,
                           Queue         => Queue,
                           Hard_Requests => Hard_Requests,
                           Soft_Requests => Soft_Requests,
                           Slot_Number   => Slot_Number,
                           Slot_Ranges   => Slot_Ranges);
   end Prune_List;

   -------------------
   -- Update_Status --
   -------------------

   procedure Update_Status is
   begin
      HTML.Bug_Ref (Bug_ID => 1830,
                    Info   => "Jobs.Update_Status called");
      SGE.Jobs.Update_Status;
   end Update_Status;


   -------------------
   -- Search_Queues --
   -------------------

   procedure Search_Queues is
   begin
      HTML.Bug_Ref (Bug_ID => 1830,
                    Info   => "Jobs.Search_Queues called");
      SGE.Jobs.Search_Queues;
   exception
      when E : others => HTML.Error ("Error while searching for queues: "
                                       & Exception_Message (E));
   end Search_Queues;

   ------------------
   -- Sort_By      --
   --  Purpose:  Sort the job list by any column/field
   --  Parameter Field: Title of the column to sort by
   ------------------

   procedure Sort_By (Field : String; Direction : String) is
   begin
      HTML.Bug_Ref (Bug_ID => 1830,
                    Info   => "Jobs.Sort_By");
      SGE.Jobs.Sort_By (Field     => Field,
                        Direction => Direction);
   end Sort_By;

   ---------------------
   -- Put_Predecessor --
   ---------------------

   procedure Put_Predecessor (ID : Natural) is
      S : constant String := Ada.Strings.Fixed.Trim (Source => ID'Img,
                                            Side   => Ada.Strings.Left);
   begin
      HTML.Put_Link (Label      => "Predecessor",
                     ID         => S,
                     Link_Param => "job_id");
   end Put_Predecessor;

   ---------------------
   -- Put_Successor --
   ---------------------

   procedure Put_Successor (ID : Natural) is
      S : constant String := Ada.Strings.Fixed.Trim (Source => ID'Img,
                                            Side   => Ada.Strings.Left);
   begin
      HTML.Put_Link (Label      => "Successor",
                     ID         => S,
                     Link_Param => "job_id");
   end Put_Successor;

   -----------------
   -- Put_Request --
   -----------------

   procedure Put_Request (S : String) is
   begin
      HTML.Put_Paragraph (Label    => "requested",
                          Contents => S);
   end Put_Request;


   --------------
   -- Put_List --
   --------------

   procedure Put_List (Show_Resources : Boolean) is
      Span : Positive := 5 + Balancer_Capability'Range_Length;
   begin
      if not Show_Resources then
         Span := Span + 1;
      end if;
      HTML.Begin_Div (Class => "job_list");
      Ada.Text_IO.Put ("<table><tr>");
      HTML.Put_Cell (Data       => "",
                     Tag        => "th",
                     Colspan => Span,
                     Class => "delimited");
      if Show_Resources then
         HTML.Put_Cell (Data => "Resource Usage",
                        Tag => "th",
                        Colspan => 3,
                        Class   => "delimited");
      end if;
      HTML.Put_Cell (Data => "Priority" & HTML.Help_Icon (Topic => "Job_priority"),
                     Tag => "th",
                     Colspan => 8,
                     Class => "delimited");
      Ada.Text_IO.Put ("</tr><tr>");
      Put_Core_Header;
      HTML.Put_Header_Cell (Data => "Submitted");
      HTML.Put_Header_Cell (Data => "Slots");
      HTML.Put_Header_Cell (Data => "State");
      if Show_Resources then
         HTML.Put_Header_Cell (Data => "CPU");
         HTML.Put_Header_Cell (Data => "Memory",
                            Acronym => "Gigabyte-hours");
         HTML.Put_Header_Cell (Data => "IO",
                            Acronym => "Gigabytes");
      else
         HTML.Put_Header_Cell (Data => "Res");
      end if;
      HTML.Put_Header_Cell (Data => "Priority");
      HTML.Put_Header_Cell (Data => "O",
                            Acronym => "Override");
      HTML.Put_Header_Cell (Data => "S",
                            Acronym => "Share");
      HTML.Put_Header_Cell (Data => "F",
                            Acronym => "Functional");
      HTML.Put_Header_Cell (Data => "Urgency");
      HTML.Put_Header_Cell (Data => "Resource");
      HTML.Put_Header_Cell (Data => "Waiting");
      HTML.Put_Header_Cell (Data => "Custom");
      Ada.Text_IO.Put ("</tr>");
      if Show_Resources then
         Iterate (Jobs.Put_Res_Line'Access);
      else
         Iterate (Jobs.Put_Prio_Line'Access);
      end if;
   end Put_List;

   -------------------
   -- Put_Time_List --
   -------------------

   procedure Put_Time_List is
   begin
      HTML.Begin_Div (Class => "job_list");
      Ada.Text_IO.Put ("<table><tr>");
      Put_Core_Header;
      HTML.Put_Header_Cell (Data => "Slots");
      HTML.Put_Header_Cell (Data => "Ends In");
      HTML.Put_Header_Cell (Data => "Ends At");
      HTML.Put_Header_Cell (Data => "State");
      Ada.Text_IO.Put ("</tr>");
      Iterate (Jobs.Put_Time_Line'Access);
         --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "job_list");
   end Put_Time_List;

   --------------------
   -- Put_Bunch_List --
   --------------------

   procedure Put_Bunch_List is
   begin
      HTML.Put_Heading (Title => "Jobs",
                        Level => 2);
      HTML.Begin_Div (Class => "job_list");
      Ada.Text_IO.Put_Line ("<table><tr>");
      Put_Core_Header;
      HTML.Put_Header_Cell (Data     => "PE");
      HTML.Put_Header_Cell (Data     => "Slots");
      HTML.Put_Header_Cell (Data     => "Hard");
      HTML.Put_Header_Cell (Data     => "Soft");
      HTML.Put_Header_Cell (Data     => "State");
      Ada.Text_IO.Put ("</tr>");
      Iterate (Jobs.Put_Bunch_Line'Access);

      --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "job_list");
   end Put_Bunch_List;


   -----------------
   -- Put_Details --
   -----------------

   procedure Put_Details is
   begin
      Iterate (Jobs.Put'Access);
   end Put_Details;

   ---------
   -- Put --
   ---------

   procedure Put (J : Job) is

      procedure Put_Actions is
      begin
         HTML.Begin_Div (ID => "job_actions");
         HTML.Put_Img (Name => "kill",
                       Text => "Kill job",
                       Link => HTML.Get_Action_URL (Action => "k",
                                                    Params => "j=" & Get_ID (J)));
         HTML.End_Div (ID => "job_actions");
      end Put_Actions;

      procedure Put_Name is
         procedure Put_Message (Message : String) is
         begin
            Ada.Text_IO.Put_Line ("<p class=""message"">"
                   & Message & "</p>");
         end Put_Message;

         procedure Put_Error (Message : String) is
         begin
            HTML.Comment (Message);
         end Put_Error;
      begin
         HTML.Begin_Div (Class => "job_name");
         Ada.Text_IO.Put ("<p>");
         HTML.Put_Img (Name => "hand.right",
                       Text => "unlock job manipulation",
                       Link => "#",
                       Extra_Args => "onclick=""document.getElementById('job_actions').style.display = 'block' """);
         --         HTML.Put_Paragraph ("Name", Get_Name (J));

         Ada.Text_IO.Put_Line ("Name: " & Get_Name (J) & "</p>");
         Iterate_Messages (J, Put_Message'Access);
         if Has_Errors (J) then
            Ada.Text_IO.Put_Line ("<em>Internal error log entries present</em>");
         end if;
         Iterate_Errors (J, Put_Error'Access);
         HTML.End_Div (Class => "job_name");
      end Put_Name;

      procedure Put_Meta is
      begin
         HTML.Begin_Div (Class => "job_meta");
         HTML.Put_Paragraph ("ID", Get_ID (J));
         HTML.Put_Paragraph ("Owner",  To_String (Get_Owner (J)));
         HTML.Put_Paragraph ("Group", Get_Group (J));
         HTML.Put_Paragraph ("Account", Get_Account (J));
         HTML.Put_Paragraph (Label    => "Submitted",
                             Contents => Get_Submission_Time (J));
         Iterate_Predecessors (J, Process => Put_Predecessor'Access);
         Iterate_Predecessor_Requests (J, Process => Put_Request'Access);
         Iterate_Successors (J, Process => Put_Successor'Access);
         HTML.Put_Link ("Advance Reservation", Get_Advance_Reservation (J), "ar_id");
         Ada.Text_IO.Put ("<p>Reserve: ");
         HTML.Put (Has_Reserve (J));
         Ada.Text_IO.Put_Line ("</p>");
         Ada.Text_IO.Put ("<p>State: ");
         Put_State (J);
         Ada.Text_IO.Put_Line ("</p>");
         HTML.Put_Clearer;
         HTML.End_Div (Class => "job_meta");
      end Put_Meta;

      procedure Put_Queues is

         Assigned_Queues, Detected_Queues, Marked_Queues : String_Sets.Set;

         procedure Put_Queue (Q : String) is
         begin
            HTML.Put_Paragraph (Label => "Queue", Contents => Q);
         end Put_Queue;

         procedure Put_Slot_Range (R : Step_Range) is
         begin
            Ranges.Put (R, Label => "Slots");
         end Put_Slot_Range;

         procedure Put_Task_Range (R : Step_Range) is
         begin
            Ranges.Put (R, Label => "Tasks");
         end Put_Task_Range;

      begin
         Assigned_Queues := Get_Task_List (J);
         Detected_Queues := Get_Detected_Queues (J);
         Marked_Queues := String_Sets.Symmetric_Difference (Assigned_Queues, Detected_Queues);

         HTML.Begin_Div (Class => "job_queue");
         HTML.Put_Heading (Title => "Requested",
                           Level => 3);
         Iterate_Queues (J, Put_Queue'Access);

         HTML.Put_Paragraph ("PE", Get_PE (J));
         HTML.Put_Paragraph ("Granted", Get_Granted_PE (J));
         Iterate_Slots (J, Put_Slot_Range'Access);
         Iterate_Tasks (J, Put_Task_Range'Access);

         HTML.Put_Heading (Title => "Assigned",
                           Level => 3);
         HTML.Put_Queue_List (Assigned_Queues, Marked_Queues);

         HTML.Put_Heading (Title => "Detected",
                           Level => 3);
         HTML.Put_Queue_List (Detected_Queues, Marked_Queues);

         HTML.Put_Clearer;
         HTML.End_Div (Class => "job_queue");
      end Put_Queues;

      procedure Put_Resources is
      begin
         HTML.Begin_Div (Class => "job_resources");
         HTML.Put_Heading (Title => "Hard",
                           Level => 3);
         Resources.Put_List (Get_Hard_Resources (J));

         HTML.Put_Heading (Title => "Soft",
                           Level => 3);
         Resources.Put_List (Get_Soft_Resources (J));
         HTML.End_Div (Class => "job_resources");
      end Put_Resources;

      procedure Put_Usage is
         JAT : constant SGE.Jobs.Usage := Get_JAT_Usage (J);
         PET : constant SGE.Jobs.Usage := Get_PET_Usage (J);
      begin
         HTML.Begin_Div (Class => "job_usage");
         HTML.Put_Heading (Title => "JAT",
                           Level => 3);
         for T in JAT'Range loop
            Put_Usage (T, JAT (T));
         end loop;
         HTML.Put_Heading (Title => "PET",
                           Level => 3);
         for T in PET'Range loop
            Put_Usage (T, PET (T));
         end loop;
         HTML.End_Div (Class => "job_usage");
      end Put_Usage;

      procedure Put_Files is
      begin
         HTML.Begin_Div (Class => "job_files");
         HTML.Put_Paragraph ("Directory", Get_Directory (J));
         HTML.Put_Paragraph ("Script", Get_Script_File (J));
         HTML.Put_Heading (Title => "Job Args",
                           Level => 3);
         HTML.Put_List (Get_Args (J));

         HTML.Put_Paragraph ("Executable", Get_Exec_File (J));
         Ada.Text_IO.Put ("<p>Merge StdErr: ");
         HTML.Put (Is_Merge_Std_Err (J));
         Ada.Text_IO.Put_Line ("</p>");
         HTML.Put_Heading (Title => "StdOut",
                           Level => 3);
         HTML.Put_List (Get_Std_Out_Paths (J));

         HTML.Put_Heading (Title => "StdErr",
                           Level => 3);
         HTML.Put_List (Get_Std_Err_Paths (J));
         Ada.Text_IO.Put ("<p>Notify: ");
         HTML.Put (Has_Notify (J));
         Ada.Text_IO.Put_Line ("</p>");
         HTML.End_Div (Class => "job_files");
      end Put_Files;

      procedure Put_Context is
         Slot_Range : SGE.Ranges.Step_Range_List;
         pragma Unreferenced (Slot_Range);
      begin
         HTML.Begin_Div (Class => "job_context");
         HTML.Put_Heading (Title => "Balancer",
                           Level => 3);
         if Supports_Balancer (J, CPU_GPU) then
            HTML.Put_Paragraph (Label => "Cores without GPU",
                                Contents => Get_CPU_Range (J));
            Slot_Range := To_Step_Range_List (Get_Context (J, SGE.Context.Slots_CPU));
            HTML.Put_Paragraph (Label => "Cores with GPU",
                                Contents => Get_GPU_Range (J));
            Try_Put_Paragraph (Label => "Last migration",
                               Getter => Get_Last_Migration'Access,
                               J => J);
         end if;
         if Supports_Balancer (J, Low_Cores) then
            HTML.Put_Paragraph (Label => "Reduce slots after",
                                Contents => SGE.Resources.Format_Duration (Get_Reduce_Wait (J)));
            HTML.Put_Paragraph (Label => "Reduce slots to",
                                Contents => Get_Reduced_Slots (J));
            Try_Put_Paragraph (Label => "Reduce runtime to",
                               Getter => Get_Reduced_Runtime'Access,
                              J => J);
            Try_Put_Paragraph (Label => "Last slot reduction",
                               Getter => Get_Last_Reduction'Access,
                               J => J);
         end if;
         if Supports_Balancer (J, High_Cores) then
            HTML.Put_Paragraph (Label    => "Extend slots to",
                                Contents => Get_Extended_Slots (J));
            Try_Put_Paragraph (Label => "Last slot extension",
                               Getter => Get_Last_Extension'Access,
                               J     => J);
         end if;

         HTML.Put_Heading (Title => "Other context",
                           Level => 3);
         HTML.Put_List_Head;
         if Has_Context (J) then
            Iterate_Context (J, HTML.Put_List_Entry'Access);
         else
            HTML.Put_Empty_List;
         end if;
         HTML.Put_List_Tail;
         HTML.End_Div (Class => "job_context");
      end Put_Context;

   begin
      HTML.Begin_Div (Class => "job_info");

      HTML.Begin_Div (Class => "action_and_name");
      Put_Actions;
      Put_Name;
      HTML.Put_Clearer;
      HTML.End_Div (Class => "action_and_name");
      Put_Meta;
      Put_Queues;
      HTML.Begin_Div (Class => "res_and_context");
      Put_Resources;
      Put_Context;
      HTML.End_Div (Class => "res_and_context");
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

   procedure Put_Core_Header is
   begin
      HTML.Put_Header_Cell (Data => "Number");
      for Capability in Balancer_Capability'Range loop
         if Capability /= Any then
            HTML.Put_Header_Cell (Data => ""); -- Balancer support
         end if;
      end loop;

      HTML.Put_Header_Cell (Data => "Owner");
      HTML.Put_Header_Cell (Data => "Name");
   end Put_Core_Header;

   procedure Put_Core_Line (J : Job) is
      Task_IDs : constant SGE.Ranges.Step_Range_List := Get_Task_IDs (J);
   begin
      if Is_Empty (Task_IDs) or else not Is_Collapsed (Task_IDs) then
         HTML.Put_Cell (Data       => Ada.Strings.Fixed.Trim (Get_ID (J), Ada.Strings.Left),
                        Link_Param => "job_id");
      else
         HTML.Put_Cell (Data       => Ada.Strings.Fixed.Trim (Get_ID (J), Ada.Strings.Left)
                                      & "-" & Ada.Strings.Fixed.Trim (Min (Task_IDs)'Img, Ada.Strings.Left),
                        Link_Param => "job_id");
      end if;
      for Capability in Balancer_Capability'Range loop
         if Capability /= Any then
            if Supports_Balancer (J, Capability) then
               HTML.Put_Img_Cell ("balance_" & To_String (Capability));
            else
               HTML.Put_Cell (Data => "");
            end if;
         end if;
      end loop;
      HTML.Put_Cell (Data => To_String (Get_Owner (J)), Link_Param => "user");
      HTML.Put_Cell (Data => Name_As_HTML (J));
   end Put_Core_Line;

   -------------------
   -- Put_Prio_Core --
   -------------------

   procedure Put_Prio_Core (J : Job) is
   begin
      HTML.Put_Cell (Data => Get_Priority (J)'Img);
      HTML.Put_Cell (Data => Get_Override_Tickets (J)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Share_Tickets (J)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Functional_Tickets (J)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Urgency (J)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Resource_Contrib (J)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Waiting_Contrib (J)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Posix_Priority (J)'Img, Class => "right");
   end Put_Prio_Core;


   -------------------
   -- Put_Time_Line --
   --  Purpose: Output one Job, including prospective end time, as a table row (tr).
   -------------------

   procedure Put_Time_Line (J : Job) is
   begin
      Start_Row (J);
      Put_Core_Line (J);

      HTML.Put_Cell (Data => Get_Slot_Number (J), Tag => "td class=""right""");
      begin
         HTML.Put_Duration_Cell (Remaining_Time (J));
      exception
         when Resources.Error =>
            HTML.Put_Cell (Data => "<i>unknown</i>", Class => "right");
      end;
      begin
         HTML.Put_Time_Cell (End_Time (J));
      exception
         when Resources.Error =>
            HTML.Put_Cell (Data => "<i>unknown</i>");
      end;
      Put_State_Cell (J);
      Finish_Row (J);
   exception
      when E :
         others => HTML.Error (Message => "Error while outputting job: "
                                     & Exception_Message (E));
         Finish_Row (J);
   end Put_Time_Line;

   -------------------
   -- Put_Bunch_Line --
   --  Purpose: Output one Job
   -------------------

   procedure Put_Bunch_Line (J : Job) is
   begin
      Start_Row (J);
      Put_Core_Line (J);
      HTML.Put_Cell (Data   => Get_PE (J));
      Ranges.Put_Cell (Data => Get_Slot_List (J), Class => "right");
      HTML.Put_Cell (Data   => Get_Hard_Resources (J));
      HTML.Put_Cell (Data   => Get_Soft_Resources (J));
      Put_State_Cell (J);
      Finish_Row (J);
   exception
      when E :
         others => HTML.Error (Message => "Error while outputting job: "
                                     & Exception_Message (E));
         Finish_Row (J);
   end Put_Bunch_Line;


   ------------------
   -- Put_Res_Line --
   --  Purpose: Output one Job, including resource usage, as a table row (tr)
   ------------------

   procedure Put_Res_Line (J : Job) is
   begin
      Start_Row (J);
      Put_Core_Line (J);

      HTML.Put_Time_Cell (Get_Submission_Time (J));
      HTML.Put_Cell (Data => Get_Slot_Number (J), Tag => "td class=""right""");
      Put_State_Cell (J);
      if Get_CPU (J) > 0.1 then
         HTML.Put_Duration_Cell (Integer (Get_CPU (J)));
      else
         HTML.Put_Cell ("");
      end if;
      if Get_Mem (J) > 3_600.0 then
         HTML.Put_Cell (Data  => Integer'Image (Integer (Get_Mem (J) / 3_600.0)),
                        Class => "right");
      elsif Get_Mem (J) > 1.0 then
         HTML.Put_Cell (Data  => HTML.Encode ("<1"),
                        Class => "right");
      else
         HTML.Put_Cell ("");
      end if;
      if Get_IO (J) > 1.0 then
         HTML.Put_Cell (Data  => Integer'Image (Integer (Get_IO (J))),
                        Class => "right");
      elsif Get_IO (J) > 0.01 then
         HTML.Put_Cell (Data  => "<1",
                        Class => "right");
      else
         HTML.Put_Cell ("");
      end if;
      Put_Prio_Core (J);
      Finish_Row (J);
   exception
      when E : others => HTML.Error (Message => "Error while outputting job: "
                                     & Exception_Message (E));
      Finish_Row (J);
   end Put_Res_Line;

   procedure Put_Prio_Line (J : Job) is
   begin
      Start_Row (J);
      Put_Core_Line (J);

      HTML.Put_Time_Cell (Get_Submission_Time (J));
      HTML.Put_Cell (Data => To_String (Get_Slot_List (J), Short => True),
                     Tag  => "td class=""right""");
      Put_State_Cell (J);
      Ada.Text_IO.Put ("<td>");
      HTML.Put (Has_Reserve (J));
      Ada.Text_IO.Put ("</td>");
      Put_Prio_Core (J);
      Finish_Row (J);
   exception
      when E : others => HTML.Error (Message => "Error while outputting job: "
                                     & Exception_Message (E));
      Finish_Row (J);
   end Put_Prio_Line;

   procedure Put_State_Cell (J : Job) is
   begin
      if Has_Error (J) then
         HTML.Put_Img_Cell (Get_State (J),
                            Extra_Text => " <a href=""" &
                              HTML.Get_Action_URL (Action => "cj", Params => "j=" & Get_ID (J)) & """>clear error</a>");
      else
         HTML.Put_Img_Cell (Get_State (J));
      end if;
   end Put_State_Cell;

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
         when start_time =>
            null; -- likewise
         when end_time =>
            null; -- likewise
         when priority =>
            null;
            --  ignore for now. It is unclear how one can "use" priority
            --  Put_Paragraph (Label => "Priority",
            --               Contents => Amount'Img);
         when exit_status =>
            null;
            --  ignore as well
         when signal =>
            null;
            --  ignore
         when ru_wallclock =>
            null; -- Bug #1752
         when ru_utime =>
            null; -- likewise
         when ru_stime =>
            null; -- likewise
         when ru_maxrss =>
            null; -- likewise
         when ru_ixrss =>
            null; -- likewise
      end case;
   end Put_Usage;

   procedure Start_Row (J : Job) is
   begin
      Ada.Text_IO.Put ("<tr");
      if Has_Error_Log_Entries (J) then
         Ada.Text_IO.Put (" class=""program_error""");
      elsif Quota_Inhibited (J) and then not Is_Running (J) then
         Ada.Text_IO.Put (" class=""job-quota""");
      end if;
      Ada.Text_IO.Put_Line (">");
   end Start_Row;

   procedure Finish_Row (J : Job) is
      procedure Put_Error (Message : String) is
      begin
         HTML.Comment (Message);
      end Put_Error;

   begin
      Ada.Text_IO.Put_Line ("</tr>");
      Iterate_Errors (J, Put_Error'Access);
   end Finish_Row;

   procedure Try_Put_Paragraph (Label  : String;
                                Getter : not null access function (J : Job) return String;
                                J      : Job) is
   begin
      HTML.Put_Paragraph (Label    => Label,
                     Contents => Getter (J));
   exception
      when others => null;
   end Try_Put_Paragraph;

   procedure Try_Put_Paragraph (Label  : String;
                                Getter : not null access function (J : Job) return Time;
                                J      : Job) is
      function Wrapper (J : Job) return String is
      begin
         return HTML.To_String (Getter (J));
      end Wrapper;

   begin
      Try_Put_Paragraph (Label  => Label,
                         Getter => Wrapper'Access,
                         J      => J);
   exception
      when others =>
         HTML.Put_Paragraph (Label    => Label,
                             Contents => "<em>never</em>");
   end Try_Put_Paragraph;

   --------------
   -- Overlays --
   --------------

   procedure Create_Overlay (Nodes : Node_List) is
   begin
      SGE.Jobs.Create_Overlay (Nodes);
   exception
      when E : others
         => HTML.Error ("Unable to read job info (Create_Overlay): " & Exception_Message (E));
   end Create_Overlay;

   procedure Apply_Overlay is
   begin
      SGE.Jobs.Apply_Overlay;
   end Apply_Overlay;


end Jobs;
