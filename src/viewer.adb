with Ada.Text_IO, CGI, System, Pipe_Commands, Pipe_Streams;
use  Pipe_Streams;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Sax.Readers, Sax.Exceptions;
with DOM.Readers, DOM.Core;
use  DOM.Core;
with DOM.Core.Documents, DOM.Core.Nodes, DOM.Core.Attrs;
use  DOM.Core.Documents, DOM.Core.Nodes, DOM.Core.Attrs;
with HTML;
with Command_Tools; use Command_Tools;
with Ada.Exceptions; use Ada.Exceptions;
with Resources; use Resources; use Resources.Resource_Lists;

package body Viewer is

   procedure View_Cluster_Queues is
      Reader      : DOM.Readers.Tree_Reader;
      SGE_Out     : DOM.Core.Document;
      SGE_Command : Pipe_Stream;
      List        : Node_List;
      Children    : Node_List;
      N           : Node;
      C           : Node;

      Q_Name      : Unbounded_String;
      Q_Load      : Unbounded_String;
      Q_Used      : Unbounded_String;
      Q_Reserved  : Unbounded_String;
      Q_Available : Unbounded_String;
      Q_Total     : Unbounded_String;
      Q_Disabled  : Unbounded_String;
      Q_Offline   : Unbounded_String;
   --  Cluster Queue Statistics
   begin
      SGE_Command.Set_Public_Id ("qstat");
      SGE_Command.execute
        ("SGE_ROOT=" &
         sgeroot &
         " " &
         sgeroot &
         "/bin/lx26-amd64/qstat -g c -xml" &
         ASCII.NUL,
         Pipe_Commands.read_file);
      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (SGE_Command);
      SGE_Out := Reader.Get_Tree;
      SGE_Command.Close;

      CGI.Put_HTML_Heading (Title => "Cluster Queues", Level => 2);
      --  Fetch Cluster Queues
      List := Get_Elements_By_Tag_Name (SGE_Out, "cluster_queue_summary");
      --  Table Header
      Ada.Text_IO.Put ("<table><tr>");
      HTML.Put_Cell (Data => "Queue", Tag => "th");
      HTML.Put_Cell ("Load", Tag => "th");
      HTML.Put_Cell ("Slots", Tag => "th");
      HTML.Put_Cell ("Used", Tag => "th");
      HTML.Put_Cell ("Reserved", Tag => "th");
      HTML.Put_Cell ("Available", Tag => "th");
      HTML.Put_Cell ("Disabled", Tag => "th");
      HTML.Put_Cell ("Offline", Tag => "th");
      Ada.Text_IO.Put ("</tr>");

      for Index in 1 .. Length (List) loop
         Ada.Text_IO.Put ("<tr>");
         N        := Item (List, Index - 1);
         Children := Child_Nodes (N);
         for Ch_Index in 0 .. Length (Children) - 1 loop
            C := Item (Children, Ch_Index);
            if Name (C) = "name" then
               Q_Name := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "load" then
               Q_Load := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "used" then
               Q_Used := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "resv" then
               Q_Reserved := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "available" then
               Q_Available := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "total" then
               Q_Total := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "temp_disabled" then
               Q_Disabled := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "manual_intervention" then
               Q_Offline := To_Unbounded_String (Value (First_Child (C)));
            end if;
         end loop;
         HTML.Put_UCell_With_Link (Q_Name, "queue");
         HTML.Put_UCell (Q_Load);
         HTML.Put_UCell (Q_Total);
         HTML.Put_UCell (Q_Used);
         HTML.Put_UCell (Q_Reserved);
         HTML.Put_UCell (Q_Available);
         HTML.Put_UCell (Q_Disabled);
         HTML.Put_UCell (Q_Offline);

         Ada.Text_IO.Put ("</tr>");
      end loop;
      --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
      Reader.Free;
   end View_Cluster_Queues;


   procedure View_Jobs (Selector : String) is
      Reader      : DOM.Readers.Tree_Reader;
      SGE_Out     : DOM.Core.Document;
      SGE_Command : Pipe_Stream;
      List        : Node_List;
      Children    : Node_List;
      N           : Node;
      C           : Node;

      J_Number          : Unbounded_String; -- Job ID
      J_Full_Name       : Unbounded_String; -- Job name
      J_Name            : Unbounded_String; -- Job name, truncated to Max_J_Name_Length
      J_Owner           : Unbounded_String; -- User whom this job belongs to
      J_Priority        : Unbounded_String; -- Numerical priority
      J_State           : Unbounded_String; -- r(unning), qw(aiting) etc.
      J_Slots           : Unbounded_String; -- how many slots/CPUs to use
      J_PE              : Unbounded_String; -- Parallel environment
      J_Submission_Time : Unbounded_String; -- when submitted
      --  Job List Entries
   begin
      SGE_Command.Set_Public_Id ("qstat");
      SGE_Command.execute ("SGE_ROOT=" & sgeroot & " " &
         sgeroot & "/bin/lx26-amd64/qstat " & Selector & " -xml" & ASCII.NUL,
         Pipe_Commands.read_file);
      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (SGE_Command);
      SGE_Out := Reader.Get_Tree;
      SGE_Command.Close;

   --  Fetch Jobs
      List := Get_Elements_By_Tag_Name (SGE_Out, "job_list");
   --  Table Header
      Ada.Text_IO.Put ("<table><tr>");
      HTML.Put_Cell (Data => "Number", Tag => "th");
      HTML.Put_Cell (Data => "Owner", Tag => "th");
      HTML.Put_Cell (Data => "Name", Tag => "th");
      HTML.Put_Cell (Data => "Priority", Tag => "th");
      HTML.Put_Cell (Data => "Submitted", Tag => "th");
      HTML.Put_Cell (Data => "Slots", Tag => "th");
      HTML.Put_Cell (Data => "State", Tag => "th");
      Ada.Text_IO.Put ("</tr>");

      for Index in 1 .. Length (List) loop
         Ada.Text_IO.Put ("<tr>");
         N        := Item (List, Index - 1);
         Children := Child_Nodes (N);
         for Ch_Index in 0 .. Length (Children) - 1 loop
            C := Item (Children, Ch_Index);
            if Name (C) = "JB_job_number" then
               J_Number := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JAT_prio" then
               J_Priority := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_name" then
               J_Full_Name := To_Unbounded_String (Value (First_Child (C)));
               if Length (J_Full_Name) > Max_J_Name_Length then
                  J_Name := Head (Source => J_Full_Name,
                                  Count  => Max_J_Name_Length);
               else
                  J_Name := J_Full_Name;
               end if;
            elsif Name (C) = "JB_owner" then
               J_Owner := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "state" then
               J_State := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_submission_time" then
               J_Submission_Time :=
               To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "slots" then
               J_Slots := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "requested_pe" then
               J_PE := To_Unbounded_String (Value (First_Child (C)));
            end if;
         end loop;
         HTML.Put_UCell_With_Link (Data => J_Number, Link_Param => "job_id");
         HTML.Put_UCell_With_Link (Data => J_Owner, Link_Param => "user");
         HTML.Put_UCell (Data => J_Name);
         HTML.Put_UCell (Data => J_Priority);
         HTML.Put_UCell (Data => J_Submission_Time);
         HTML.Put_UCell (Data => J_Slots);
         HTML.Put_UCell (Data => J_State);
         Ada.Text_IO.Put ("</tr>");
      end loop;

   --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
      Reader.Free;
   end View_Jobs;


   procedure View is

   begin
      CGI.Put_CGI_Header;
      Ada.Text_IO.Put_Line ("<html><head><title>Owl Status</title>");
      Ada.Text_IO.Put_Line ("<link style=status.css />");
      Ada.Text_IO.Put_Line ("</head><body>");
      CGI.Put_HTML_Heading (Title => "Owl Status", Level => 1);
      HTML.Put_Navigation_Begin;
      HTML.Put_Navigation_Link ("All Jobs", "all_jobs=y");
      HTML.Put_Navigation_Link ("Waiting Jobs", "waiting_jobs=y");
      HTML.Put_Navigation_End;

      View_Cluster_Queues;

      if CGI.Input_Received then
         if not Param_Is ("queue", "") then
            View_Jobs_In_Queue (Sanitise (CGI.Value ("queue")));
         elsif not Param_Is ("user", "") then
            View_Jobs_Of_User (Sanitise (CGI.Value ("user")));
         elsif not Param_Is ("job_id", "") then
            View_Job (Sanitise (CGI.Value ("job_id")));
         elsif Param_Is ("all_jobs", "y") then
            View_Global_Jobs;
         elsif Param_Is ("waiting_jobs", "y") then
            View_Waiting_Jobs;
         end if;
      end if;
      CGI.Put_HTML_Tail;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("<p><strong>Warning: Unhandled Exception occurred.</strong>");
         Ada.Text_IO.Put_Line ("<it>" & Exception_Message (E) & "</it></p>");
         CGI.Put_HTML_Tail;

   end View;

   procedure View_Jobs_In_Queue (Queue : String) is
   begin
      CGI.Put_HTML_Heading (Title => """" & Queue & """ Jobs", Level => 2);
      View_Jobs ("-u \* -s r -q " & Queue);
   end View_Jobs_In_Queue;

   procedure View_Global_Jobs is
   begin
      CGI.Put_HTML_Heading (Title => "All Jobs", Level => 2);
      View_Jobs ("-u \*");
   end View_Global_Jobs;

   procedure View_Waiting_Jobs is
   begin
      CGI.Put_HTML_Heading (Title => "Pending Jobs", Level => 2);
      View_Jobs ("-u \* -s p");
   end View_Waiting_Jobs;

   procedure View_Jobs_Of_User (User : String) is
   begin
      CGI.Put_HTML_Heading (Title => "Jobs of " & User,
                            Level => 2);
      View_Jobs ("-u " & User);
   end View_Jobs_Of_User;

   procedure View_Job (Job_ID : String) is
      Reader      : DOM.Readers.Tree_Reader;
      SGE_Out     : DOM.Core.Document;
      SGE_Command : Pipe_Stream;
      List        : Node_List;
      Children    : Node_List;
      N           : Node;
      C           : Node;
      Resource_List : Resources.Resource_Lists.List;


      J_Number          : Unbounded_String; -- Job ID
      J_Name            : Unbounded_String; -- Job name
      J_Owner           : Unbounded_String; -- User whom this job belongs to
      J_Group           : Unbounded_string;
      J_Priority        : Unbounded_String; -- Numerical priority
      J_State           : Unbounded_String; -- r(unning), qw(aiting) etc.
      J_Slots           : Unbounded_String; -- how many slots/CPUs to use
      J_PE              : Unbounded_String; -- Parallel environment
      J_Submission_Time : Unbounded_String; -- when submitted
      J_Exec_File       : Unbounded_String;
      J_script_File     : Unbounded_String;
      J_Merge_Std_Err   : Unbounded_String;
      J_Array           : Unbounded_String;
      J_Queue           : Unbounded_String;
      J_Directory       : Unbounded_String;
      J_Reserve         : Unbounded_String;

      procedure Extract_Resource_List is
         Resource_Nodes : Node_List := Child_Nodes (C);
         Resource_Tags      : Node_List;

         N, R          : Node;
         Res_Value     : Unbounded_String;
         Res_Name      : Unbounded_String;

      begin
         for I in 1 .. Length (Resource_Nodes) loop
            N := Item (Resource_Nodes, I - 1);
            if Name (N) = "qstat_l_requests" then
               Resource_Tags := Child_Nodes (N);
               for J in 1 .. Length (Resource_Tags) loop
                  R := Item (Resource_Tags, J - 1);
                  if Name (R) = "CE_name" then
                     Res_Name := To_Unbounded_String (Value (First_Child (R)));
                  elsif Name (R) = "CE_stringval" then
                     Res_Value := To_Unbounded_String (Value (First_Child (R)));
                     --  maybe check for relop here?
                  end if;
               end loop;
               Resource_List.Append (New_Resource (Name  => Res_Name,
                                                  Value => Res_Value));
            end if;
         end loop;
      end Extract_Resource_List;

      procedure Extract_PE_Range is
         Children : Node_List := Child_Nodes (C);
         N : Node;
      begin
         for I in 1 .. Length (Children) loop
            N := Item (Children, I - 1);

            Ada.Text_IO.Put_Line ("N  => " & Name (N) & " V => " & Value (N));
         end loop;
      end Extract_PE_Range;

      procedure Parse_One_Job is
      begin
         Resource_List.Clear;
         for Ch_Index in 0 .. Length (Children) - 1 loop
            C := Item (Children, Ch_Index);
            if Name (C) = "JB_job_number" then
               J_Number := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_ar" then
               J_Array := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_exec_file" then
               J_Exec_File := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_owner" then
               J_Owner := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_group" then
               J_Group := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_merge_stderr" then
               J_Merge_Std_Err := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_job_name" then
               J_Name := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_hard_resource_list" then
               Extract_Resource_List;
            elsif Name (C) = "JB_hard_queue_list" then
               J_Queue := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_script_file" then
               J_Script_File := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_cwd" then
               J_Directory := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_reserve" then
               J_Reserve := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_pe" then
               J_PE := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_pe_range" then
               Extract_PE_Range;
            end if;
         end loop;
      end Parse_One_Job;

      procedure Output_One_Job is
         Res : Resource_Lists.Cursor;
      begin
         Ada.Text_IO.Put_Line ("<div class=""job_name"">");
         HTML.Put_Paragraph ("Name", J_Name);
         Ada.Text_IO.Put_Line ("</div>");

         Ada.Text_IO.Put_Line ("<div class=""job_meta"">");
         HTML.Put_Paragraph ("ID", J_Number);
         HTML.Put_Paragraph ("Owner", J_Owner);
         HTML.Put_Paragraph ("Group", J_Group);
         Ada.Text_IO.Put ("<p>Array: ");
         HTML.Put_True_False (J_Array);
         Ada.Text_IO.Put_Line ("</p>");
         Ada.Text_IO.Put ("<p>Reserve: ");
         HTML.Put_True_False (J_Reserve);
         Ada.Text_IO.Put_Line ("</p>");
         Ada.Text_IO.Put_Line ("</div>");

         Ada.Text_IO.Put_Line ("<div class=""job_files"">");
         HTML.Put_Paragraph ("Directory", J_Directory);
         HTML.Put_Paragraph ("Script", J_Script_File);
         HTML.Put_Paragraph ("Executable", J_Exec_File);
         Ada.Text_IO.Put ("<p>Merge StdErr: ");
         HTML.Put_True_False (J_Merge_Std_Err);
         Ada.Text_IO.Put_Line ("</p>");
         Ada.Text_IO.Put_Line ("</div>");

         Ada.Text_IO.Put_Line ("<div class=""job_queue"">");
         HTML.Put_Paragraph ("Queue", J_Queue);
         HTML.Put_Paragraph ("PE", J_PE);
         Ada.Text_IO.Put_Line ("</div>");

         Ada.Text_IO.Put_Line ("<div class=""job_resources"">");
         Res := Resource_List.First;
         loop
            exit when Res = No_Element;
            HTML.Put_Paragraph (Label => Resource_Lists.Element (Res).Name,
                                Contents => Resource_Lists.Element (Res).Value);
            Res := Next (Res);
         end loop;

         Ada.Text_IO.Put_Line ("</div>");

         Ada.Text_IO.Put ("</div>");
      end Output_One_Job;


   begin
      CGI.Put_HTML_Heading (Title => "Details of Job " & Job_ID,
                            Level => 2);
      SGE_Command.Set_Public_Id ("qstat");
      SGE_Command.execute ("SGE_ROOT=" & sgeroot & " " &
         sgeroot & "/bin/lx26-amd64/qstat -j " & Job_ID & " -xml" & ASCII.NUL,
         Pipe_Commands.read_file);
      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (SGE_Command);
      SGE_Out := Reader.Get_Tree;
      SGE_Command.Close;

   --  Fetch Jobs
      List := Get_Elements_By_Tag_Name (SGE_Out, "djob_info");
      if Length (List) = 0 then
         Ada.Text_IO.Put_Line ("<i>unknown job</i>");
         return;
      end if;

      for Index in 1 .. Length (List) loop

         Ada.Text_IO.Put ("<div class=""job_info"">");
         Children := Child_Nodes (Item (List, Index - 1)); -- djob_info
         N := Item (Children, 1);
         --  element; this is probably faster than
         --  looping through the list;
         --  in case we err, raise an exception
         if Name (N) /= "element" then
            raise Assumption_Error;
         end if;
         Children := Child_Nodes (N); -- data fields of the job

         Parse_One_Job;
         Output_One_Job;
      end loop;

      Reader.Free;
   exception
      when Sax.Readers.XML_Fatal_Error =>
         Ada.Text_IO.Put_Line ("<p><it>Job does not exist</it></p>");
         Reader.Free;
   end View_Job;

   function Param_Is (Param : String; Expected : String) return Boolean is
   begin
      return Standard."=" (CGI.Value (Param), Expected);
   end Param_Is;


end Viewer;
