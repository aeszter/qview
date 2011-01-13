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
with Slots; use Slots; use Slots.Slot_Lists;
with Jobs; use Jobs; use Jobs.Job_Lists;
with Queues; use Queues; use Queues.Queue_Lists;
with Partitions; use Partitions; use Partitions.Partition_Lists;

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

      Ada.Text_IO.Put_Line ("<div class=""cqueues"">");
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
      HTML.Put_Cell ("<acronym title=""aoACDS: overloaded or in maintenance window"">Suspended</acronym>",
                     Tag => "th");
      HTML.Put_Cell ("<acronym title=""cdsuE: config problem, offline, disabled by admin, or node error"">Offline</acronym>",
                     Tag => "th");
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
         HTML.Put_Cell (Data => Q_Name, Link_Param => "queue");
         HTML.Put_Cell (Data => Q_Load);
         HTML.Put_Cell (Data => Q_Total, Class => "right");
         HTML.Put_Cell (Data => Q_Used, Class => "right");
         HTML.Put_Cell (Data => Q_Reserved, Class => "right");
         HTML.Put_Cell (Data => Q_Available, Class => "right");
         HTML.Put_Cell (Data => Q_Disabled, Class => "right");
         HTML.Put_Cell (Data => Q_Offline, Class => "right");

         Ada.Text_IO.Put ("</tr>");
      end loop;
      --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
            Ada.Text_IO.Put_Line ("</div>"); -- cqueues

      Reader.Free;
   end View_Cluster_Queues;


   procedure View_Jobs (Selector : String) is
      Reader      : DOM.Readers.Tree_Reader;
      SGE_Out     : DOM.Core.Document;
      SGE_Command : Pipe_Stream;
      List        : Node_List;
      Children    : Node_List;


      procedure Put_Table_Header is
      begin
         Ada.Text_IO.Put_Line ("<div class=""job_list"">");
         Ada.Text_IO.Put ("<table><tr>");
         HTML.Put_Header_Cell (Data => "Number", Params => My_Params);
         HTML.Put_Header_Cell (Data => "Owner", Params => My_Params);
         HTML.Put_Header_Cell (Data => "Name", Params => My_Params);
         HTML.Put_Header_Cell (Data => "Priority", Params => My_Params);
         HTML.Put_Header_Cell (Data => "Submitted", Params => My_Params);
         HTML.Put_Header_Cell (Data => "Slots", Params => My_Params);
         HTML.Put_Header_Cell (Data => "State", Params => My_Params);
         Ada.Text_IO.Put ("</tr>");
      end Put_Table_Header;

      procedure Put_Job_List_Entry (Job : Jobs.Job_Lists.Cursor) is
         J : Jobs.Job := Jobs.Job_Lists.Element (Job);
      begin
         Ada.Text_IO.Put ("<tr>");
         HTML.Put_Cell (Data => J.Number, Link_Param => "job_id");
         HTML.Put_Cell (Data => J.Owner, Link_Param => "user");
         HTML.Put_Cell (Data => J.Name);
         HTML.Put_Cell (Data => J.Priority);
         HTML.Put_Time_Cell (J.Submission_Time);
         HTML.Put_Cell (Data => J.Slots, Tag => "td class=""right""");
         HTML.Put_Img_Cell (State_As_String (J));
         Ada.Text_IO.Put ("</tr>");
      end Put_Job_List_Entry;


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

      Put_Table_Header;

      Jobs.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "job_list"));
      if not HTML.Param_Is ("sort", "") then
         Jobs.Sort_By (CGI.Value ("sort"));
      end if;
      Job_List.Iterate (Put_Job_List_Entry'Access);

   --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
      Ada.Text_IO.Put_Line ("</div>");
      Reader.Free;
   end View_Jobs;


   procedure View is

   begin
      CGI.Put_CGI_Header;
      Ada.Text_IO.Put_Line ("<html><head><title>Owl Status</title>");
      HTML.Put_Stylesheet ("/status.css");
      Ada.Text_IO.Put_Line ("</head><body>");
      Ada.Text_IO.Put_Line ("<div id=""page"">");
      Ada.Text_IO.Put_Line ("<div id=""header"">");
      CGI.Put_HTML_Heading (Title => "Owl Status", Level => 1);
      HTML.Put_Navigation_Begin;
      HTML.Put_Navigation_Link (Data => "Overview", Link_Param => "");
      HTML.Put_Navigation_Link ("All Jobs", "all_jobs=y");
      HTML.Put_Navigation_Link ("Waiting Jobs", "waiting_jobs=y");
      HTML.Put_Navigation_Link (Data       => "Detailed Queues",
                                Link_Param => "detailed_queues=y");
      HTML.Put_Navigation_End;
      Ada.Text_IO.Put_Line ("</div>"); -- header

      Ada.Text_IO.Put_Line ("<div id=""content"">");
      if not HTML.Param_Is ("detailed_queues", "y") then
         View_Cluster_Queues;
      end if;

      if CGI.Input_Received then
         if not HTML.Param_Is ("queue", "") then
            Set_Params ("queue=" & Sanitise (CGI.Value ("queue")));
            View_Jobs_In_Queue (Sanitise (CGI.Value ("queue")));
         elsif not HTML.Param_Is ("user", "") then
            Set_Params ("user=" & Sanitise (CGI.Value ("user")));
            View_Jobs_Of_User (Sanitise (CGI.Value ("user")));
         elsif not HTML.Param_Is ("job_id", "") then
            Set_Params ("job_id=" & Sanitise (CGI.Value ("job_id")));
            View_Job (Sanitise (CGI.Value ("job_id")));
         elsif HTML.Param_Is ("all_jobs", "y") then
            Set_Params ("all_jobs=y");
            View_Global_Jobs;
         elsif HTML.Param_Is ("waiting_jobs", "y") then
            Set_Params ("waiting_jobs=y");
            View_Waiting_Jobs;
         elsif HTML.Param_Is ("detailed_queues", "y") then
            Set_Params ("detailed_queues=y");
            View_Detailed_Queues;
         end if;
      end if;
      Ada.Text_IO.Put_Line ("</div>"); -- content
      HTML.Put_Clearer;
      Ada.Text_IO.Put_Line ("</div>"); -- page

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
      Slot_List   : Slots.Slot_Lists.List;


      J_Number          : Unbounded_String; -- Job ID
      J_Name            : Unbounded_String; -- Job name
      J_Owner           : Unbounded_String; -- User whom this job belongs to
      J_Group           : Unbounded_String;
      J_Priority        : Unbounded_String; -- Numerical priority
      J_State           : Unbounded_String; -- r(unning), qw(aiting) etc.
      J_Slots           : Unbounded_String; -- how many slots/CPUs to use
      J_PE              : Unbounded_String; -- Parallel environment
      J_Submission_Time : Unbounded_String; -- when submitted
      J_Exec_File       : Unbounded_String;
      J_Script_File     : Unbounded_String;
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
         Ranges : Node_List;
         N, R     : Node;
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
               Slot_List.Append (New_Range (Min  => Slots_Min,
                                            Max  => Slots_Max,
                                            Step => Slots_Step));
            end if;

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
         Slot_Range : Slot_Lists.Cursor;
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
         Slot_Range := Slot_List.First;
         loop
            exit when Slot_Range = Slots.Slot_Lists.No_Element;
            Slots.Put (Slots.Slot_Lists.Element (Slot_Range));
            Slot_Range := Next (Slot_Range);
         end loop;
         Ada.Text_IO.Put_Line ("</div>");

         Ada.Text_IO.Put_Line ("<div class=""job_resources"">");
         Res := Resource_List.First;
         loop
            exit when Res = Resources.Resource_Lists.No_Element;
            Resources.Put (Resources.Resource_Lists.Element (Res));
            Res := Next (Res);
         end loop;

         Ada.Text_IO.Put_Line ("</div>");

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
         HTML.Put_Clearer;
         Ada.Text_IO.Put ("</div> <!-- job_info -->");
         HTML.Put_Clearer;
      end loop;

      Reader.Free;
   exception
      when Sax.Readers.XML_Fatal_Error =>
         Ada.Text_IO.Put_Line ("<p><it>Job does not exist</it></p>");
         Reader.Free;
   end View_Job;

   --------------------------
   -- View_Detailed_Queues --
   --------------------------

   procedure View_Detailed_Queues is

      Queue_List : Queues.Queue_Lists.List;

      procedure Parse_One_Queue (Nodes : Node_List) is
         N : Node;
         A : Attr;
         Used, Reserved, Total : Natural := 0;
         State                 : Unbounded_String;
         Mem, Cores, Runtime   : Unbounded_String;
         Network               : Resources.Network := none;
         type small is digits 4 range 0.0 .. 1.0;
      begin
         for Index in 1 .. Length (Nodes) loop
            N := Item (Nodes, Index - 1);
            if Name (N) = "slots_used" then
               Used := Integer'Value (Value (First_Child (N)));
            elsif Name (N) = "slots_resv" then
               Reserved := Integer'Value (Value (First_Child (N)));
            elsif Name (N) = "slots_total" then
               Total := Integer'Value (Value (First_Child (N)));
            elsif Name (N) = "state" then
               State := To_Unbounded_String (Value (First_Child (N)));
            elsif Name (N) = "resource" then
               A := Get_Named_Item (Attributes (N), "name");
               if Value (A) = "mem_total" then
                  Mem := To_Unbounded_String (Value (First_Child (N)));
               elsif Value (A) = "num_proc" then
                  Cores := To_Unbounded_String (Value (First_Child (N)));
               elsif Value (A) = "infiniband" and then
                  small'Value (Value (First_Child (N))) = 1.0 then
                  Network := ib;
               elsif Value (A) = "ethernet" and then
                  small'Value (Value (First_Child (N))) = 1.0 then
                  Network := eth;
               elsif Value (A) = "h_rt" then
                  Runtime := To_Unbounded_String (Value (First_Child (N)));
               else
                  HTML.Put_Paragraph (Label    => "Unidentified Resource",
                                      Contents => Value (A));
               end if;
            end if;
         end loop;

         Queue_List.Append (New_Queue (Used => Used,
                                       Reserved => Reserved,
                                       Total    => Total,
                                       Memory   => To_String (Mem),
                                       Cores    => To_String (Cores),
                                       Network  => Network,
                                       Runtime  => Runtime,
                                       State    => To_String (State)
                                      ));
      exception
            when E : others =>
            HTML.Put_Paragraph (Label    => "Failed to parse queue",
                                Contents => Exception_Message (E));
      end Parse_One_Queue;

      function Setup_Parser return DOM.Core.Document is
         Reader      : DOM.Readers.Tree_Reader;
         SGE_Command : Pipe_Stream;
      begin
         SGE_Command.Set_Public_Id ("qstat");
         SGE_Command.execute ("SGE_ROOT=" & sgeroot & " " & sgeroot
           & "/bin/lx26-amd64/qstat -F h_rt,eth,ib,mem_total,num_proc -xml"
           & ASCII.NUL,
           Pipe_Commands.read_file);
         Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
         Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
         Reader.Parse (SGE_Command);
         SGE_Command.Close;
         return Reader.Get_Tree;
      end Setup_Parser;

      procedure Put_Partition (Partition : Partitions.Partition_Lists.Cursor) is
         P : Partitions.Partition := Partitions.Partition_Lists.Element (Partition);
      begin
         if P.Available > 0 then
            Ada.Text_IO.Put ("<tr class=""available"">");
         elsif P.Offline = P.Total then
            Ada.Text_IO.Put ("<tr class=""offline"">");
         else
            Ada.Text_IO.Put ("<tr>");
         end if;
         HTML.Put_Cell (Data => P.Network'Img);
         HTML.Put_Cell (Data => P.Cores'Img, Class => "right");
         HTML.Put_Cell (Data => P.Memory'Img & "G", Class => "right");
         HTML.Put_Cell (Data => P.Runtime, Class => "right");
         HTML.Put_Cell (Data => P.Total'Img, Class => "right");
         HTML.Put_Cell (Data => P.Used'Img, Class => "right");
         HTML.Put_Cell (Data => P.Reserved'Img, Class => "right");
         HTML.Put_Cell (Data => P.Available'Img, Class => "right");
         HTML.Put_Cell (Data => P.Suspended'Img, Class => "right");
         HTML.Put_Cell (Data => P.Offline'Img, Class => "right");
         Ada.Text_IO.Put ("</tr>");
      end Put_Partition;


      SGE_Out        : DOM.Core.Document;
      Nodes          : Node_List;
      Partition_List : Partitions.Partition_Lists.List;
   begin
      Ada.Text_IO.Put ("<div class=""partitions"">");
      CGI.Put_HTML_Heading (Title => "Detailed Queue Information",
                            Level => 2);
      SGE_Out := Setup_Parser;

      --  Fetch Queues
      Nodes := Get_Elements_By_Tag_Name (SGE_Out, "Queue-List");
      if Length (Nodes) = 0 then
         Ada.Text_IO.Put_Line ("<i>No queues found</i>");
         return;
      end if;

      for Index in 1 .. Length (Nodes) loop
         Parse_One_Queue (Child_Nodes (Item (Nodes, Index - 1)));
      end loop;

      --  Detect different partitions
      Partitions.Build_List (Queue_List, Partition_List);

      --  Output
      Ada.Text_IO.Put_Line ("<table><tr>");
      HTML.Put_Cell (Data => "Interconnect", Tag => "th");
      HTML.Put_Cell (Data       => "Cores", Tag => "th");
      HTML.Put_Cell (Data => "RAM", Tag => "th");
      HTML.Put_Cell (Data => "Runtime", Tag => "th");
      HTML.Put_Cell (Data => "Slots", Tag => "th");
      HTML.Put_Cell (Data => "Used", Tag => "th");
      HTML.Put_Cell (Data => "Reserved", Tag => "th");
      HTML.Put_Cell (Data => "Available", Tag => "th");
      HTML.Put_Cell (Data => "<acronym title=""aoACDS: overloaded or in maintenance window"">Suspended</acronym>", Tag => "th");
      HTML.Put_Cell ("<acronym title=""cdsuE: config problem, offline, disabled by admin, or node error"">Offline</acronym>", Tag => "th");
      Ada.Text_IO.Put_Line ("</tr>");
      Partition_List.Iterate (Put_Partition'Access);
      Ada.Text_IO.Put_Line ("</table>");
      Ada.Text_IO.Put ("</div> <!-- partitions -->");
   end View_Detailed_Queues;

   procedure Set_Params (Params : String) is
   begin
      My_Params := To_Unbounded_String (Params);
   end Set_Params;


end Viewer;
