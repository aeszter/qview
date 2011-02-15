with Ada.Text_IO, CGI, Pipe_Commands, Pipe_Streams;
use  Pipe_Streams;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Sax.Readers;
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
with Bunches; use Bunches; use Bunches.Bunch_Lists;
with Queues; use Queues; use Queues.Queue_Lists;
with Partitions; use Partitions; use Partitions.Partition_Lists;
with Utils; use Utils; use Utils.String_Lists;
with Diagnostics;
with Ada.Strings.Fixed;

package body Viewer is

   ----------
   -- View --
   --  Purpose: Main routine, display the entire page
   ----------

   procedure View is

      procedure Put_Headers is
      begin
         CGI.Put_CGI_Header;
         Ada.Text_IO.Put_Line ("<html><head><title>Owl Status</title>");
         HTML.Put_Stylesheet ("/status.css");
         Ada.Text_IO.Put_Line ("</head><body>");
         HTML.Begin_Div (ID => "page");
         HTML.Begin_Div (ID => "header");
         CGI.Put_HTML_Heading (Title => "Owl Status", Level => 1);
         HTML.Put_Navigation_Begin;
         HTML.Put_Navigation_Link (Data => "Overview", Link_Param => "");
         HTML.Put_Navigation_Link ("All Jobs", "all_jobs=y");
         HTML.Put_Navigation_Link ("Waiting Jobs", "waiting_jobs=y");
         HTML.Put_Navigation_Link (Data       => "Detailed Queues",
                                   Link_Param => "categories=supply");
         HTML.Put_Navigation_Link (Data       => "Job Overview",
                                   Link_Param => "categories=demand");
         HTML.Put_Navigation_Link (Data       => CGI.HTML_Encode ("Supply & Demand"),
                                   Link_Param => "categories=both");
         HTML.Put_Navigation_Link (Data       => "Finishing Jobs",
                                   Link_Param => "forecast=y");
         HTML.Put_Navigation_End;
         HTML.End_Div (ID => "header");
      end Put_Headers;

      procedure Put_Diagnostics is
      begin
         Ada.Text_IO.Put ("<li>");
         Ada.Text_IO.Put ("Time:");
         Diagnostics.Put_Time;
         Ada.Text_IO.Put ("</li>");
      end Put_Diagnostics;


      procedure Put_Footer is
      begin
         HTML.Begin_Div (ID => "footer");
         Ada.Text_IO.Put ("<ul>");
         Ada.Text_IO.Put_Line ("<li><a href=""mailto:aeszter@gwdg.de"">"
                               & "aeszter@gwdg.de</a></li>");
         Ada.Text_IO.Put_Line ("<li><a href=""/usage"">"
                               & "<img src=""/usage/webalizer.png"" "
                               & "alt=""Stats by Webalizer""></a></li>");
         Ada.Text_IO.Put_Line ("<li><a href=""http://ram/bugzilla/enter_bug.cgi?"
                               & "component=qview&form_name=enter_bug"
                               & "&product=Private%20projects"">"
                               &"Report Problem/Suggest Enhancement</a></li>");
         Put_Diagnostics;
         HTML.Put_Navigation_Link (Data       => "Profiling Mode",
                                   Link_Param => "profile=10");
         Ada.Text_IO.Put ("</ul>");
         HTML.End_Div (ID =>  "footer");
         HTML.Put_Clearer;
         HTML.End_Div (ID => "page");
      end Put_Footer;
      Sort_Direction : Unbounded_String := To_Unbounded_String ("inc");

      procedure View_Cluster_Queues is
         SGE_Out     : DOM.Core.Document;
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
         SGE_Out := Setup_Parser (Selector => "-g c");

         HTML.Begin_Div (Class => "cqueues");
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
         HTML.End_Div (Class => "cqueues");

      end View_Cluster_Queues;

      -----------------------
      -- View_Job_Overview --
      -----------------------

      procedure View_Job_Overview is


         procedure Put_Bunch (Bunch : Bunches.Bunch_Lists.Cursor) is
            B : Bunches.Bunch := Bunches.Bunch_Lists.Element (Bunch);
         begin
            if B.Error > 0 then
               Ada.Text_IO.Put ("<tr class=""job-error"">");
            elsif B.Waiting = 0 then
               Ada.Text_IO.Put ("<tr class=""job-held"">");
            else
               Ada.Text_IO.Put ("<tr>");
            end if;
            HTML.Put_Cell (Data => B.PE);
            HTML.Put_Cell (Data => B.Slots, Class => "right");
            HTML.Put_Cell (Data => B.Queue);
            HTML.Put_Cell (Data => To_Unbounded_String (B.Hard));
            HTML.Put_Cell (Data => To_Unbounded_String (B.Soft));
            HTML.Put_Cell (Data => B.Total'Img, Class => "right");
            HTML.Put_Cell (Data => B.Waiting'Img, Class => "right");
            HTML.Put_Cell (Data => B.On_Hold'Img, Class => "right");
            HTML.Put_Cell (Data => B.Error'Img, Class => "right");
            Ada.Text_IO.Put ("</tr>");
         end Put_Bunch;


         SGE_Out     : DOM.Core.Document;
         Nodes       : Node_List;
         Bunch_List : Bunches.Bunch_Lists.List;
      begin
         HTML.Begin_Div (Class => "bunches");
         if HTML.Param_Is ("categories", "demand") then
            CGI.Put_HTML_Heading (Title => "Job Overview",
                               Level => 2);
         else
            CGI.Put_HTML_Heading (Title => "Demand",
                               Level => 2);
         end if;
         SGE_Out := Setup_Parser (Selector => "-u \* -r -s p");

         --  Fetch Queues
         Nodes := Get_Elements_By_Tag_Name (SGE_Out, "job_list");
         if Length (Nodes) = 0 then
            Ada.Text_IO.Put_Line ("<i>No jobs found</i>");
            return;
         end if;

         Jobs.Append_List (Nodes);

         --  Detect different bunches
         Bunches.Build_List (Job_List, Bunch_List);

         --  Output
         Ada.Text_IO.Put_Line ("<table><tr>");
         HTML.Put_Cell (Data => "<acronym title=""Parallel Environment"">PE</acronym>",
                        Tag => "th");
         HTML.Put_Cell (Data => "Slots", Tag => "th");
         HTML.Put_Cell (Data => "Queue", Tag => "th");
         HTML.Put_Cell (Data => "Hard Requests", Tag => "th");
         HTML.Put_Cell (Data => "Soft Requests", Tag => "th");
         HTML.Put_Cell (Data => "Total", Tag => "th");
         HTML.Put_Cell (Data => "Waiting", Tag => "th");
         HTML.Put_Cell (Data => "Held", Tag => "th");
         HTML.Put_Cell (Data => "Error", Tag => "th");
         Ada.Text_IO.Put_Line ("</tr>");
         Bunch_List.Iterate (Put_Bunch'Access);
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "bunches");
      end View_Job_Overview;


      --------------------------
      -- View_Detailed_Queues --
      --------------------------

      procedure View_Detailed_Queues is

         Queue_List : Queues.Queue_Lists.List;

         procedure Parse_One_Queue (Nodes : Node_List) is
            N                     : Node;
            A                     : Attr;
            Used, Reserved, Total : Natural := 0;
            State                 : Unbounded_String;
            Mem, Runtime          : Unbounded_String;
            Cores                 : Natural;
            Network               : Resources.Network := none;
            Model                 : Unbounded_String := Null_Unbounded_String;
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
                     Cores := Integer'Value (Value (First_Child (N)));
                  elsif Value (A) = "infiniband" and then
                    small'Value (Value (First_Child (N))) = 1.0 then
                     Network := ib;
                  elsif Value (A) = "ethernet" and then
                    small'Value (Value (First_Child (N))) = 1.0 then
                     Network := eth;
                  elsif Value (A) = "h_rt" then
                     Runtime := To_Unbounded_String (Value (First_Child (N)));
                  elsif Value (A) = "cpu_model" then
                     Model := To_Unbounded_String (Value (First_Child (N)));
                  end if;
               end if;
            end loop;

            Queue_List.Append (New_Queue (Used => Used,
                                          Reserved => Reserved,
                                          Total    => Total,
                                          Memory   => To_String (Mem),
                                          Cores    => Cores,
                                          Network  => Network,
                                          Model    => To_Model (Model),
                                          Runtime  => Runtime,
                                          State    => To_String (State)
                                         ));
         exception
            when E : others =>
               HTML.Put_Paragraph (Label    => "Failed to parse queue",
                                   Contents => Exception_Message (E));
         end Parse_One_Queue;


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
            HTML.Put_Cell (Data => Model_As_String (P));
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
         HTML.Begin_Div (Class => "partitions");
         if HTML.Param_Is ("categories", "supply") then
            CGI.Put_HTML_Heading (Title => "Detailed Queue Information",
                               Level => 2);
         else
            CGI.Put_HTML_Heading (Title => "Supply",
                               Level => 2);
         end if;

         SGE_Out := Setup_Parser (Selector => "-F h_rt,eth,ib,mem_total,num_proc,cm");

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
         HTML.Put_Cell (Data => "CPU<a href=""http://wiki.mpibpc.gwdg.de"
                           & "/grubmueller/index.php/CPU Families"">"
                           & "<img src=""/icons/help.png"" /></a>", Tag => "th");
         HTML.Put_Cell (Data => "Cores", Tag => "th");
         HTML.Put_Cell (Data => "RAM", Tag => "th");
         HTML.Put_Cell (Data => "Runtime", Tag => "th");
         HTML.Put_Cell (Data => "Slots", Tag => "th");
         HTML.Put_Cell (Data => "Used", Tag => "th");
         HTML.Put_Cell (Data => "Reserved", Tag => "th");
         HTML.Put_Cell (Data => "Available", Tag => "th");
         HTML.Put_Cell (Data => "<acronym title=""d: disabled by admin or"">Suspended</acronym>", Tag => "th");
         HTML.Put_Cell ("<acronym title=""u: unreacheable"">Offline</acronym>", Tag => "th");
         Ada.Text_IO.Put_Line ("</tr>");
         Partition_List.Iterate (Put_Partition'Access);
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "partitions");
      end View_Detailed_Queues;

      procedure View_Jobs (Selector : String) is
         SGE_Out     : DOM.Core.Document;


         procedure Put_Table_Header is
         begin
            HTML.Begin_Div (Class => "job_list");
            Ada.Text_IO.Put ("<table><tr>");
            HTML.Put_Cell (Data       => "",
                           Tag        => "th",
                           Colspan => 6,
                           Class => "delimited");
            HTML.Put_Cell (Data => "Resource Usage",
                           Tag => "th",
                           Colspan => 3,
                           Class => "delimited");
            HTML.Put_Cell (Data => "Priority<a href=""http://wiki.mpibpc.gwdg.de"
                           & "/grubmueller/index.php/Job_priority"">"
                           & "<img src=""/icons/help.png"" /></a>",
                           Tag => "th",
                           Colspan => 8,
                           Class => "delimited");
            Ada.Text_IO.Put ("</tr><tr>");
            HTML.Put_Header_Cell (Data => "Number", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Owner", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Name", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Submitted", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Slots", Params => My_Params);
            HTML.Put_Header_Cell (Data => "State", Params => My_Params);
            HTML.Put_Header_Cell (Data => "CPU", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Memory",
                                  Acronym => "Gigabyte-hours",
                                  Params => My_Params);
            HTML.Put_Header_Cell (Data => "IO",
                                  Acronym => "Gigabytes",
                                  Params => My_Params);
            HTML.Put_Header_Cell (Data => "Priority", Params => My_Params);
            HTML.Put_Header_Cell (Data => "O",
                                  Acronym => "Override",
                                  Params => My_Params);
            HTML.Put_Header_Cell (Data => "S",
                                  Acronym => "Share",
                                  Params => My_Params);
            HTML.Put_Header_Cell (Data => "F",
                                  Acronym => "Functional",
                                  Params => My_Params);
            HTML.Put_Header_Cell (Data => "Urgency", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Resource", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Waiting", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Custom", Params => My_Params);
            Ada.Text_IO.Put ("</tr>");
         end Put_Table_Header;

         procedure Put_Job_List_Entry (Job : Jobs.Job_Lists.Cursor) is
            J : Jobs.Job := Jobs.Job_Lists.Element (Job);
         begin
            Ada.Text_IO.Put ("<tr>");
            HTML.Put_Cell (Data       => Ada.Strings.Fixed.Trim (J.Number'Img, Ada.Strings.Left),
                           Link_Param => "job_id");
            HTML.Put_Cell (Data => J.Owner, Link_Param => "user");
            if J.Name_Truncated then
               HTML.Put_Cell (Data => "<acronym title=""" & J.Full_Name & """>"
                              & J.Name & "</acronym>");
            else
               HTML.Put_Cell (Data => J.Name);
            end if;
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
               HTML.Put_Cell (Data  => CGI.HTML_Encode ("<1"),
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
         end Put_Job_List_Entry;


      begin
         SGE_Out := Setup_Parser (Selector => "-urg -pri -ext " & Selector);

         Put_Table_Header;

         Jobs.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "job_list"));
         if not HTML.Param_Is ("sort", "") then
            if Length (Sort_Direction) /= 3 then
               --  something wrong -- maybe an attack?
               Sort_Direction := To_Unbounded_String ("inc");
            end if;
            Jobs.Sort_By (Field     => CGI.Value ("sort"),
                          Direction => To_String (Sort_Direction));
         end if;
         Job_List.Iterate (Put_Job_List_Entry'Access);

         --  Table Footer
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "job_list");
      end View_Jobs;

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
         SGE_Out       : DOM.Core.Document;
         List          : Node_List;


         procedure Output_One_Job  (Cursor : Job_Lists.Cursor) is
            Res        : Resource_Lists.Cursor;
            Slot_Range : Slot_Lists.Cursor;
            Q, Msg     : String_Lists.Cursor;

            J : Job := Job_Lists.Element (Cursor);
         begin
            HTML.Begin_Div (Class => "job_info");
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
            HTML.End_Div;

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

            HTML.Begin_Div (Class => "job_files");
            HTML.Put_Paragraph ("Directory", J.Directory);
            HTML.Put_Paragraph ("Script", J.Script_File);
            HTML.Put_Paragraph ("Executable", J.Exec_File);
            Ada.Text_IO.Put ("<p>Merge StdErr: ");
            HTML.Put (J.Merge_Std_Err);
            Ada.Text_IO.Put_Line ("</p>");
            Ada.Text_IO.Put ("<p>Notify: ");
            HTML.Put (J.Notify);
            Ada.Text_IO.Put_Line ("</p>");
            HTML.End_Div (Class => "job_files");

            HTML.Begin_Div (Class => "job_queue");
            CGI.Put_HTML_Heading (Title => "Requested",
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

            CGI.Put_HTML_Heading (Title => "Assigned",
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

            HTML.Put_Clearer;
            HTML.End_Div (Class => "job_info");
            HTML.Put_Clearer;
         end Output_One_Job;


      begin
         CGI.Put_HTML_Heading (Title => "Details of Job " & Job_ID,
                               Level => 2);
         SGE_Out := Setup_Parser (Selector => "-j " & Job_ID);

         --  Fetch Jobs
         List := Get_Elements_By_Tag_Name (SGE_Out, "djob_info");
         if Length (List) = 0 then
            Ada.Text_IO.Put_Line ("<i>unknown job</i>");
            return;
         end if;

         Append_List (List);
         Job_List.Iterate (Output_One_Job'Access);

      exception
         when Sax.Readers.XML_Fatal_Error =>
            Ada.Text_IO.Put_Line ("<p><it>Job does not exist</it></p>");
      end View_Job;

      -------------------
      -- View_Forecast --
      -------------------

      procedure View_Forecast is
         SGE_Out     : DOM.Core.Document;


         procedure Put_Table_Header is
         begin
            HTML.Begin_Div (Class => "job_list");
            Ada.Text_IO.Put ("<table><tr>");
            HTML.Put_Header_Cell (Data => "Number", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Owner", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Name", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Slots", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Ends In", Params => My_Params);
            HTML.Put_Header_Cell (Data => "Ends At", Params => My_Params);
            HTML.Put_Header_Cell (Data => "State", Params => My_Params);
            Ada.Text_IO.Put ("</tr>");
         end Put_Table_Header;

         procedure Put_Job_List_Entry (Job : Jobs.Job_Lists.Cursor) is
            J : Jobs.Job := Jobs.Job_Lists.Element (Job);
         begin
            Ada.Text_IO.Put ("<tr>");
            HTML.Put_Cell (Data       => Ada.Strings.Fixed.Trim (J.Number'Img, Ada.Strings.Left),
                           Link_Param => "job_id");
            HTML.Put_Cell (Data => J.Owner, Link_Param => "user");
            if J.Name_Truncated then
               HTML.Put_Cell (Data => "<acronym title=""" & J.Full_Name & """>"
                              & J.Name & "</acronym>");
            else
               HTML.Put_Cell (Data => J.Name);
            end if;
            HTML.Put_Cell (Data => J.Slot_Number, Tag => "td class=""right""");
            HTML.Put_Duration_Cell (Remaining_Time (J));
            HTML.Put_Time_Cell (End_Time (J));
            HTML.Put_Img_Cell (State_As_String (J));
         exception
            when E : others => HTML.Error (Message => "Error while outputting job: "
                                           & Exception_Message (E));
         end Put_Job_List_Entry;

      begin
         SGE_Out := Setup_Parser (Selector => "-u \* -s r -r");

         Put_Table_Header;

         Jobs.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "job_list"));
         if not HTML.Param_Is ("sort", "") then
            if Length (Sort_Direction) /= 3 then
               --  something wrong -- maybe an attack?
               Sort_Direction := To_Unbounded_String ("inc");
            end if;
            Jobs.Sort_By (Field     => CGI.Value ("sort"),
                          Direction => To_String (Sort_Direction));
         end if;
         Job_List.Iterate (Put_Job_List_Entry'Access);

         --  Table Footer
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "job_list");
      end View_Forecast;


      N : Positive;

   begin
      begin
         if not HTML.Param_Is ("sort", "") then
            if HTML.Param_Is ("dir", "") then
               Sort_Direction := CGI.Cookie_Value (String'(CGI.Value ("sort")) & "sort");
            else
               Sort_Direction := CGI.Value ("dir");
               CGI.Set_Cookie (Key   => CGI.Value ("sort") & "sort",
                            Value => To_String (Sort_Direction));
            end if;
         end if;
         Put_Headers;
      exception
         when E : others =>
            Put_Headers;
                     HTML.Error ("Unhandled Exception occurred.");
         HTML.Error (Exception_Message (E));

      end;
      HTML.Begin_Div (ID => "content");
      if HTML.Param_Is ("categories", "") then
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
         elsif not HTML.Param_Is ("categories", "") then
            Set_Params ("categories=" & CGI.Value ("categories"));
            if HTML.Param_Is ("categories", "supply") then
               View_Detailed_Queues;
            elsif HTML.Param_Is ("categories", "demand") then
               View_Job_Overview;
            elsif HTML.Param_Is ("categories", "both") then
               View_Detailed_Queues;
               View_Job_Overview;
            end if;
         elsif HTML.Param_Is ("forecast", "y") then
            Set_Params ("forecast=y");
            View_Forecast;
         elsif not HTML.Param_Is ("profile", "") then
            N := Integer'Value (CGI.Value ("profile"));
            if N > 10 then
               N := 1;
            end if;

            for I in 1 .. N  loop
               View_Global_Jobs;
               View_Detailed_Queues;
               View_Job_Overview;
            end loop;
         end if;
      end if;
      HTML.End_Div (ID => "content");
      Put_Footer;
      HTML.Finalize_Divs (Silent => True);
      CGI.Put_HTML_Tail;
   exception
      when E : others =>
         HTML.Error ("Unhandled Exception occurred.");
         HTML.Error (Exception_Message (E));
         HTML.Finalize_Divs (Silent => True);
         CGI.Put_HTML_Tail;
   end View;


   ----------------
   -- Set_Params --
   ----------------

   procedure Set_Params (Params : String) is
   begin
      My_Params := To_Unbounded_String (Params);
   end Set_Params;

   ------------------
   -- Setup_Parser --
   ------------------

   function Setup_Parser (Selector : String) return DOM.Core.Document is
      Reader      : DOM.Readers.Tree_Reader;
      SGE_Command : Pipe_Stream;
   begin
      SGE_Command.Set_Public_Id ("qstat");
      SGE_Command.execute ("SGE_ROOT=" & sgeroot & " " & sgeroot
        & "/bin/lx26-amd64/qstat " & Selector & " -xml"
        & ASCII.NUL,
        Pipe_Commands.read_file);
      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (SGE_Command);
      SGE_Command.Close;
      Pipe_Streams.Wait_For_Children;
      return Reader.Get_Tree;
   end Setup_Parser;


end Viewer;
