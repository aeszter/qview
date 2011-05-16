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
with Hosts; use Hosts; use Hosts.Host_Lists;
with Utils; use Utils; use Utils.String_Lists;
with Diagnostics;
with Ada.Characters.Handling;

package body Viewer is

   ----------
   -- View --
   --  Purpose: Main routine, display the entire page
   ----------

   procedure View is

      Headers_Sent : Boolean := False;

      procedure Put_Headers (Title : String) is
      begin
         CGI.Put_CGI_Header;
         Headers_Sent := True;
         Ada.Text_IO.Put_Line ("<html><head><title>Owl Status - "
                               & HTML.Encode (Title) & "</title>");
         HTML.Put_Stylesheet ("/status.css");
         Ada.Text_IO.Put_Line ("</head><body>");
         HTML.Begin_Div (ID => "page");
         HTML.Begin_Div (ID => "header");
         CGI.Put_HTML_Heading (Title => "Owl Status", Level => 1);
         HTML.Put_Navigation_Begin;
         HTML.Put_Navigation_Link (Data => "Overview", Link_Param => "cqueues=y");
         HTML.Put_Navigation_Link ("All Jobs", "jobs=all");
         HTML.Put_Navigation_Link ("Waiting Jobs", "jobs=waiting");
         HTML.Put_Navigation_Link (Data       => "Detailed Queues",
                                   Link_Param => "categories=supply");
         HTML.Put_Navigation_Link (Data       => "Job Overview",
                                   Link_Param => "categories=demand");
         HTML.Put_Navigation_Link (Data       => CGI.HTML_Encode ("Supply & Demand"),
                                   Link_Param => "categories=both");
         HTML.Put_Navigation_Link (Data       => "Finishing Jobs",
                                   Link_Param => "forecast=y");
         HTML.Put_Search_Box;
         HTML.Put_Navigation_End;
         HTML.End_Div (ID => "header");
         HTML.Begin_Div (ID => "content");
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

         procedure Put_Table_Header is
         begin
            Ada.Text_IO.Put_Line ("<table><tr>");
            HTML.Put_Cell (Data => "<acronym title=""click on arrow to view job list"">"
                           & "Detail</acronym>", Tag => "th");
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
         end Put_Table_Header;

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
         Bunches.Build_List (Jobs.List, Bunch_List);

         --  Output
         Put_Table_Header;
         Ada.Text_IO.Put_Line ("</tr>");
         Bunch_List.Iterate (Bunches.Put'Access);
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

         SGE_Out := Setup_Parser (Selector => Resource_Selector);

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
         HTML.Put_Cell (Data => "<acronym title=""click on arrow to view node list"">"
                        & "Detail</acronym>", Tag => "th");
         HTML.Put_Cell (Data => "Interconnect", Tag => "th");
         HTML.Put_Cell (Data => "CPU" & HTML.Help_Icon (Topic => "CPU Families"),
                        Tag => "th");
         HTML.Put_Cell (Data => "Cores", Tag => "th");
         HTML.Put_Cell (Data => "RAM", Tag => "th");
         HTML.Put_Cell (Data => "Runtime", Tag => "th");
         HTML.Put_Cell (Data => "Slots", Tag => "th");
         HTML.Put_Cell (Data => "Used", Tag => "th");
         HTML.Put_Cell (Data => "Reserved", Tag => "th");
         HTML.Put_Cell (Data => "Available", Tag => "th");
         HTML.Put_Cell (Data => "<acronym title=""d: disabled by admin or health checker"">Suspended</acronym>", Tag => "th");
         HTML.Put_Cell ("<acronym title=""u: unreacheable"">Offline</acronym>", Tag => "th");
         Ada.Text_IO.Put_Line ("</tr>");
         Partition_List.Iterate (Partitions.Put'Access);
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
            HTML.Put_Cell (Data => "Priority" & HTML.Help_Icon (Topic => "Job_priority"),
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
         Jobs.List.Iterate (Jobs.Put_Res_Line'Access);

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

         Jobs.Append_List (List);
         Jobs.List.Iterate (Jobs.Put'Access);

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
         Jobs.List.Iterate (Jobs.Put_Time_Line'Access);

         --  Table Footer
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "job_list");
      end View_Forecast;

      ----------------
      -- View_Hosts --
      ----------------

      procedure View_Hosts (What : String) is
         SGE_Out     : DOM.Core.Document;

         procedure Put_Table_Header is
         begin
            HTML.Put_Heading (Title => "Hosts",
                              Level => 2);
            HTML.Begin_Div (Class => "host_list");
            Ada.Text_IO.Put_Line ("<table><tr>");
            HTML.Put_Header_Cell (Data     => "Name", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "Interconnect", Params => My_Params);
            HTML.Put_Cell (Data     => "CPU" & HTML.Help_Icon (Topic => "CPU Families"),
                          Tag => "th");
            HTML.Put_Header_Cell (Data     => "Cores", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "Free", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "RAM", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "Load", Params => My_Params,
                                 Acronym => "per core");
            HTML.Put_Header_Cell (Data => "Mem", Params => My_Params,
                                  Acronym => "% used");
            HTML.Put_Header_Cell (Data => "Swap", Params => My_Params,
                                  Acronym => "% used");
            HTML.Put_Header_Cell (Data     => "Queues",
                                  Params   => My_Params,
                                  Sortable => False);
            Ada.Text_IO.Put ("</tr>");
         end Put_Table_Header;

         Selector : Unbounded_String;
      begin
         if What /= "partition" then
            raise Constraint_Error with "Expected ""partition"" but got """
              & What & """";
         end if;
         if HTML.Param_Is (Param    => "net",
                           Expected => "ETH") then
            Selector := To_Unbounded_String (" -l eth");
         elsif HTML.Param_Is (Param    => "net",
                              Expected => "IB") then
            Selector := To_Unbounded_String (" -l ib");
         end if;
         if not HTML.Param_Is (Param    => "model",
                               Expected => "") then
            if HTML.Param_Is (Param    => "model",
                              Expected => "MAGNYCOURS") then
               Selector := Selector & " -l cm=magny-cours";
            else
               Selector := Selector & " -l cm="
                 & Ada.Characters.Handling.To_Lower (Sanitise (CGI.Value ("model")));
            end if;
         end if;

         SGE_Out := Setup_Parser (Command  => "qhost",
                                  Selector => "-q -j " & Resource_Selector
                                  & To_String (Selector));
         Put_Table_Header;

         Hosts.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "host"));
         Hosts.Prune_List (Net     => CGI.Value ("net"),
                           Cores   => CGI.Value ("cores"),
                           Memory  => CGI.Value ("mem"),
                           Runtime => CGI.Value ("rt"));
         Host_List.Iterate (Hosts.Put'Access);

         --  Table Footer
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "host_list");
      exception
         when E : others =>
            HTML.Error ("Error while putting host: " & Exception_Message (E));
            Ada.Text_IO.Put_Line ("</table>");
            HTML.End_Div (Class => "host_list");
      end View_Hosts;

      ----------------
      -- View_Bunch --
      ----------------

      procedure View_Bunch is
         SGE_Out     : DOM.Core.Document;

         procedure Put_Table_Header is
         begin
            HTML.Put_Heading (Title => "Jobs",
                              Level => 2);
            HTML.Begin_Div (Class => "job_list");
            Ada.Text_IO.Put_Line ("<table><tr>");
            HTML.Put_Header_Cell (Data     => "ID", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "Owner", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "Name", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "PE", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "Slots", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "Hard", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "Soft", Params => My_Params);
            HTML.Put_Header_Cell (Data     => "State", Params => My_Params);
            Ada.Text_IO.Put ("</tr>");
         end Put_Table_Header;

      begin
         SGE_Out := Setup_Parser (Command  => "qstat",
                                  Selector => "-r -s p -u \*");
         Put_Table_Header;

         Jobs.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "job_list"));
         Jobs.Prune_List (PE            => CGI.Value ("pe"),
                          Slots         => CGI.Value ("slots"),
                          Queue         => CGI.Value ("queue"),
                          Hard_Requests => CGI.Value ("hr"),
                          Soft_Requests => CGI.Value ("sr")
                         );
         Jobs.List.Iterate (Jobs.Put_Bunch_Line'Access);

         --  Table Footer
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "job_list");
      exception
         when E : others =>
            HTML.Error ("Error while viewing bunch of jobs: "
                        & Exception_Message (E));
            Ada.Text_IO.Put_Line ("</table>");
            HTML.End_Div (Class => "job_list");
      end View_Bunch;

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
      exception
         when E : others =>
            Put_Headers (Title => "Error");
            HTML.Error ("Unhandled Exception occurred.");
            HTML.Error (Exception_Message (E));

      end;

      --  Note: until we clean up our parameters, order is important here.
      --  The problem is that some (like queue) can be used with or without
      --  a command. Therefore, check for clear commands like categories or
      --  jobs=bunch first.
      if CGI.Input_Received then
         if not HTML.Param_Is ("categories", "") then
            Set_Params ("categories=" & CGI.Value ("categories"));
            if HTML.Param_Is ("categories", "supply") then
               Put_Headers (Title => "Supply");
               View_Detailed_Queues;
            elsif HTML.Param_Is ("categories", "demand") then
               Put_Headers (Title => "Demand");
               View_Job_Overview;
            elsif HTML.Param_Is ("categories", "both") then
               Put_Headers (Title => "Supply & Demand");
               View_Detailed_Queues;
               View_Job_Overview;
            end if;
         elsif not HTML.Param_Is ("search", "") then
            declare ID : Positive;
               pragma Unreferenced (ID);
            begin
               ID := Positive'Value (CGI.Value ("search"));
               Put_Headers (Title => "Job " & CGI.Value ("search"));
               Set_Params ("job_id=" & Sanitise (CGI.Value ("search")));
               View_Job (Sanitise (CGI.Value ("search")));
            exception
               when Constraint_Error =>
                  Put_Headers (Title => "User " & CGI.Value ("search"));
                  Set_Params ("user=" & Sanitise (CGI.Value ("search")));
                  View_Jobs_Of_User (Sanitise (CGI.Value ("search")));
            end;
         elsif HTML.Param_Is ("cqueues", "y") then
            Put_Headers (Title => "Overview");
            View_Cluster_Queues;
         elsif HTML.Param_Is ("jobs", "bunch") then
            Put_Headers (Title => "Job Group");
            Set_Params ("jobs=bunch");
            View_Bunch;
         elsif not HTML.Param_Is ("queue", "") then
            Put_Headers (Title => "Queue " & CGI.Value ("queue"));
            Set_Params ("queue=" & Sanitise (CGI.Value ("queue")));
            View_Jobs_In_Queue (Sanitise (CGI.Value ("queue")));
         elsif not HTML.Param_Is ("hosts", "") then
            Put_Headers (Title => "Hosts: "
                         & CGI.Value ("net") & "/"
                         & CGI.Value ("model") & "/"
                         & CGI.Value ("cores") & "/"
                         & CGI.Value ("mem")
                        );
                         -- & "/" & CGI.Value "rt"
                         --  currently not used, so do not confuse the user
            Set_Params ("hosts=" & Sanitise (CGI.Value ("hosts")));
            View_Hosts (Sanitise (CGI.Value ("hosts")));
         elsif not HTML.Param_Is ("user", "") then
            Put_Headers (Title => "User " & CGI.Value ("user"));
            Set_Params ("user=" & Sanitise (CGI.Value ("user")));
            View_Jobs_Of_User (Sanitise (CGI.Value ("user")));
         elsif not HTML.Param_Is ("job_id", "") then
            Put_Headers (Title => "Job " & CGI.Value ("job_id"));
            Set_Params ("job_id=" & Sanitise (CGI.Value ("job_id")));
            View_Job (Sanitise (CGI.Value ("job_id")));
         elsif HTML.Param_Is ("jobs", "all") then
            Put_Headers (Title => "All Jobs");
            Set_Params ("jobs=all");
            View_Global_Jobs;
         elsif HTML.Param_Is ("jobs", "waiting") then
            Put_Headers (Title => "Waiting Jobs");
            Set_Params ("jobs=waiting");
            View_Waiting_Jobs;
         elsif HTML.Param_Is ("forecast", "y") then
            Put_Headers (Title => "Finishing Jobs");
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
               View_Hosts (What => "partition");
            end loop;
         end if;
      else
         Put_Headers (Title => "");
      end if;
      HTML.End_Div (ID => "content");
      Put_Footer;
      HTML.Finalize_Divs (Silent => True);
      CGI.Put_HTML_Tail;
   exception
      when E : others =>
         if not Headers_Sent then
            Put_Headers (Title => "Error");
         end if;
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

   function Setup_Parser (Command  : String := "qstat";
                          Selector : String) return DOM.Core.Document is
      Reader      : DOM.Readers.Tree_Reader;
      SGE_Command : Pipe_Stream;
      Command_String : String := sgeroot
        & "/bin/lx26-amd64/" & Command & " " & Selector & " -xml";
   begin
      SGE_Command.Set_Public_Id ("qstat");
      HTML.Comment (Command_String);
      SGE_Command.execute ("SGE_ROOT=" & sgeroot & " " &
                           Command_String
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
