with Ada.Text_IO, CGI;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with HTML;
with Ada.Exceptions; use Ada.Exceptions;
with Slurm.Utils;
with Slurm.General;
with Utils;
with Jobs; use Jobs;
with Bunches; use Bunches;
with Nodes; use Nodes;
with Maintenance;
with Share_Tree;
with Diagnostics;
with Ada.Strings;
with Ada.Integer_Text_IO;
with Nodegroups;
with Slurm.Node_Properties;
with Slurm.Loggers;
with Slurm.Bunches;

package body Viewer is

   Cluster_Name : constant String := CGI.Get_Environment ("CLUSTER_NAME");

   procedure Append_Params (Params : String) is
   begin
      My_Params := My_Params & "&" & Params;
   end Append_Params;

   function Params return String is
   begin
      return To_String (My_Params);
   end Params;

   procedure Put_Error (Message : String) is
   begin
      CGI.Put_CGI_Header;
      Ada.Text_IO.Put_Line ("<html><head><title>" & Cluster_Name & " Status - Error</title>");
      Ada.Text_IO.Put_Line ("</head><body>");
      Ada.Text_IO.Put_Line ("<p>" & Message & "</p>");
      CGI.Put_HTML_Tail;
   end Put_Error;

   procedure Put_Result (Message : String) is
   begin
      CGI.Put_CGI_Header;
      Ada.Text_IO.Put_Line ("<html><head><title>" & Cluster_Name & " Status - Result</title>");
      Ada.Text_IO.Put_Line ("</head><body>");
      Ada.Text_IO.Put_Line ("<p>" & Message & "</p>");
      CGI.Put_HTML_Tail;
   end Put_Result;

   procedure Set_Params (Params : String) is
   begin
      My_Params := To_Unbounded_String (Params);
   end Set_Params;

   function Sort_Field return String is
   begin
      return To_String (Sort_String);
   end Sort_Field;

   procedure View is
      procedure Put_Error (Message : String);
      procedure Put_Headers (Title : String);
      procedure Put_Diagnostics;
      procedure Put_Footer;
      procedure View_Bunch;
      procedure View_Equivalent_Hosts (Host_Name : String);
      procedure View_Global_Jobs;
      procedure View_Job (Job_ID : String);
      procedure View_Job_Overview;
      procedure View_Nodegroup;
      procedure View_Jobs_Of_User (User : String);
      procedure View_Jobs_In_Queue (Queue : String);
      procedure View_Maintenance_Report;
      procedure View_Reservations;
      procedure View_Waiting_Jobs;
      procedure View_Running_Jobs;

      Headers_Sent : Boolean := False;

      procedure Put_Diagnostics is
      begin
         Ada.Text_IO.Put ("<li>");
         Ada.Text_IO.Put ("Time:");
         Diagnostics.Put_Time;
         Ada.Text_IO.Put ("</li>");

         Ada.Text_IO.Put ("<li>");
         Ada.Text_IO.Put ("Memory:");
         Diagnostics.Put_Memory;
         Ada.Text_IO.Put ("</li>");

         Ada.Text_IO.Put ("<li>");
         Ada.Text_IO.Put ("Generated: ");
         Diagnostics.Put_Date;
         Ada.Text_IO.Put ("</li>");
      end Put_Diagnostics;

      procedure Put_Error (Message : String) is
      begin
         Ada.Text_IO.Put_Line ("<li>" & Message & "</li>");
      end Put_Error;

      procedure Put_Footer is
      begin
         HTML.Begin_Div (ID => "footer");
         Ada.Text_IO.Put ("<ul>");
         Ada.Text_IO.Put_Line ("<li><a href=""mailto:aeszter@gwdg.de"">"
                               & "aeszter@gwdg.de</a></li>");
         Ada.Text_IO.Put_Line ("<li><a href="""
                               & CGI.Get_Environment ("BUGZILLA_URL")
                               & "/enter_bug.cgi?"
                               & "component=qview&form_name=enter_bug"
                               & "&product=Projects&version="
                               & Utils.Version & """>"
                               & "Report Problem/Suggest Enhancement</a></li>");
         Put_Diagnostics;
         Ada.Text_IO.Put_Line ("<li>qview " & Utils.Version & "</li>");
         Ada.Text_IO.Put_Line ("<li>slurmlib " & Slurm.Utils.Version & "</li>");
         Ada.Text_IO.Put ("<li>Slurm API");
         Ada.Integer_Text_IO.Put (Slurm.General.API_Version, Base => 16);
         Ada.Text_IO.Put_Line ("</li>");
         Ada.Text_IO.Put ("</ul>");
         HTML.End_Div (ID =>  "footer");
         HTML.Put_Clearer;
         HTML.End_Div (ID => "page");
      end Put_Footer;

      procedure Put_Headers (Title : String) is
      begin
         CGI.Put_CGI_Header;
         Headers_Sent := True;
--           SGE.Debug.Log (Message  => CGI.Cookie_Count'Img & " cookies read",
--                      Where    => SGE.Debug.Default,
--                      Severity => 1);
         Ada.Text_IO.Put_Line ("<html><head><title>" & Cluster_Name & " Status - "
                               & HTML.Encode (Title) & "</title>");
         HTML.Put_Stylesheet (CGI.My_URL & "?css=y");
         HTML.Put_Opensearch (CGI.My_URL & "?opensearch=y");
         Ada.Text_IO.Put_Line ("</head><body>");
         HTML.Begin_Div (ID => "page");
         HTML.Begin_Div (ID => "header");
         CGI.Put_HTML_Heading (Title => Cluster_Name & " Status", Level => 1);
         HTML.Put_Navigation_Begin;
         HTML.Put_Navigation_Link (Data       => "Overview",
                                   Link_Param => "categories=both");
         HTML.Put_Navigation_Link ("All Jobs", "jobs=all");
         HTML.Put_Navigation_Link ("All Nodes", "nodes=all");
         HTML.Put_Navigation_Link ("Waiting Jobs", "jobs=waiting");
         HTML.Put_Navigation_Link (Data       => "Running Jobs",
                                   Link_Param => "jobs=running");
         HTML.Put_Navigation_Link (Data       => "Scheduler",
                                   Link_Param => "reservation=y");
         HTML.Put_Navigation_Link (Data       => "Maintenance",
                                   Link_Param => "maintenance=y");
         HTML.Put_Navigation_Link (Data       => "Users",
                                   Link_Param => "sharetree=y");
         HTML.Put_Search_Box;
         HTML.Put_Navigation_End;
         HTML.End_Div (ID => "header");
         HTML.Begin_Div (ID => "content");
      end Put_Headers;

      procedure View_Bunch is
         Requirements : Slurm.Bunches.Set_Of_Requirements;
      begin
         Slurm.Bunches.Init (Requirements,
                       CPUs => Integer'Value (CGI.Value ("cpus")),
                       Gres => To_String (CGI.Value ("gres")),
                       TRES => To_String (CGI.Value ("tres")));
         Jobs.Put_Pending_List (Requirements,
                                Sort_By => Sort_Field,
                                Direction => Sort_Direction);
      exception
         when E : others =>
            HTML.Error ("Error while viewing bunch of jobs: "
                        & Exception_Message (E));
      end View_Bunch;

      procedure View_Equivalent_Hosts (Host_Name : String) is
         pragma Unreferenced (Host_Name);
--           procedure View_One_Queue (Q : SGE.Queues.Queue);
--           SGE_Out : Parser.Tree;
--           Props   : Set_Of_Properties;
--
--           procedure View_One_Queue (Q : SGE.Queues.Queue) is
--           begin
--              Props := SGE.Queues.Get_Properties (Q);
--              Set_Runtime (Props   => Props,
--                           Runtime => Null_Unbounded_String); -- not used, see Bug #1495
--              View_Hosts (Props => Props, Slots => SGE.Queues.Get_Slot_Count (Q));
--           end View_One_Queue;

      begin
--           SGE_Out := Parser.Setup (Selector => Parser.Resource_Selector
--                                    & Implicit_Trust (" -q *@") & Sanitise (Host_Name));
--           Queues.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "Queue-List"));
--           SGE.Parser.Free;
--
--           Queues.Iterate (Process => View_One_Queue'Access);
         HTML.Put_Paragraph (Label    => "View_Equivalent_Hosts",
                             Contents => "unimplemented");
      end View_Equivalent_Hosts;

      procedure View_Global_Jobs is
      begin
         CGI.Put_HTML_Heading (Title => "All Jobs", Level => 2);
         Jobs.Put_Global_List (Sort_By   => Sort_Field,
                               Direction => Sort_Direction);
      end View_Global_Jobs;

      procedure View_Job (Job_ID : String) is
      begin
         CGI.Put_HTML_Heading (Title => "Details of Job " & Job_ID,
                               Level => 2);
         Jobs.Put_Details (Integer'Value (Job_ID));

      exception
         when Constraint_Error =>
            Ada.Text_IO.Put_Line ("<p><it>Job does not exist</it></p>");
      end View_Job;

      procedure View_Job_Overview is
      begin
         HTML.Begin_Div (Class => "bunches");
         CGI.Put_HTML_Heading (Title => "Demand",
                            Level => 2);
         Bunches.Put_All;
         HTML.End_Div (Class => "bunches");
      end View_Job_Overview;

      procedure View_Jobs_In_Queue (Queue : String) is
      begin
         CGI.Put_HTML_Heading (Title => """" & Queue & """ Jobs", Level => 2);
         HTML.Put_Paragraph (Label    => "View_Jobs_In_Queue",
                             Contents => "unimplemented");
--           View_Jobs (Implicit_Trust ("-u * -s r -q ") & Sanitise (Queue));
      end View_Jobs_In_Queue;

      procedure View_Jobs_Of_User (User : String) is
      begin
         CGI.Put_HTML_Heading (Title => "Jobs of " & User,
                               Level => 2);
         Jobs.Put_User_List (User,
                             Sort_By   => Sort_Field,
                             Direction => Sort_Direction);
      end View_Jobs_Of_User;

      procedure View_Maintenance_Report is
      begin
         Maintenance.Put_All;
      end View_Maintenance_Report;

      procedure View_Nodegroup is
         use Slurm.Node_Properties;

         Props : Set_Of_Properties;
      begin
         Nodes.Init (Properties => Props,
                     GRES       => CGI.Value ("gres"),
                     TRES       => CGI.Value ("tres"),
                     CPUs       => CGI.Value ("cores"),
                     Features   => CGI.Value ("features"),
                     Memory     => CGI.Value ("mem"));
         Nodes.Put_List (Props);
      end View_Nodegroup;

      procedure View_Reservations is
      begin
         CGI.Put_HTML_Heading (Title => "Scheduler", Level => 2);
         Jobs.Put_Reserving_List (Sort_By   => Sort_Field,
                                  Direction => Sort_Direction);
      end View_Reservations;

      procedure View_Running_Jobs is
      begin
         CGI.Put_HTML_Heading (Title => "Running Jobs", Level => 2);
         Jobs.Put_Running_List (Sort_By   => Sort_Field,
                               Direction => Sort_Direction);
      end View_Running_Jobs;

      procedure View_Waiting_Jobs is
      begin
         CGI.Put_HTML_Heading (Title => "Pending Jobs", Level => 2);
         Jobs.Put_Pending_List (Sort_By => Sort_Field,
                                Direction => Sort_Direction);
      end View_Waiting_Jobs;

   begin
--        SGE.Debug.Initialize (CGI.Value ("DEBUG"), HTML.Comment'Access);
--        CM.Debug.Initialize (HTML.Comment'Access);
      begin
         if not HTML.Param_Is ("sort", "") then
            Sort_String := CGI.Value ("sort");
            if HTML.Param_Is ("dir", "") then
               Sort_Direction := CGI.Cookie_Value (String'(CGI.Value ("sort")) & "sort");
            else
               Sort_Direction := CGI.Value ("dir");
               CGI.Set_Cookie (Key   => CGI.Value ("sort") & "sort",
                               Value => Sort_Direction);
            end if;
         end if;
      exception
         when Constraint_Error =>
            Sort_Direction := "inc";
         when E : others =>
            Put_Headers (Title => "Error");
            HTML.Error ("Unhandled Exception occurred.");
            HTML.Error (Exception_Message (E));
            HTML.Error (Exception_Name (E));

      end;

      --  Note: until we clean up our parameters, order is important here.
      --  The problem is that some (like queue) can be used with or without
      --  a command. Therefore, check for clear commands like categories or
      --  jobs=bunch first.
      if CGI.Input_Received then
         if not HTML.Param_Is ("categories", "") then
            Set_Params ("categories=" & CGI.Value ("categories"));
            Put_Headers (Title => "Supply & Demand");
            HTML.Begin_Div (Class => "nodegroups");
            CGI.Put_HTML_Heading (Title => "Supply",
                   Level => 2);
            Nodegroups.Put_All;
            HTML.End_Div (Class => "nodegroups");
            View_Job_Overview;
         elsif not HTML.Param_Is ("search", "") then
            declare ID : Positive;
               pragma Unreferenced (ID);
               Search_String : constant String := CGI.Value ("search");
               Start         : constant Natural := Search_String'First;
            begin
               ID := Positive'Value (CGI.Value ("search"));
               Put_Headers (Title => "Job " & CGI.Value ("search"));
               Set_Params ("job_id=" & CGI.Value ("search"));
               View_Job (CGI.Value ("search"));
            exception
               when Constraint_Error =>
                  if Search_String (Start .. Start + 3) = "node" then
                     Put_Headers (Title => Search_String & " and equivalent");
                     Set_Params ("" & Search_String);
                     View_Equivalent_Hosts (Search_String);
                  else
                     Put_Headers (Title => "User " & CGI.Value ("search"));
                     Set_Params ("user=" & CGI.Value ("search"));
                     View_Jobs_Of_User (CGI.Value ("search"));
                  end if;
            end;
         elsif HTML.Param_Is ("jobs", "bunch") then
            Put_Headers (Title => "Job Group");
            Set_Params ("jobs=bunch");
            View_Bunch;
         elsif not HTML.Param_Is ("queue", "") then
            Put_Headers (Title => "Queue " & CGI.Value ("queue"));
            Set_Params ("queue=" & CGI.Value ("queue"));
            View_Jobs_In_Queue (CGI.Value ("queue"));
         elsif not HTML.Param_Is ("node", "") then
            Put_Headers (Title => "Node " & CGI.Value ("node"));
            Nodes.Put_Details (CGI. Value ("node"));
         elsif not HTML.Param_Is ("user", "") then
            Put_Headers (Title => "User " & CGI.Value ("user"));
            Set_Params ("user=" & CGI.Value ("user"));
            View_Jobs_Of_User (CGI.Value ("user"));
         elsif not HTML.Param_Is ("job_id", "") then
            Put_Headers (Title => "Job " & CGI.Value ("job_id"));
            Set_Params ("job_id=" & CGI.Value ("job_id"));
            View_Job (CGI.Value ("job_id"));
         elsif HTML.Param_Is ("jobs", "all") then
            Put_Headers (Title => "All Jobs");
            Set_Params ("jobs=all");
            View_Global_Jobs;
         elsif HTML.Param_Is ("nodes", "all") then
            Put_Headers (Title => "All Nodes");
            Nodes.Put_All;
         elsif HTML.Param_Is ("nodes", "group") then
            Put_Headers (Title => "Nodegroup");
            View_Nodegroup;
         elsif HTML.Param_Is ("jobs", "waiting") then
            Put_Headers (Title => "Waiting Jobs");
            Set_Params ("jobs=waiting");
            View_Waiting_Jobs;
         elsif HTML.Param_Is ("jobs", "running") then
            Put_Headers (Title => "Running Jobs");
            Set_Params ("jobs=running");
            View_Running_Jobs;
         elsif HTML.Param_Is ("reservation", "y") then
            Put_Headers (Title => "Reservations");
            Set_Params ("reservation=y");
            View_Reservations;
         elsif HTML.Param_Is ("maintenance", "y") then
            Put_Headers (Title => "Maintenance Report");
            Set_Params ("maintenance=y");
            View_Maintenance_Report;
         elsif HTML.Param_Is ("sharetree", "y") then
            Put_Headers (Title => "User List");
            Set_Params ("sharetree=y");
            Share_Tree.Put_All (Sort_Field => HTML.Param ("sort"),
                                Sort_Direction => Sort_Direction);
         end if;
      else
         Put_Headers (Title => "");
      end if;
      if Slurm.Loggers.Errors_Exist then
         HTML.Begin_Div (ID => "internal_errors");
         HTML.Put_Heading (Title => "Internal errors",
                           Level => 2);
         Slurm.Loggers.Iterate_Errors (Put_Error'Access);
         HTML.End_Div (ID => "internal_errors");
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
         HTML.Error ("Unhandled Exception " & Exception_Name (E) & " occurred.");
         HTML.Error (Exception_Message (E));
         HTML.Finalize_Divs (Silent => True);
         CGI.Put_HTML_Tail;
   end View;

--     procedure View_Hosts (Props : Set_Of_Properties; Slots : Positive) is
--        function GPU_Selector return Trusted_String;
--        function Net_Selector return Trusted_String;
--
--        SGE_Out      : Parser.Tree;
--        CPU_Selector : constant Trusted_String := Implicit_Trust (" -l cm=")
-- & Sanitise (To_String (Get_Model (Props)));
--        GPU          : constant String := To_String (Get_GPU (Props));
--
--        function GPU_Selector return Trusted_String is
--        begin
--           if GPU /= ""
--             and then not Ada.Strings.Equal_Case_Insensitive (GPU, "none")
--           then
--              return Implicit_Trust (" -l gm=") & Sanitise (GPU);
--           else
--              return Implicit_Trust ("");
--           end if;
--        end GPU_Selector;
--
--        function Net_Selector return Trusted_String is
--        begin
--           case Get_Network (Props) is
--              when eth =>
--                 Append_Params ("net=ETH");
--                 return Implicit_Trust (" -l eth");
--              when ib =>
--                 Append_Params ("net=IB");
--                 return Implicit_Trust (" -l ib");
--              when ibswitch =>
--                 Append_Params ("net=IBSWITCH");
--                 return Implicit_Trust ("");
--              when none =>
--                 Append_Params ("net=NONE");
--                 return Implicit_Trust ("");
--           end case;
--        end Net_Selector;
--
--     begin
--        Append_Params ("gm=" & GPU);
--        Append_Params ("model=" & To_String (Get_Model (Props)));
--        Append_Params ("cores=" & Get_Cores (Props)'Img);
--        Append_Params ("mem=" & To_String (Get_Memory (Props)));
--        Append_Params ("slots=" & Slots'Img);
--
--        SGE_Out := Parser.Setup (Command  => Cmd_Qhost,
--                                 Selector => Implicit_Trust ("-q -j ") & Parser.Resource_Selector
--                                 & Net_Selector & CPU_Selector & GPU_Selector);
--
--        SGE.Hosts.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "host"));
--        SGE.Parser.Free;
--        SGE.Hosts.Prune_List (Requirements => Props, Slots => Slots);
--
--        --  Can we factor this out?
--        if not HTML.Param_Is ("sort", "") then
--           SGE.Hosts.Sort_By (Field     => CGI.Value ("sort"),
--                         Direction => Sort_Direction);
--        end if;
--
--        Hosts.Put_All;
--
--     exception
--        when E : others =>
--           HTML.Error ("Error while viewing host: " & Exception_Message (E));
--           Ada.Text_IO.Put_Line ("</table>");
--           HTML.End_Div (Class => "host_list");
--     end View_Hosts;

end Viewer;
