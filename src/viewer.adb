with Ada.Text_IO, CGI;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with HTML;
with Parser;
with SGE.Parser; use SGE.Parser;
with SGE.Quota;
with Command_Tools; use Command_Tools;
with Ada.Exceptions; use Ada.Exceptions;
with SGE.Utils; use SGE.Utils; use SGE.Utils.String_Lists;
with Utils;
with SGE.Resources; use SGE.Resources; use SGE.Resources.Resource_Lists;
with Jobs; use Jobs;
with Bunches; use Bunches;
with Partitions; use Partitions;
with Hosts; use Hosts;
with Reservations;
with Maintenance;
with Share_Tree;
with Diagnostics;
with SGE.Debug;
with Ada.Strings;
with SGE.Spread_Sheets;
with SGE.Hosts;
with SGE.Queues;
with SGE.Jobs;
with Advance_Reservations;
with SGE.Loggers;

package body Viewer is

   ----------
   -- View --
   --  Purpose: Main routine, display the entire page
   ----------

   procedure View is

      Headers_Sent : Boolean := False;

      procedure Put_Error (Message : String) is
      begin
         Ada.Text_IO.Put_Line ("<li>" & Message & "</li>");
      end Put_Error;


      procedure Put_Headers (Title : String) is
      begin
         CGI.Put_CGI_Header;
         Headers_Sent := True;
         SGE.Debug.Log (Message  => CGI.Cookie_Count'Img & " cookies read",
                    Where    => SGE.Debug.Default,
                    Severity => 1);
         Ada.Text_IO.Put_Line ("<html><head><title>Owl Status - "
                               & HTML.Encode (Title) & "</title>");
         HTML.Put_Stylesheet (CGI.My_URL & "?css=y");
         Ada.Text_IO.Put_Line ("</head><body>");
         HTML.Begin_Div (ID => "page");
         HTML.Begin_Div (ID => "header");
         CGI.Put_HTML_Heading (Title => "Owl Status", Level => 1);
         HTML.Put_Navigation_Begin;
         HTML.Put_Navigation_Link (Data => "Overview", Link_Param => "categories=both");
         HTML.Put_Navigation_Link (Data       => CGI.HTML_Encode ("With Slot Ranges"),
                                   Link_Param => "categories=with_slots");
         HTML.Put_Navigation_Link ("All Jobs", "jobs=all");
         HTML.Put_Navigation_Link ("Waiting Jobs", "jobs=waiting");
         HTML.Put_Navigation_Link (Data       => "Finishing Jobs",
                                   Link_Param => "forecast=y");
         HTML.Put_Navigation_Link (Data       => "Reservations",
                                   Link_Param => "ar=y");
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


      procedure Put_Footer is
      begin
         HTML.Begin_Div (ID => "footer");
         Ada.Text_IO.Put ("<ul>");
         Ada.Text_IO.Put_Line ("<li><a href=""mailto:aeszter@gwdg.de"">"
                               & "aeszter@gwdg.de</a></li>");
         Ada.Text_IO.Put_Line ("<li><a href=""" & CGI.Get_Environment ("BUGZILLA_URL")
                               & "/enter_bug.cgi?"
                               & "component=qview&form_name=enter_bug"
                               & "&product=Projects&version="
                               & Utils.Version & """>"
                               &"Report Problem/Suggest Enhancement</a></li>");
         Put_Diagnostics;
         Ada.Text_IO.Put_Line ("<li>version " & Utils.Version & "</li>");
         Ada.Text_IO.Put_Line ("<li>SGElib " & SGE.Utils.Version & "</li>");
         Ada.Text_IO.Put ("</ul>");
         HTML.End_Div (ID =>  "footer");
         HTML.Put_Clearer;
         HTML.End_Div (ID => "page");
      end Put_Footer;

      -----------------------
      -- View_Job_Overview --
      -----------------------

      procedure View_Job_Overview (Slot_Ranges : Boolean) is
         SGE_Out     : Parser.Tree;
      begin
         HTML.Begin_Div (Class => "bunches");
         if HTML.Param_Is ("categories", "demand") then
            CGI.Put_HTML_Heading (Title => "Job Overview",
                               Level => 2);
         else
            CGI.Put_HTML_Heading (Title => "Demand",
                               Level => 2);
         end if;
         SGE_Out := Parser.Setup (Command  => "qquota",
                                  Selector => "-l slots -u *");
         SGE.Quota.Append_List (Get_Elements_By_Tag_Name (Doc      => SGE_Out,
                                                          Tag_Name => "qquota_rule"));
         SGE.Parser.Free;

         SGE_Out := Parser.Setup (Selector => "-u * -r -s p");

         Jobs.Append_List (Get_Job_Nodes_From_Qstat_U (SGE_Out));
         SGE.Parser.Free;

         if Slot_Ranges then
            SGE_Out := Parser.Setup (Selector => "-j *");

            Jobs.Create_Overlay (Get_Job_Nodes_From_Qstat_J (SGE_Out));
            SGE.Parser.Free;
            Jobs.Apply_Overlay;
         end if;

         SGE.Jobs.Update_Quota;

         --  Detect different bunches
         Bunches.Build_List;

         --  Output
         Bunches.Put_List;
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "bunches");
      end View_Job_Overview;


      --------------------------
      -- View_Detailed_Queues --
      --------------------------

      procedure View_Detailed_Queues is
         SGE_Out        : Parser.Tree;
      begin
         HTML.Begin_Div (Class => "partitions");
         if HTML.Param_Is ("categories", "supply") then
            CGI.Put_HTML_Heading (Title => "Detailed Queue Information",
                               Level => 2);
         else
            CGI.Put_HTML_Heading (Title => "Supply",
                               Level => 2);
         end if;

         SGE_Out := Parser.Setup (Selector => Parser.Resource_Selector);

         SGE.Queues.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "Queue-List"));
         SGE.Parser.Free;


         --  Detect different partitions
         Partitions.Build_List;

         --  Output
         Partitions.Put_Summary;
         Partitions.Put_List;
         HTML.End_Div (Class => "partitions");
      end View_Detailed_Queues;

      procedure View_Jobs (Selector : String; Only_Waiting : Boolean := False) is
         SGE_Out     : Parser.Tree;


      begin
         SGE_Out := Parser.Setup (Command  => "qquota",
                                  Selector => "-l slots -u *");
         SGE.Quota.Append_List (Get_Elements_By_Tag_Name (Doc      => SGE_Out,
                                                          Tag_Name => "qquota_rule"));
         SGE.Parser.Free;
         SGE_Out := Parser.Setup (Selector => "-urg -pri -ext " & Selector);

         Jobs.Append_List (Get_Job_Nodes_From_Qstat_U (SGE_Out));
         SGE.Parser.Free;
         if Only_Waiting then
            SGE_Out := Parser.Setup (Selector => "-j *");

            Jobs.Create_Overlay (Get_Job_Nodes_From_Qstat_J (SGE_Out));
            Jobs.Apply_Overlay;
            SGE.Parser.Free;
         end if;
         SGE.Jobs.Update_Quota;

         if not HTML.Param_Is ("sort", "") then
            Jobs.Sort_By (Field     => CGI.Value ("sort"),
                          Direction => Sort_Direction);
         end if;

         Jobs.Put_Summary;
         Jobs.Put_List (Show_Resources => not Only_Waiting);

         --  Table Footer
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "job_list");
      end View_Jobs;

      procedure View_Jobs_In_Queue (Queue : String) is
      begin
         CGI.Put_HTML_Heading (Title => """" & Queue & """ Jobs", Level => 2);
         View_Jobs ("-u * -s r -q " & Queue);
      end View_Jobs_In_Queue;


      procedure View_Global_Jobs is
      begin
         CGI.Put_HTML_Heading (Title => "All Jobs", Level => 2);
         View_Jobs ("-u *");
      end View_Global_Jobs;

      procedure View_Waiting_Jobs is
      begin
         CGI.Put_HTML_Heading (Title => "Pending Jobs", Level => 2);
         View_Jobs (Selector => "-u * -s p", Only_Waiting => True);
      end View_Waiting_Jobs;

      procedure View_Jobs_Of_User (User : String) is
      begin
         CGI.Put_HTML_Heading (Title => "Jobs of " & User,
                               Level => 2);
         View_Jobs ("-u " & User);
      end View_Jobs_Of_User;

      procedure View_Job (Job_ID : String) is
         SGE_Out       : Parser.Tree;
      begin
         CGI.Put_HTML_Heading (Title => "Details of Job " & Job_ID,
                               Level => 2);
         SGE_Out := Parser.Setup (Selector => "-j " & Job_ID);

         Jobs.Append_List (Get_Job_Nodes_From_Qstat_J (SGE_Out));
         Jobs.Update_Messages (Get_Message_Nodes_From_Qstat_J (SGE_Out));
         SGE.Parser.Free;
         Jobs.Update_Status;
         Jobs.Search_Queues;
         Jobs.Put_Details;

      exception
         when Parser_Error =>
            Ada.Text_IO.Put_Line ("<p><it>Job does not exist</it></p>");
      end View_Job;

      procedure View_Advance_Reservation (AR_ID : String) is
         package AR renames Advance_Reservations;
         SGE_Out       : Parser.Tree;
      begin
         CGI.Put_HTML_Heading (Title => "Details of Advance Reservation " & AR_ID,
                               Level => 2);
         SGE_Out := Parser.Setup (Command  => "qrstat",
                                  Selector => "-ar " & AR_ID);

         AR.Append_List (SGE_Out);
         SGE.Parser.Free;
         AR.Put_Details;

      exception
         when Parser_Error =>
            Ada.Text_IO.Put_Line ("<p><it>AR does not exist</it></p>");
      end View_Advance_Reservation;

      -------------------
      -- View_Forecast --
      -------------------

      procedure View_Equivalent_Hosts (Host_Name : String) is
         SGE_Out : Parser.Tree;
         Q       : SGE.Queues.Queue;
         Props : Set_Of_Properties;
      begin
         SGE_Out := Parser.Setup (Selector => Parser.Resource_Selector & " -q *@" & Host_Name);
         SGE.Queues.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "Queue-List"));
         SGE.Parser.Free;

         SGE.Queues.Rewind;
         Q := SGE.Queues.Current;
         loop
            Props := SGE.Queues.Get_Properties (Q);
            Set_Runtime (Props   => Props,
                         Runtime => Null_Unbounded_String); -- not used, see Bug #1495
            View_Hosts (Props => Props, Queue_Name => SGE.Queues.Get_Name (Q));
            exit when SGE.Queues.At_End;
            Q := SGE.Queues.Next;
         end loop;
      end View_Equivalent_Hosts;

      procedure View_Forecast is
         SGE_Out     : Parser.Tree;

      begin
         SGE_Out := Parser.Setup (Selector => "-u * -s r -r");


         Jobs.Append_List (Get_Job_Nodes_From_Qstat_U (SGE_Out));
         SGE.Parser.Free;
         SGE.Jobs.Update_Quota;
         if not HTML.Param_Is ("sort", "") then
            Jobs.Sort_By (Field     => CGI.Value ("sort"),
                          Direction => Sort_Direction);
         else
            Jobs.Sort_By (Field     => "Ends In",
                          Direction => "inc");
         end if;
         Jobs.Put_Time_List;

      end View_Forecast;

      procedure View_Advance_Reservations is
         package AR renames Advance_Reservations;
         SGE_Out : Parser.Tree;

      begin
         SGE_Out := Parser.Setup (Command  => "qrstat",
                                  Selector => "-u *");
         AR.Append_List (SGE_Out);
         SGE.Parser.Free;
         AR.Put_List;
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "ar_list");
      end View_Advance_Reservations;

      ----------------
      -- View_Bunch --
      ----------------

      procedure View_Bunch is
         SGE_Out : Parser.Tree;


      begin
         SGE_Out := Parser.Setup (Command  => "qquota",
                                  Selector => "-l slots -u *");
         SGE.Quota.Append_List (Get_Elements_By_Tag_Name (Doc      => SGE_Out,
                                                          Tag_Name => "qquota_rule"));
         SGE.Parser.Free;
         SGE_Out := Parser.Setup (Command  => "qstat",
                                  Selector => "-r -s p -u *");

         Append_Params ("pe="&CGI.Value ("pe"));
         Append_Params ("queue="&CGI.Value ("queue"));
         Append_Params ("hr="&CGI.Value ("hr"));
         Append_Params ("sr="&CGI.Value ("sr"));
         Append_Params ("slot_ranges="&CGI.Value ("slot_ranges"));
         Append_Params ("slot_number="&CGI.Value ("slot_number"));

         Jobs.Append_List (Get_Job_Nodes_From_Qstat_U (SGE_Out));
         SGE.Parser.Free;
         SGE_Out := Parser.Setup (Command  => "qstat",
                                  Selector => "-j *");
         Jobs.Create_Overlay (Get_Job_Nodes_From_Qstat_J (SGE_Out));
         SGE.Parser.Free;
         Jobs.Prune_List (PE            => CGI.Value ("pe"),
                          Queue         => CGI.Value ("queue"),
                          Hard_Requests => CGI.Value ("hr"),
                          Soft_Requests => CGI.Value ("sr"),
                          Slot_Ranges   => CGI.Value ("slot_ranges"),
                          Slot_Number   => CGI.Value ("slot_number")
                         );
         SGE.Jobs.Update_Quota;

         if not HTML.Param_Is ("sort", "") then
            Jobs.Sort_By (Field     => CGI.Value ("sort"),
                          Direction => Sort_Direction);
         end if;

         Jobs.Put_Bunch_List;
      exception
         when E : others =>
            HTML.Error ("Error while viewing bunch of jobs: "
                        & Exception_Message (E));
            Ada.Text_IO.Put_Line ("</table>");
            HTML.End_Div (Class => "job_list");
      end View_Bunch;

      procedure View_Partition is
         Props : Set_Of_Properties;
      begin
         Init (Props  => Props,
               Net    => CGI.Value ("net"),
               Memory => CGI.Value ("mem") & "G",
               Cores  => CGI.Value ("cores"),
               Model  => CGI.Value ("model"),
               SSD    => CGI.Value ("ssd"),
               GPU    => CGI.Value ("gpu_model"));
         View_Hosts (Props => Props, Queue_Name => CGI.Value ("q"));
      end View_Partition;

      procedure View_Reservations is
      begin
         Reservations.Read;
         Reservations.Put_All;
      end View_Reservations;

      procedure View_Maintenance_Report is
      begin
         Maintenance.Put_All;
      end View_Maintenance_Report;

      procedure View_Share_Tree is
         SGE_Out : SGE.Spread_Sheets.Spread_Sheet;

      begin
         SGE_Out := Parser.Setup_No_XML (Command => "sge_share_mon",
                                         Selector => "-f user_name,usage,cpu,ltcpu,mem,io,job_count -c1 -x");
         Share_Tree.Append_List (SGE_Out);
         if not HTML.Param_Is ("sort", "") then
            Share_Tree.Sort_By (Field     => CGI.Value ("sort"),
                          Direction => Sort_Direction);
         end if;

         Share_Tree.Put_Summary;
         Share_Tree.Put_List;


      end View_Share_Tree;

   begin
      SGE.Debug.Initialize (CGI.Value ("DEBUG"), HTML.Comment'Access);
      begin
         if not HTML.Param_Is ("sort", "") then
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
            if HTML.Param_Is ("categories", "supply") then
               Put_Headers (Title => "Supply");
               View_Detailed_Queues;
            elsif HTML.Param_Is ("categories", "demand") then
               Put_Headers (Title => "Demand");
               View_Job_Overview (Slot_Ranges => False);
            elsif HTML.Param_Is ("categories", "both") then
               Put_Headers (Title => "Supply & Demand");
               View_Detailed_Queues;
               View_Job_Overview (Slot_Ranges => False);
            elsif HTML.Param_Is ("categories", "with_slots") then
               Put_Headers (Title => "Supply & Demand (with slot ranges)");
               View_Detailed_Queues;
               View_Job_Overview (Slot_Ranges => True);
            end if;
         elsif not HTML.Param_Is ("search", "") then
            declare ID : Positive;
               pragma Unreferenced (ID);
               Search_String : constant String := CGI.Value ("search");
               Start         : constant Natural := Search_String'First;
            begin
               ID := Positive'Value (CGI.Value ("search"));
               Put_Headers (Title => "Job " & CGI.Value ("search"));
               Set_Params ("job_id=" & Sanitise (CGI.Value ("search")));
               View_Job (Sanitise (CGI.Value ("search")));
            exception
               when Constraint_Error =>
                  if Search_String (Start .. Start + 3) = "node" then
                     Put_Headers (Title => Search_String & " and equivalent");
                     Set_Params ("" & Sanitise (Search_String));
                     View_Equivalent_Hosts (Sanitise (Search_String));
                  else
                     Put_Headers (Title => "User " & CGI.Value ("search"));
                     Set_Params ("user=" & Sanitise (CGI.Value ("search")));
                     View_Jobs_Of_User (Sanitise (CGI.Value ("search")));
                  end if;
            end;
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
                        --  & "/" & CGI.Value "rt"
                        --  currently not used, so do not confuse the user
                        --  see Bug #1495
            Set_Params ("hosts=" & Sanitise (CGI.Value ("hosts")));
            View_Partition;
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
         elsif HTML.Param_Is ("ar", "y") then
            Put_Headers (Title => "Advance Reservations");
            Set_Params ("ar=y");
            View_Advance_Reservations;
         elsif not HTML.Param_Is ("ar_id", "") then
            Put_Headers (Title => "Advance Reservation " & CGI.Value ("ar_id"));
            Set_Params ("ar_id=" & Sanitise (CGI.Value ("ar_id")));
            View_Advance_Reservation (Sanitise (CGI.Value ("ar_id")));
         elsif HTML.Param_Is ("forecast", "y") then
            Put_Headers (Title => "Finishing Jobs");
            Set_Params ("forecast=y");
            View_Forecast;
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
            View_Share_Tree;
         end if;
      else
         Put_Headers (Title => "");
      end if;
      if SGE.Loggers.Errors_Exist then
         HTML.Begin_Div (ID => "internal_errors");
         HTML.Put_Heading (Title => "Internal errors",
                           Level => 2);
         SGE.Loggers.Iterate_Errors (Put_Error'Access);
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

   procedure View_Hosts (Props : Set_Of_Properties; Queue_Name : String) is
      SGE_Out     : Parser.Tree;
      Selector    : Unbounded_String;
      GPU : String := To_String (Get_GPU (Props));
   begin
      case Get_Network (Props) is
         when eth =>
            Selector := To_Unbounded_String (" -l eth");
            Append_Params ("net=ETH");
         when ib =>
            Selector := To_Unbounded_String (" -l ib");
            Append_Params ("net=IB");
         when ibswitch =>
            Append_Params ("net=IBSWITCH");
         when none =>
            Append_Params ("net=NONE");
      end case;
      Selector := Selector & " -l cm=" & To_String (Get_Model (Props));
      if GPU /= "" and then GPU /= "none" then
         Selector := Selector & " -l gm=" & GPU;
      end if;
      Append_Params ("gm=" & GPU);
      Append_Params ("model=" & Get_Model (Props)'Img);
      Append_Params ("cores=" & Get_Cores (Props)'Img);
      Append_Params ("mem=" & To_String (Get_Memory (Props)));
      Append_Params ("q=" & Queue_Name);

      SGE_Out := Parser.Setup (Command  => "qhost",
                               Selector => "-q -j " & Parser.Resource_Selector
                               & To_String (Selector));

      SGE.Hosts.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "host"));
      SGE.Parser.Free;
      SGE.Hosts.Prune_List (Requirements => Props, Queue_Name => Queue_Name);

      --  Can we factor this out?
      if not HTML.Param_Is ("sort", "") then
         SGE.Hosts.Sort_By (Field     => CGI.Value ("sort"),
                       Direction => Sort_Direction);
      end if;

      Hosts.Put_All;

   exception
      when E : others =>
         HTML.Error ("Error while viewing host: " & Exception_Message (E));
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "host_list");
   end View_Hosts;


   ----------------
   -- Set_Params --
   ----------------

   procedure Set_Params (Params : String) is
   begin
      My_Params := To_Unbounded_String (Params);
   end Set_Params;

   -------------------
   -- Append_Params --
   -------------------

   procedure Append_Params (Params : String) is
   begin
      My_Params := My_Params & "&" & Params;
   end Append_Params;

   function Params return String is
   begin
      return To_String (My_Params);
   end Params;

end Viewer;
