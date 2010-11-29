with Ada.Text_IO, CGI, System, Pipe_Commands, Pipe_Streams;
use Pipe_Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Sax.Readers;
with DOM.Readers, DOM.Core; use DOM.Core;
with DOM.Core.Documents, DOM.Core.Nodes, DOM.Core.Attrs; use DOM.Core.Documents, DOM.Core.Nodes, DOM.Core.Attrs;
with HTML;

procedure Viewer is
   type AStdioFileID is new System.Address;
   --  pointer to a C stdio file id

   sgeroot : constant String := "/cm/shared/apps/sge/current";
   Reader : DOM.Readers.Tree_Reader;
   SGE_Out : DOM.Core.Document;
   SGE_Command : Pipe_Stream;
   List : Node_List;
   Children : Node_List;
   N : Node;
   C : Node;

   Q_Name      : Unbounded_String;
   Q_Load      : Unbounded_String;
   Q_Used      : Unbounded_String;
   Q_Reserved  : Unbounded_String;
   Q_Available : Unbounded_String;
   Q_Total     : Unbounded_String;
   Q_Disabled  : Unbounded_String;
   Q_Offline   : Unbounded_String;
   --  Cluster Queue Statistics

   J_Number          : Unbounded_String;
   J_Name            : Unbounded_String;
   J_Owner           : Unbounded_String;
   J_Priority        : Unbounded_String;
   J_State           : Unbounded_String;
   J_Slots           : Unbounded_String;
   J_PE              : Unbounded_String;
   J_Submission_Time : Unbounded_String;
   --  Job List Entries
begin
   CGI.Put_CGI_Header;
   Ada.Text_IO.Put_Line ("<html><head><title>Owl Status</title>");
   Ada.Text_IO.Put_Line ("<link style=status.css />");
   Ada.Text_IO.Put_Line ("</head><body>");
   CGI.Put_HTML_Heading (Title => "Owl Status", Level => 1);

   begin
      SGE_Command.Set_Public_Id ("qstat");
      SGE_Command.execute ("SGE_ROOT=" & sgeroot & " "
                      & sgeroot & "/bin/lx26-amd64/qstat -g c -xml" & ASCII.NUL,
                           Pipe_Commands.read_file);
      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (SGE_Command);
      SGE_Out := Reader.Get_Tree;
      SGE_Command.close;

      CGI.Put_HTML_Heading (Title => "Cluster Queues",
                            Level => 2);
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
         N := Item (List, Index - 1);
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
         HTML.Put_UCell (Q_Name);
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
   end;

   --  Global Jobs
   Reader.Start_Document;
   SGE_Command.execute ("SGE_ROOT=" & sgeroot & " "
                   & sgeroot & "/bin/lx26-amd64/qstat -u \* -r -xml" & ASCII.NUL,
                        Pipe_Commands.read_file);
   Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
   Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
   Reader.Parse (SGE_Command);
   SGE_Out := Reader.Get_Tree;
   SGE_Command.close;

   CGI.Put_HTML_Heading (Title => "Jobs",
                         Level => 2);
   --  Fetch Jobs
   List := Get_Elements_By_Tag_Name (SGE_Out, "job_list");
   --  Table Header
   Ada.Text_IO.Put ("<table><tr>");
   HTML.Put_Cell (Data => "Number", Tag => "th");
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
      N := Item (List, Index - 1);
      Children := Child_Nodes (N);
      for Ch_Index in 0 .. Length (Children) - 1 loop
         C := Item (Children, Ch_Index);
         if Name (C) = "JB_job_number" then
            J_Number :=  To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JAT_prio" then
            J_Priority :=  To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JB_name" then
            J_Name :=  To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JB_owner" then
            J_Owner :=  To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "state" then
            J_State :=  To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "JB_submission_time" then
            J_Submission_Time := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "slots" then
            J_Slots := To_Unbounded_String (Value (First_Child (C)));
         elsif Name (C) = "requested_pe" then
            J_PE :=  To_Unbounded_String (Value (First_Child (C)));
         end if;
      end loop;
      HTML.Put_UCell (Data => J_Number);
      HTML.Put_UCell (Data => J_Owner);
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
   CGI.Put_HTML_Tail;
end Viewer;
