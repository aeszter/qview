with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with HTML;
with Resources; use Resources;

package body Hosts is

   -------------
   -- New_Job --
   -------------

   function New_Job (ID : Positive; PE_Master : String) return Job is
   begin
      if PE_Master = "MASTER" then
         return New_Job (ID => ID, Master => True);
      elsif
        PE_Master = "SLAVE" then
         return New_Job (ID => ID, Master => False);
      else
         raise Constraint_Error with "Expected ""MASTER"" or ""SLAVE"", found """
           & PE_Master & """";
      end if;
   end New_Job;

   function New_Job (ID : Positive; Master : Boolean) return Job is
      J : Job;
   begin
      J.Master := Master;
      J.ID := ID;
      return J;
   end New_Job;


   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Host_Nodes : Node_List) is
      Value_Nodes : Node_List;
      N, V        : Node;
      A           : Attr;
      H           : Host;
      Job_ID      : Positive;
      type Fixed is digits 4;

   begin
      for I in 1 .. Length (Host_Nodes) loop
         N := Item (Host_Nodes, I - 1);
         A := Get_Named_Item (Attributes (N), "name");
         H.Name := To_Unbounded_String (Value (A));
         Value_Nodes := Child_Nodes (N);
         for J in 1 .. Length (Value_Nodes) loop
            V := Item (Value_Nodes, J - 1);
            if Name (V) = "resourcevalue" then
               A := Get_Named_Item (Attributes (V), "name");
               if Value (A) = "num_proc" then
                  H.Properties.Cores := Integer (Fixed'Value (Value (First_Child (V))));
               elsif
                 Value (A) = "ethernet" then
                  if Fixed'Value (Value (First_Child (V))) = 1.0 then
                     H.Properties.Network := eth;
                  end if;
               elsif
                  Value (A) = "infiniband" then
                  if Fixed'Value (Value (First_Child (V))) = 1.0 then
                     H.Properties.Network := ib;
                  end if;
               else
                  HTML.Put_Paragraph (Value (A), Value (First_Child (V)));
               end if;
            elsif Name (V) = "jobvalue" then
               A := Get_Named_Item (Attributes (V), "name");
               if Value (A) = "pe_master" then
                  Job_ID := Integer'Value (Value (Get_Named_Item (Attributes (V), "jobid")));
                  H.Jobs.Append (New_Job (Job_ID, Value (First_Child (V))));
               end if;
            end if;
         end loop;
         Host_List.Append (H);
      end loop;
   exception
      when E :
         others => HTML.Error ("Unable to parse job: " & Exception_Message (E));
         HTML.Error ("Node " & Name (V) & ":" & Value (V));
         HTML.Error ("Attrib " & Name (A) & ":" & Value (A));
   end Append_List;


   ---------
   -- Put --
   ---------

   procedure Put (Cursor : Host_Lists.Cursor) is
      H : Host := Host_Lists.Element (Cursor);
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => H.Properties.Cores'Img);
      HTML.Put_Cell (Data => H.Properties.Network'Img);
      Ada.Text_IO.Put ("</tr>");
   exception
      when E : others =>
         HTML.Error ("Error while putting host: " & Exception_Message (E));
         Ada.Text_IO.Put ("</tr>");
   end Put;

end Hosts;
