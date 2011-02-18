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
      N, V, C     : Node;
      A           : Attr;
      H           : Host;
      Job_ID      : Positive;
      Job_Nodes : Node_List;

   begin
      for I in 1 .. Length (Host_Nodes) loop
         N := Item (Host_Nodes, I - 1);
         H.Properties.Network := none;
         H.Properties.Model := none;
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
               elsif Value (A) = "cpu_model" then
                  H.Properties.Model := To_Model (Value (First_Child (V)));
               elsif Value (A) = "mem_total" then
                  H.Properties.Memory := To_Gigs (Value (First_Child (V)));
               else
                  HTML.Put_Paragraph (Value (A), Value (First_Child (V)));
               end if;
            elsif Name (V) = "hostvalue" then
               A := Get_Named_Item (Attributes (V), "name");
               if Value (A) = "load_avg" then
                  begin
                     H.Load := Fixed'Value (Value (First_Child (V)));
                     exception when Constraint_Error => H.Load := 0.0;
                  end;
               elsif Value (A) = "mem_used" then
                  begin
                     H.Mem_Used := To_Gigs (Value (First_Child (V)));
                     exception when Constraint_Error => H.Mem_Used := 0.0;
                  end;
               elsif Value (A) = "swap_total" then
                  begin
                     H.Swap_Total := To_Gigs (Value (First_Child (V)));
                     exception when Constraint_Error => H.Swap_Total := 0.0;
                  end;
               elsif Value (A) = "swap_used" then
                  begin
                     H.Swap_Used := To_Gigs (Value (First_Child (V)));
                     exception when Constraint_Error => H.Swap_Used := 0.0;
                  end;
               end if;
            elsif Name (V) = "job" then
               Job_Nodes := Child_Nodes (V);
               for K in 1 .. Length (Job_Nodes) loop
                  C := Item (Job_Nodes, K - 1);
                  if Name (C) = "jobvalues" then
                     A := Get_Named_Item (Attributes (C), "name");
                     if Value (A) = "pe_master" then
                        Job_ID := Integer'Value (Value (Get_Named_Item (Attributes (C), "jobid")));
                        H.Jobs.Append (New_Job (Job_ID, Value (First_Child (C))));
                     end if;
                  end if;
               end loop;
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
      HTML.Put_Cell (Data => H.Name);
      HTML.Put_Cell (Data => H.Properties.Network'Img);
      HTML.Put_Cell (Data => H.Properties.Model'Img);
      HTML.Put_Cell (Data => H.Properties.Cores'Img, Class => "right");
      HTML.Put_Cell (Data => H.Properties.Memory'Img, Class => "right");
      HTML.Put_Cell (Data  => Load_Per_Core (H)'Img,
                     Class => "right " & Color_Class (Load_Per_Core (H)));
      HTML.Put_Cell (Data  => Mem_Percentage (H)'Img,
                     Class => "right " & Color_Class (Mem_Percentage (H)));
                     HTML.Put_Cell (Data  => Swap_Percentage (H)'Img,
                                    Class => "right " & Color_Class (Swap_Percentage (H)));
      Ada.Text_IO.Put ("</tr>");
      H.Jobs.Iterate (Put_Jobs'Access);
   exception
      when E : others =>
         HTML.Error ("Error while putting host: " & Exception_Message (E));
         Ada.Text_IO.Put ("</tr>");
   end Put;

   procedure Put_Jobs (Cursor : Job_Lists.Cursor) is
      J : Job := Job_Lists.Element (Cursor);
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => ""); -- H.Name
      if J.Master then
         HTML.Put_Cell (Data => "<img src=""/icons/eye.png"" />");
      else
         HTML.Put_Cell (Data => "");
      end if;
      HTML.Put_Cell (Data => J.ID'Img);

      Ada.Text_IO.Put ("</tr>");
   end Put_Jobs;

   -------------------
   -- Load_Per_Core --
   --  Purpose: Compute the Unix load per CPU core
   --  Parameter H: The host record under consideration
   -------------------

   function Load_Per_Core (H : Host) return Fixed is
   begin
      if H.Properties.Cores = 0 then
         raise Constraint_Error with "host """ & To_String (H.Name)
           & """ has no cores";
      else
         return H.Load / H.Properties.Cores;
      end if;
   end Load_Per_Core;

   ---------------
   -- Mem_Ratio --
   --  Purpose: Compute the percentage of RAM used
   --  Parameter H: The host record under consideration
   --  Returns: RAM used, 1 means all RAM is used
   ---------------

   function Mem_Ratio (H : Host) return Fixed is
   begin
      if H.Properties.Memory = 0.0 then
         raise Constraint_Error with "host """ & To_String (H.Name)
           & """ has no memory";
      else
         return H.Mem_Used / H.Properties.Memory;
      end if;
   end Mem_Ratio;

   --------------------
   -- Mem_Percentage --
   --  Purpose: Compute the percentage of RAM used
   --  Parameter H: The host record under consideration
   --  Returns: RAM used, 100 means all RAM is used
   --------------------

   function Mem_Percentage (H : Host) return Percent is
   begin
      return Percent (Mem_Ratio (H) * 100.0);
   end Mem_Percentage;

   ---------------
   -- Swap_Ratio --
   --  Purpose: Compute the percentage of swap space used
   --  Parameter H: The host record under consideration
   --  Returns: swap used, 1 means all swap is used
   ---------------

   function Swap_Ratio (H : Host) return Fixed is
   begin
      if H.Swap_Total = 0.0 then
         raise Constraint_Error with "host """ & To_String (H.Name)
           & """ has no swap space";
      else
         return H.Swap_Used / H.Swap_Total;
      end if;
   end Swap_Ratio;

   ---------------------
   -- Swap_Percentage --
   --  Purpose: Compute the percentage of swap space used
   --  Parameter H: The host record under consideration
   --  Returns: swap used, 100 means all swap is used
   ---------------------

   function Swap_Percentage (H : Host) return Percent is
   begin
      return Percent (Swap_Ratio (H) * 100.0);
   end Swap_Percentage;

   -----------------
   -- Color_Class --
   --  Purpose: translate a percentage to a string suitable
   --  for use as a CSS class
   --  Parameter P : percentage to classify
   --  Returns: one of "pct_cold", "pct_low", "pct_med",
   --  "pct_high", or "pct_hot"
   -----------------

   function Color_Class (P : Percent) return String is
   begin
      if P < 10 then
         return "pct_cold";
      elsif P < 30 then
         return "pct_low";
      elsif P < 60 then
         return "pct_med";
      elsif P < 90 then
         return "pct_high";
      else
         return "pct_hot";
      end if;
   end Color_Class;

   -----------------
   -- Color_Class --
   --  Purpose: translate a load value to a string suitable
   --  for use as a CSS class
   --  Parameter Load: the load to classify
   --  Returns: one of "load_cold", "load_low", "load_normal",
   --  "load_high", "load_extreme"
   -----------------

   function Color_Class (Load : Fixed) return String is
   begin
      if Load < 0.1 then
         return "load_cold";
      elsif Load < 0.8 then
         return "load_low";
      elsif Load < 1.1 then
         return "load_normal";
      elsif Load < 1.5 then
         return "load_high";
      else
         return "load_extreme";
      end if;
   end Color_Class;


end Hosts;
