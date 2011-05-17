with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with HTML;
with Resources; use Resources;
with Ada.Strings.Fixed;
with Hosts; use Hosts.Job_Lists; use Hosts.Host_Lists;
with Utils; use Utils;

package body Hosts is

   -----------
   -- Equal --
   --  Purpose: Two jobs are equal iff their IDs are identical
   -----------

   function Equal (Left, Right : Job) return Boolean is
   begin
      return Left.ID = Right.ID;
   end Equal;

   -------------
   -- New_Job --
   --  Purpose: Create a new Job record with a given ID and master/slave setting
   --  Parameter ID: The job ID
   --  Parameter PE_Master: either "MASTER" or "SLAVE"
   --  Returns: The newly created Job record
   --  Throws: Constraint_Error if PE_Master is invalid
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

   -------------
   -- New_Job --
   --  Purpose: Create a new Job record with a given ID and master/slave setting
   --  Parameter ID: The job ID
   --  Parameter Master: True for job masters
   --  Returns: The newly created Job record
   -------------

   function New_Job (ID : Positive; Master : Boolean) return Job is
      J : Job;
   begin
      J.Master := Master;
      J.ID := ID;
      if Master then
         J.Slaves := 0;
      else
         J.Slaves := 1;
      end if;
      return J;
   end New_Job;

   ----------------
   -- Compactify --
   --  Purpose: Replace multiple consecutive entries with a single one,
   --  keeping count of the number of original entries
   ----------------

   procedure Compactify (List : in out Job_List) is
      Pos        : Job_Lists.Cursor := List.First;
      Short_List : Job_List;
      Orig       : Job;

   begin
      if List.Is_Empty then
         return;
      end if;
      while Pos /= Job_Lists.No_Element loop
         Orig := Job_Lists.Element (Pos);
         if Short_List.Is_Empty or else
            not Equal (Short_List.Last_Element, Orig) then
            Short_List.Append (Orig);
         else
            Short_List.Update_Element (Position => Short_List.Last,
                                       Process  => Add_Slave_Process'Access);
         end if;
         Next (Pos);
      end loop;
      Job_Lists.Move (Target => List,
                      Source => Short_List);
   exception
      when E : others =>
         HTML.Error ("Unable to sort job list: " & Exception_Message (E));
   end Compactify;


   -----------------------
   -- Update_Used_Slots --
   --  Purpose: Count the number of used slots (from the host's job list)
   --          and store the result in Properties.Used
   --  Parameter H: The host to modify
   -----------------------

   procedure Update_Used_Slots (H : in out Host) is
      Pos        : Job_Lists.Cursor := H.Jobs.First;
      J          : Job;
   begin
      while Pos /= Job_Lists.No_Element loop
         J := Job_Lists.Element (Pos);
         H.Properties.Used := H.Properties.Used + Positive'Max (1, J.Slaves);
         --  serial jobs have J.Slaves = 0 (they are only a master process)
         Next (Pos);
      end loop;
   exception
      when E : others =>
         HTML.Error ("Unable to count slots for host "
                     & To_String (H.Name)
                     & ": " & Exception_Message (E));
         H.Properties.Used := 0;
   end Update_Used_Slots;

   -----------------------
   -- Add_Slave_Process --
   --  Purpose: Increment the Slaves number of a given Job by one
   --  Parameter J: The job to update
   -----------------------

   procedure Add_Slave_Process (J : in out Job) is
   begin
      J.Slaves := J.Slaves + 1;
   end Add_Slave_Process;

   ------------------
   -- Append_Queue --
   ------------------

   procedure Append_Queue (H : out Host; State : String) is
   begin
      H.Queues.Append (To_Unbounded_String (State));
   end Append_Queue;


   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Host_Nodes : Node_List) is
      Value_Nodes : Node_List;
      N, V        : Node;
      A           : Attr;
      H           : Host;

   begin
      Hosts :
      for I in 1 .. Length (Host_Nodes) loop
         N := Item (Host_Nodes, I - 1);
         H.Properties.Network := none;
         H.Properties.Model := none;
         H.Jobs := Job_Lists.Empty_List;
         H.Queues := String_Lists.Empty_List;
         H.Properties.Cores := 1;
         H.Properties.Memory := 0.0;
         H.Properties.Used := 0;
         A := Get_Named_Item (Attributes (N), "name");
         H.Name := To_Unbounded_String (Value (A));
         if Value (A) /= "global" then
            Value_Nodes := Child_Nodes (N);
            Host_Attributes :
            for J in 1 .. Length (Value_Nodes) loop
               V := Item (Value_Nodes, J - 1);
               if Name (V) = "resourcevalue" then
                  Parse_Resource (H => H, N => V);
               elsif Name (V) = "hostvalue" then
                  Parse_Hostvalue (H => H, N => V);
               elsif Name (V) = "job" then
                  Parse_Job (H => H, N => V);
               elsif Name (V) = "queue" then
                  Parse_Queue (H => H, N => V);
               end if;
            end loop Host_Attributes;
            Compactify (H.Jobs);
            Update_Used_Slots (H);
            Host_List.Append (H);
         end if;
      end loop Hosts;
   exception
      when E :
         others => HTML.Error ("Unable to parse hosts: " & Exception_Message (E));
         HTML.Error ("Node " & Name (V) & ":" & Value (V));
         HTML.Error ("Host " & Name (A) & ":" & Value (A));
   end Append_List;

   ----------------
   -- Prune_List --
   --  Purpose: Prune the Host list by removing all entries that do not
   --          fulfill the given requirements
   --  Parameter Network: Network requirement
   --  Parameter Cores: Required number of Cores. Note: this is not a minimum
   --           requirement, but must be matched exactly
   --  Parameter Memory: Required amount of Memory, must be matched exactly
   --  Parameter Runtime: Required Queue h_rt, must be matched exactly
   ----------------

   procedure Prune_List (Net, Cores, Memory, Runtime : String) is
      pragma Unreferenced (Runtime); -- hosts do not have an associated runtime
                                       --  Bug #999: maybe we can call qstat to get the
                                       --  necessary values?
      Temp      : Host_Lists.List;
      Pos       : Host_Lists.Cursor := Host_List.First;
      H         : Host;
      Mem       : Gigs;
      Req_Cores : Positive;
      Req_Net   : Network;
   begin
      Req_Net := To_Network (Net);
      Req_Cores := Positive'Value (Cores);
      Mem := Gigs'Value (Memory);
      loop
         exit when Pos = Host_Lists.No_Element;
         H := Host_Lists.Element (Pos);
         if H.Properties.Network = Req_Net and then
           H.Properties.Memory = Mem and then
           H.Properties.Cores = Req_Cores  then
               Temp.Append (H);
         end if;
         Next (Pos);
      end loop;
      Host_List := Temp;
   end Prune_List;

   -----------------
   -- Parse_Queue --
   -----------------

   procedure Parse_Queue (H : in out Host; N : Node) is
      A, Q_Name : Attr;
      pragma Unreferenced (Q_Name);
      --  probably not needed; not storing this simplifies the Host type
      --  since we can use a list of strings instead of a customized list
      Q_Values : Node_List := Child_Nodes (N);
      Q_Value : Node;
   begin
      Q_Name := Get_Named_Item (Attributes (N), "name");

      Queue_Values :
      for I in 1 .. Length (Q_Values) loop
         Q_Value := Item (List  => Q_Values,
                          Index => I - 1);
         if Name (Q_Value) = "queuevalue" then
            A := Get_Named_Item (Attributes (Q_Value), "name");
            if Value (A) = "state_string" and then
               Has_Child_Nodes (Q_Value) then
               Append_Queue (H     => H,
                             State => Value (First_Child (Q_Value)));
               HTML.Comment ("appended");
            end if;
         end if;
      end loop Queue_Values;


   exception
      when E : others =>
         HTML.Error ("Unable to read queue: " & Exception_Message (E));
   end Parse_Queue;

   ---------------------
   -- Parse_Resources --
   --  Purpose: Given a Node of an XML DOM tree,
   --  read in resources of a Host
   --  Parameter H: The Host to update
   --  Parameter V: The XML Node to read from
   ---------------------

   procedure Parse_Resource (H : in out Host; N : Node) is
      A : Attr;
   begin
      A := Get_Named_Item (Attributes (N), "name");
      if Value (A) = "num_proc" then
         H.Properties.Cores := Integer (Fixed'Value (Value (First_Child (N))));
      elsif
        Value (A) = "ethernet" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 then
            H.Properties.Network := eth;
         end if;
      elsif
         Value (A) = "infiniband" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 then
            H.Properties.Network := ib;
         end if;
      elsif Value (A) = "cpu_model" then
         H.Properties.Model := To_Model (Value (First_Child (N)));
      elsif Value (A) = "mem_total" then
         H.Properties.Memory := To_Gigs (Value (First_Child (N)));
      else
         HTML.Put_Paragraph (Value (A), Value (First_Child (N)));
      end if;
   exception
      when E : others =>
         HTML.Error ("Unable to read resource: " & Exception_Message (E));
   end Parse_Resource;


   ---------------------
   -- Parse_Hostvalue --
   ---------------------

   procedure Parse_Hostvalue (H : in out Host; N : Node) is
      A : Attr;
   begin
      A := Get_Named_Item (Attributes (N), "name");
      if Value (A) = "load_avg" then
         begin
            H.Load := Fixed'Value (Value (First_Child (N)));
            exception when Constraint_Error => H.Load := 0.0;
         end;
      elsif Value (A) = "mem_used" then
         begin
            H.Mem_Used := To_Gigs (Value (First_Child (N)));
            exception when Constraint_Error => H.Mem_Used := 0.0;
         end;
      elsif Value (A) = "swap_total" then
         begin
            H.Swap_Total := To_Gigs (Value (First_Child (N)));
            exception when Constraint_Error => H.Swap_Total := 0.0;
         end;
      elsif Value (A) = "swap_used" then
         begin
            H.Swap_Used := To_Gigs (Value (First_Child (N)));
            exception when Constraint_Error => H.Swap_Used := 0.0;
         end;
      end if;
   exception
      when E : others =>
         HTML.Error ("Could not parse host value: " & Exception_Message (E));
   end Parse_Hostvalue;

   ---------------
   -- Parse_Job --
   --  Purpose: Given an XML DOM Node, read in information for a Job
   --  on a Host
   --  Parameter H: The Host to update
   --  Parameter N: The XML Node to read from
   ---------------

   procedure Parse_Job (H : in out Host; N : Node) is
      A     : Attr;
      C     : Node;
      Nodes : Node_List;
      ID    : Natural;
   begin
      Nodes := Child_Nodes (N);
      Job_Attributes :
      for K in 1 .. Length (Nodes) loop
         C := Item (Nodes, K - 1);
         if Name (C) = "jobvalue" then
            A := Get_Named_Item (Attributes (C), "name");
            if Value (A) = "pe_master" then
               ID := Integer'Value (Value (Get_Named_Item (Attributes (C), "jobid")));
               H.Jobs.Append (New_Job (ID, Value (First_Child (C))));
            end if;
         end if;
      end loop Job_Attributes;
   exception
      when E : others =>
         HTML.Error ("Unable to parse job: " & Exception_Message (E));
   end Parse_Job;

   ---------
   -- Put --
   --  Purpose : Output one Host as an HTML <tr>,
   --    adding one <tr> for every Job on the Host
   --  Parameter Pos : Cursor pointing to the Job record to output
   ---------

   ---------
   -- Put --
   ---------

   procedure Put (Cursor : Host_Lists.Cursor) is
      H : Host := Host_Lists.Element (Cursor);
      Free : Natural;
   begin
      Free := H.Properties.Cores - H.Properties.Used;
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => H.Name);
      HTML.Put_Cell (Data => H.Properties.Network'Img);
      HTML.Put_Cell (Data => H.Properties.Model'Img);
      HTML.Put_Cell (Data => H.Properties.Cores'Img, Class => "right");
      HTML.Put_Cell (Data => Free'Img, Class => "right");
      HTML.Put_Cell (Data => H.Properties.Memory'Img, Class => "right");
      HTML.Put_Cell (Data  => Load_Per_Core (H)'Img,
                     Class => "right " & Color_Class (Load_Per_Core (H)));
      HTML.Put_Cell (Data  => Mem_Percentage (H)'Img,
                     Class => "right " & Color_Class (Mem_Percentage (H)));
                     HTML.Put_Cell (Data  => Swap_Percentage (H)'Img,
                                    Class => "right " & Color_Class (Swap_Percentage (H)));
      H.Queues.Iterate (Put_Status'Access);
      Ada.Text_IO.Put ("</tr>");
      H.Jobs.Iterate (Put_Jobs'Access);
   exception
      when E : others =>
         HTML.Error ("Error while putting host "& To_String (H.Name) & ": "
                     & Exception_Message (E));
         Ada.Text_IO.Put ("</tr>");
   end Put;

   --------------
   -- Put_Jobs --
   --------------

   procedure Put_Jobs (Cursor : Job_Lists.Cursor) is
      J : Job := Job_Lists.Element (Cursor);
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => ""); -- H.Name
      if J.Master then
         if J.Slaves > 0 then -- master
            HTML.Put_Cell (Data => "<img src=""/icons/master.png"" />" & J.Slaves'Img,
                           Class => "right");
         else -- serial
            HTML.Put_Cell (Data => "<img src=""/icons/serial.png"" />" & "1",
                           Class => "right");
         end if;
      else
         HTML.Put_Cell (Data => J.Slaves'Img, Class => "right");
      end if;
      HTML.Put_Cell (Data => Ada.Strings.Fixed.Trim (J.ID'Img, Ada.Strings.Left),
                    Link_Param => "job_id");

      Ada.Text_IO.Put ("</tr>");
   end Put_Jobs;

   ----------------
   -- Put_Status --
   ----------------

   procedure Put_Status (Cursor : Utils.String_Lists.Cursor) is
      S : String := To_String (String_Lists.Element (Cursor));
   begin
      HTML.Put_Img_Cell (Image => S);
   end Put_Status;

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
