with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with HTML;
with Resources; use Resources;
with Ada.Strings.Fixed;
with Hosts; use Hosts.Job_Lists; use Hosts.Host_Lists;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Debug;

package body Hosts is

   ----------------------
   -- Precedes_By_Free --
   ----------------------

   function Precedes_By_Free (Left, Right : Host) return Boolean is
   begin
      return Get_Free_Slots (Left) < Get_Free_Slots (Right);
   end Precedes_By_Free;

   function Get_Free_Slots (H : Host) return Natural is
   begin
      return Get_Cores (H.Properties) - H.Slots_Used;
   end Get_Free_Slots;

   ----------------------
   -- Precedes_By_Swap --
   ----------------------

   function Precedes_By_Swap (Left, Right : Host) return Boolean is
   begin
      return Swap_Ratio (Left) < Swap_Ratio (Right);
   end Precedes_By_Swap;

   ---------------------
   -- Precedes_By_Mem --
   ---------------------

   function Precedes_By_Mem (Left, Right : Host) return Boolean is
   begin
      return Mem_Ratio (Left) < Mem_Ratio (Right);
   end Precedes_By_Mem;

   ----------------------
   -- Precedes_By_Load --
   ----------------------

   function Precedes_By_Load (Left, Right : Host) return Boolean is
   begin
      return Left.Load < Right.Load;
   end Precedes_By_Load;

   ---------------------
   -- Precedes_By_RAM --
   ---------------------

   function Precedes_By_RAM (Left, Right : Host) return Boolean is
   begin
      return Get_Memory (Left.Properties) < Get_Memory (Right.Properties);
   end Precedes_By_RAM;

   -----------------------
   -- Precedes_By_Cores --
   -----------------------

   function Precedes_By_Cores (Left, Right : Host) return Boolean is
   begin
      return Get_Cores (Left.Properties) < Get_Cores (Right.Properties);
   end Precedes_By_Cores;

   ---------------------
   -- Precedes_By_Net --
   ---------------------

   function Precedes_By_Net (Left, Right : Host) return Boolean is
   begin
      return Get_Network (Left.Properties) < Get_Network (Right.Properties);
   end Precedes_By_Net;

   ----------------------
   -- Precedes_By_Name --
   ----------------------

   function Precedes_By_Name (Left, Right : Host) return Boolean is
   begin
      return Left.Name < Right.Name;
   end Precedes_By_Name;

   -------------
   -- Sort_By --
   -------------

   procedure Sort_By (Field, Direction : String) is
   begin
      if Field = "Free" then
         By_Free.Sort (Host_List);
      elsif Field = "Name" then
         By_Name.Sort (Host_List);
      elsif Field = "Interconnect" then
         By_Net.Sort (Host_List);
      elsif Field = "Cores" then
         By_Cores.Sort (Host_List);
      elsif Field = "RAM" then
         By_RAM.Sort (Host_List);
      elsif Field = "Load" then
         By_Load.Sort (Host_List);
      elsif Field = "Mem" then
         By_Mem.Sort (Host_List);
      elsif Field = "Swap" then
         By_Swap.Sort (Host_List);
      else
         HTML.Error ("Sorting by " & Field & " unimplemented");
      end if;
      if Direction = "dec" then
         Host_List.Reverse_Elements;
      end if;
   end Sort_By;

   -----------
   -- Equal --
   --  Purpose: Two jobs are equal iff their IDs and Task_IDs are identical
   -----------

   function Equal (Left, Right : Job) return Boolean is
   begin
      return Left.ID = Right.ID and then Left.Task_ID = Right.Task_ID;
   end Equal;

   -------------
   -- Set_Master --
   --  Purpose: Set a Job's Master field
   --  Parameter J: The Job to modify
   --  Parameter PE_Master: either "MASTER" or "SLAVE"
   --  Throws: Constraint_Error if PE_Master is invalid
   -------------

   procedure Set_Master (J : in out Job; PE_Master : String) is
   begin
      if PE_Master = "MASTER" then
         J.Master := True;
      elsif
        PE_Master = "SLAVE" then
         J.Master := False;
      else
         raise Constraint_Error with "Expected ""MASTER"" or ""SLAVE"", found """
           & PE_Master & """";
      end if;
   end Set_Master;

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
         H.Slots_Used := H.Slots_Used + Positive'Max (1, J.Slaves);
         --  serial jobs have J.Slaves = 0 (they are only a master process)
         Next (Pos);
      end loop;
   exception
      when E : others =>
         HTML.Error ("Unable to count slots for host "
                     & To_String (H.Name)
                     & ": " & Exception_Message (E));
         H.Slots_Used := 0;
   end Update_Used_Slots;

   -----------------------
   -- Add_Slave_Process --
   --  Purpose: Increment the Slaves number of a given Job by one
   --  Parameter J: The job to update
   --  Note: Routine is called once for the second and every subsequent
   --        Job entry with a given ID on the host.
   --        It works correctly (i.e. J.Slaves quals the number of slave processes)
   --        if a) there is a master, and it is the first in the list (experience
   --              shows that it is);
   --        or b) there is no master, but at least two slaves (this is true for
   --              our current configuration).
   --        Serial jobs need special treatment.
   -----------------------

   procedure Add_Slave_Process (J : in out Job) is
   begin
      if not J.Master and then J.Slaves = 0 then
         --  we have not yet counted ourself
         J.Slaves := 2;
      else
         J.Slaves := J.Slaves + 1;
      end if;
   end Add_Slave_Process;

   ------------------
   -- Append_Queue --
   ------------------

   procedure Append_Queue (H : out Host; Name, State : String) is
   begin
      H.Queues.Insert (Key      => To_Unbounded_String (Name),
                       New_Item => Queue_States.To_Bounded_String (State));
   end Append_Queue;


   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Host_Nodes : Node_List) is
      Value_Nodes : Node_List;
      N, V        : Node;
      A           : Attr;

   begin
      Hosts :
      for I in 1 .. Length (Host_Nodes) loop
         declare
            H : Host;
         begin
            N := Item (Host_Nodes, I - 1);
            A := Get_Attr (N, "name");
            H.Name := To_Unbounded_String (Value (A));
            if Value (A) /= "global" then
               Value_Nodes := Child_Nodes (N);
               Host_Attributes :
               for J in 1 .. Length (Value_Nodes) loop
                  V := Item (Value_Nodes, J - 1);
                  if Name (V) = "resourcevalue" then
                     Parse_Resource (Props => H.Properties, N => V);
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
         end;
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

   procedure Prune_List (Net, Cores, Memory, Queue_Name, Model,
                         SSD, GPU                             : String) is
      Temp      : Host_Lists.List;
      Pos       : Host_Lists.Cursor := Host_List.First;
      H         : Host;
      Requirements : Set_Of_Properties;
   begin
      Init (Props => Requirements, Net => Net,
            Memory => Memory,
            Cores  => Cores,
            Model  => Model,
            SSD    => SSD,
           GPU => GPU);
      loop
         exit when Pos = Host_Lists.No_Element;
         H := Host_Lists.Element (Pos);
         if H.Properties = Requirements and then
           H.Queues.Contains (To_Unbounded_String (Queue_Name)) then
            Temp.Append (H);
         else
            Debug.Log (Message  => To_String (H.Name) & ": " & Get_Mismatch (H.Properties, Requirements),
                       Where    => Debug.Default,
                       Severity => 1);
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
      Q_Values  : Node_List := Child_Nodes (N);
      Q_Value   : Node;
   begin
      Q_Name := Get_Attr (N, "name");

      Queue_Values :
      for I in 1 .. Length (Q_Values) loop
         Q_Value := Item (List  => Q_Values,
                          Index => I - 1);
         if Name (Q_Value) = "queuevalue" then
            A := Get_Attr (Q_Value, "name");
            if Value (A) = "state_string" then
               if Has_Child_Nodes (Q_Value) then
                  Append_Queue (H     => H,
                                Name  => Value (Q_Name),
                                State => Value (First_Child (Q_Value)));
               else
                  Append_Queue (H     => H,
                                Name  => Value (Q_Name),
                                State => "");
               end if;
            end if;
         end if;
      end loop Queue_Values;


   exception
      when E : others =>
         HTML.Error ("Unable to read queue: " & Exception_Message (E));
   end Parse_Queue;



   ---------------------
   -- Parse_Hostvalue --
   ---------------------

   procedure Parse_Hostvalue (H : in out Host; N : Node) is
      A : Attr;
   begin
      A := Get_Attr (N, "name");
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
      J     : Job;
   begin
      Nodes := Child_Nodes (N);
      Job_Attributes :
      for K in 1 .. Length (Nodes) loop
         C := Item (Nodes, K - 1);
         if Name (C) = "jobvalue" then
            A := Get_Attr (C, "name");
            if Value (A) = "pe_master" then
               J.ID := Integer'Value (Value (Get_Attr (C, "jobid")));
               Set_Master (J, Value (First_Child (C)));
            elsif Value (A) = "taskid" then
               J.Task_ID := Integer'Value (Value (First_Child (C)));
            end if;
         end if;
      end loop Job_Attributes;
      H.Jobs.Append (J);

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
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => H.Name);
      HTML.Put_Cell (Data => Get_Network (H.Properties)'Img);
      HTML.Put_Cell (Data => Get_Model (H.Properties)'Img);
      HTML.Put_Cell (Data => Get_Cores (H.Properties)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Free_Slots (H)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Memory (H.Properties)'Img, Class => "right");
      HTML.Put_Cell (Data  => Load_Per_Core (H)'Img,
                     Class => "right " & Color_Class (Load_Per_Core (H)));
      HTML.Put_Cell (Data  => Mem_Percentage (H)'Img,
                     Class => "right " & Color_Class (Mem_Percentage (H)));
                     HTML.Put_Cell (Data  => Swap_Percentage (H)'Img,
                                    Class => "right " & Color_Class (Swap_Percentage (H)));
      H.Queues.Iterate (Put_Queue'Access);
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

   procedure Put_Queue (Cursor : Queue_Maps.Cursor) is
      S : String := Queue_States.To_String (Queue_Maps.Element (Cursor));
   begin
      HTML.Put_Cell (Data => Queue_Maps.Key (Cursor));
      HTML.Put_Img_Cell (Image => S);
   end Put_Queue;

   -------------------
   -- Load_Per_Core --
   --  Purpose: Compute the Unix load per CPU core
   --  Parameter H: The host record under consideration
   -------------------

   function Load_Per_Core (H : Host) return Fixed is
   begin
      return H.Load / Get_Cores (H.Properties);
   exception
      when others =>
      if Get_Cores (H.Properties) = 0 then
         raise Constraint_Error with "host """ & To_String (H.Name)
              & """ has no cores";
      else
         raise;
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
      return H.Mem_Used / Get_Memory (H.Properties);
   exception
      when others =>
         if Get_Memory (H.Properties) = 0.0 then
            raise Constraint_Error with "host """ & To_String (H.Name)
              & """ has no memory";
         else
            raise;
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
