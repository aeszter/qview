with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Resources; use Resources;
with HTML;
with Ada.Exceptions; use Ada.Exceptions;

package body Queues is
   use Queue_Lists;

   procedure Sort is
   begin
      Sorting_By_Resources.Sort (List);
   end Sort;

   procedure Rewind is
   begin
      List_Cursor := List.First;
   end Rewind;

   function Empty return Boolean is
   begin
      return List.Is_Empty;
   end Empty;

   function Next return Queue is
   begin
      Next (List_Cursor);
      return Queue_Lists.Element (List_Cursor);
   end Next;

   function At_End return Boolean is
   begin
      if List_Cursor = Queue_Lists.No_Element or else
        List_Cursor = List.Last then
         return True;
      end if;
      return False;
   end At_End;

   function Current return Queue is
   begin
      return Queue_Lists.Element (List_Cursor);
   end Current;

   procedure Append_List (Input_Nodes : Node_List) is
   begin
      for Index in 1 .. Length (Input_Nodes) loop
         declare
            Queue_Nodes : Node_List := Child_Nodes (Item (Input_Nodes, Index - 1));
            N                     : Node;
            A                     : Attr;
            Used, Reserved, Total : Natural := 0;
            State                 : Unbounded_String;
            Mem, Runtime          : Unbounded_String;
            Cores                 : Natural;
            Network               : Resources.Network := none;
            Model, Queue_Name     : Unbounded_String := Null_Unbounded_String;
            type small is digits 4 range 0.0 .. 1.0;
         begin
            for Index in 1 .. Length (Queue_Nodes) loop
               N := Item (Queue_Nodes, Index - 1);
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
                  elsif Value (A) = "qname" then
                     Queue_Name := To_Unbounded_String (Value (First_Child (N)));
                  end if;
               end if;
            end loop;

            List.Append (New_Queue (Used     => Used,
                                    Reserved => Reserved,
                                    Total    => Total,
                                    Memory   => To_String (Mem),
                                    Cores    => Cores,
                                    Network  => Network,
                                    Model    => To_Model (Model),
                                    Runtime  => Runtime,
                                    Name     => Queue_Name,
                                    State    => To_String (State)
                                   ));
         exception
            when E : others =>
               HTML.Put_Paragraph (Label    => "Failed to parse queue",
                                   Contents => Exception_Message (E));
         end;
      end loop;
   end Append_List;

   ---------------
   -- New_Queue --
   --  Purpose: Create a new queue with the given resources and slots
   --  Parameter Used: Number of slots in use
   --  Parameter Reserved: Number of slots used for advance reservations
   --  Parameter Total: Number of total slots in queue
   --  Parameter Memory: RAM in queue
   --  Parameter Cores: number of cores in queue
   --  Parameter Network: type of network in queue
   --  Parameter Runtime: runtime limit of queue
   --  Returns:  the newly created queue
   ---------------

   function New_Queue
     (Used, Reserved, Total : Natural;
                       State                 : String;
                       Memory                : String;
                       Cores                 : Natural;
                       Network               : Resources.Network;
                       Model                 : Resources.CPU_Model;
                       Runtime               : Unbounded_String;
                      Name : Unbounded_String)
      return Queue
   is
      Q : Queue;
   begin
      Q.Used     := Used;
      Q.Reserved := Reserved;
      Q.Total    := Total;
      Q.Offline  := False;
      Q.Suspended := False;
      if State /= "" and then
        Index (Source  => State,
                Pattern => "u") /= 0 then
         Q.Offline := True;
      elsif Index (Source => State, Pattern => "d") /= 0 then
         Q.Suspended := True;
      end if;

      if Memory /= "" then
         Q.Memory := To_Gigs (Memory);
      else
         Q.Memory := 0.0;
      end if;

      Q.Network  := Network;
      Q.Model    := Model;
      Q.Runtime  := Runtime;
      Q.Name     := Name;
      HTML.Comment ("New queue """ & Q.Name & """");
      if Cores = 0 then
         Q.Cores := Q.Total;
      else
         Q.Cores := Cores;
      end if;

      return Q;
   end New_Queue;

   -------------
   -- To_Gigs --
   -------------

   function To_Gigs (Memory : String) return Gigs is
   begin
      if Memory (Memory'Last) = 'G' then
         return Gigs'Value (Memory (Memory'First .. Memory'Last - 1));
      elsif Memory (Memory'Last) = 'M' then
         return Gigs'Value (Memory (Memory'First .. Memory'Last - 1)) / 1024.0;
      else
         raise Constraint_Error with "unknown memory encountered: " & Memory;
      end if;
   end To_Gigs;


   ---------------------------
   -- Precedes_By_Resources --
   ---------------------------

   function Precedes_By_Resources (Left, Right : Queue) return Boolean is
   begin
      if Left.Network < Right.Network then
         return True;
      elsif Left.Network > Right.Network then
         return False;
      elsif Left.Model < Right.Model then
         return True;
      elsif Left.Model > Right.Model then
         return False;
      elsif Left.Memory < Right.Memory then
         return True;
      elsif Left.Memory > Right.Memory then
         return False;
      elsif Left.Cores < Right.Cores then
         return True;
      elsif Left.Cores > Right.Cores then
         return False;
      elsif Left.Runtime < Right.Runtime then
         return True;
      elsif Left.Runtime > Right.Runtime then
         return False;
      else
         return False;
      end if;
   end Precedes_By_Resources;

end Queues;
