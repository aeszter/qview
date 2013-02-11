with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Resources; use Resources;
with Parser; use Parser;
with HTML;
with Ada.Exceptions; use Ada.Exceptions;
with Debug;

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
            Cores                 : Natural := 0;
            SSD, GPU : Boolean := False;
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
                  A := Get_Attr (N, "name");
                  if Value (A) = "mem_total" then
                     Mem := To_Unbounded_String (Value (First_Child (N)));
                  elsif Value (A) = "num_proc" then
                     Cores := Integer'Value (Value (First_Child (N)));
                  elsif Value (A) = "infiniband" and then
                    small'Value (Value (First_Child (N))) = 1.0 and then
                    Network = none then
                     Network := ib;
                  elsif Value (A) = "ib-switch" and then
                    small'Value (Value (First_Child (N))) = 1.0 then
                     Network := ibswitch;
                  elsif Value (A) = "ethernet" and then
                    small'Value (Value (First_Child (N))) = 1.0 then
                     Network := eth;
                  elsif Value (A) = "h_rt" then
                     Runtime := To_Unbounded_String (Value (First_Child (N)));
                  elsif Value (A) = "cpu_model" then
                     Model := To_Unbounded_String (Value (First_Child (N)));
                  elsif Value (A) = "qname" then
                     Queue_Name := To_Unbounded_String (Value (First_Child (N)));
                  elsif Value (A) = "ssd"  then
                     SSD := True; -- consumable, so do not check numerical value
                  elsif Value (A) = "gpu"  then
                     GPU := True; -- consumable
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
                                    SSD      => SSD,
                                    GPU => GPU,
                                    Runtime  => Runtime,
                                    Name     => Queue_Name,
                                    State    => To_String (State)
                                   ));
         exception
            when E : others =>
               HTML.Error ("Failed to parse queue: " &
                                   Exception_Message (E));
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
                       SSD                   : Boolean;
                       GPU                   : Boolean;
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

      Set_Memory (Q.Properties, Memory);
      Set_Network (Q.Properties, Network);
      Set_Model (Q.Properties, Model);
      Set_Runtime (Q.Properties, Runtime);
      Q.Name     := Name;
      if Cores = 0 then
         Set_Cores (Q.Properties, Q.Total);
         Debug.Log (Message  => "Cores: 0" & ", using Total" & Total'Img,
                    Where    => Debug.Queues,
                    Severity => 2);
      else
         Debug.Log (Message  => "Cores:" & Cores'Img & ", ignoring Total" & Total'Img,
                    Where    => Debug.Queues,
                    Severity => 2);
         Set_Cores (Q.Properties, Cores);
      end if;
      if SSD then
         Set_SSD (Q.Properties);
      end if;
      if GPU then
         Set_GPU (Q.Properties);
      end if;

      return Q;
   end New_Queue;

   ---------------------------
   -- Precedes_By_Resources --
   ---------------------------

   function Precedes_By_Resources (Left, Right : Queue) return Boolean is
   begin
      return Left.Properties < Right.Properties;
   end Precedes_By_Resources;


   function Get_Slot_Count (Q : Queue) return Natural is
   begin
      return Q.Total;
   end Get_Slot_Count;

   function Get_Used_Slots (Q : Queue) return Natural is
   begin
      return Q.Used;
   end Get_Used_Slots;

   function Get_Reserved_Slots (Q : Queue) return Natural is
   begin
      return Q.Reserved;
   end Get_Reserved_Slots;

   function Get_Free_Slots (Q : Queue) return Natural is
   begin
      return Q.Total - Q.Used - Q.Reserved;
   end Get_Free_Slots;

   function Is_Offline (Q : Queue) return Boolean is
   begin
      return Q.Offline;
   end Is_Offline;

   function Is_Suspended (Q : Queue) return Boolean is
   begin
      return Q.Suspended;
   end Is_Suspended;

   function Get_Properties (Q : Queue) return Set_Of_Properties is
   begin
      return Q.Properties;
   end Get_Properties;

   function Get_Name (Q : Queue) return Unbounded_String is
   begin
      return Q.Name;
   end Get_Name;


end Queues;
