with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Resources; use Resources;

package body Queues is

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
      Memory, Cores         : String;
      Network               : Resources.Network;
      Runtime               : Unbounded_String)
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
         if Memory (Memory'Last) = 'G' then
            Q.Memory := Gigs'Value (Memory (Memory'First .. Memory'Last - 1));
         elsif Memory (Memory'Last) = 'M' then
            Q.Memory := Gigs'Value (Memory (Memory'First .. Memory'Last - 1)) / 1024.0;
         else
            Ada.Text_IO.Put_Line ("<i>unknown memory encountered: "
                               & Memory & "</i>");
            Q.Memory := 0.0;
         end if;
      else
         Q.Memory := 0.0;
      end if;

      Q.Network  := Network;
      Q.Runtime  := Runtime;
      if Cores = "" then
         Q.Cores := Q.Total;
      else
         Q.Cores := Integer'Value (Cores);
      end if;

      return Q;
   end New_Queue;

   ---------------------------
   -- Precedes_By_Resources --
   ---------------------------

   function Precedes_By_Resources (Left, Right : Queue) return Boolean is
   begin
      if Left.Network < Right.Network then
         return True;
      elsif Left.Network > Right.Network then
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
