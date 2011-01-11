with Ada.Text_IO;
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
      if Memory (Memory'Last) = 'G' then
         Q.Memory := Gigs'Value (Memory (Memory'First .. Memory'Last - 1));
      elsif Memory (Memory'Last) = 'M' then
         Q.Memory := Gigs'Value (Memory (Memory'First .. Memory'Last - 1)) / 1024.0;
      else
         Ada.Text_IO.Put_Line ("<i>unknown memory encountered: "
                               & Memory & "</i>");
         Q.Memory := 0.0;
      end if;

      Q.Network  := Network;
      Q.Runtime  := Runtime;
      Q.Cores    := Integer'Value (Cores);

      return Q;
   end New_Queue;

end Queues;
