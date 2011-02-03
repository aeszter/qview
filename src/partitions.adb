with Ada.Containers.Doubly_Linked_Lists;
with Queues; use Queues.Queue_Lists;
with Resources; use Resources;

package body Partitions is

   -------------------
   -- New_Partition --
   --  Purpose: Build a partition list from a queue list.
   --           This totals all slots (available, used, reserved, ...) for the
   --           for all matching queues.
   --  Parameter Q_List: List of queues to work on.
   --  Parameter Part_List: The new partition list.
   --  Side Effect: Q_List is sorted by resources.
   -------------------

   procedure Build_List (Q_List : in out Queues.Queue_Lists.List;
                          Part_List : out Partition_Lists.List) is
      P : Partition;
      Q : Queue;
      Cursor : Queues.Queue_Lists.Cursor;
   begin
      Sorting_By_Resources.Sort (Q_List);
      Cursor := Q_List.First;
      Q := Element (Cursor);

      --  Create Partition according to first Queue
      P := New_Partition (Q);
      while Cursor /= No_Element loop
         Q := Element (Cursor);
         --  New Partition?
         if not (P.Network = Q.Network and then
                   P.Model = Q.Model and then
                  P.Memory = Q.Memory and then
                   P.Cores = Q.Cores and then
                 P.Runtime = Q.Runtime) then
            --  Yes. Store previous one.
            Part_List.Append (P);
            P := New_Partition (Q);
         end if;

         --  Update totals
         P.Total := P.Total + Q.Total;
         if Q.Offline then
            P.Offline := P.Offline + Q.Total;
         elsif Q.Suspended then
            P.Suspended := P.Suspended + Q.Total;
         else
            P.Used := P.Used + Q.Used;
            P.Reserved := P.Reserved + Q.Reserved;
            P.Available := P.Available + Q.Total - Q.Reserved - Q.Used;
         end if;
         --  Advance
         Cursor := Next (Cursor);
      end loop;
      --  That's it. Store final partition.
      Part_List.Append (P);
   end Build_List;

   -------------------
   -- New_Partition --
   -------------------

   function New_Partition (Q : Queue) return Partition is
      P : Partition;
   begin
      P.Network   := Q.Network;
      P.Memory    := Q.Memory;
      P.Cores     := Q.Cores;
      P.Runtime   := Q.Runtime;
      P.Model     := Q.Model;
      P.Total     := 0;
      P.Offline   := 0;
      P.Suspended := 0;
      P.Used      := 0;
      P.Reserved  := 0;
      P.Available := 0;
      return P;
   end New_Partition;

   ---------------------
   -- Model_As_String --
   ---------------------

   function Model_As_String (P : Partition) return String is
   begin
      return To_String (P.Model);
   end Model_As_String;

end Partitions;
