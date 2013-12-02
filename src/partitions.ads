with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Partitions;

package Partitions is

   subtype Partition is SGE.Partitions.Partition;

   procedure Put (P : Partition);
   procedure Put_List;
   procedure Put_Summary;

   procedure Build_List;
end Partitions;
