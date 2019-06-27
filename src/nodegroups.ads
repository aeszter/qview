with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Slurm.Nodegroups;

package Nodegroups is

   procedure Put (G : Slurm.Nodegroups.Nodegroup);
   procedure Put_List (Source : Slurm.Nodegroups.Summarized_List);
   procedure Put_Summary;

   procedure Put_All;
end Nodegroups;
