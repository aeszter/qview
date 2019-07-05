with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Slurm.Bunches;

package Bunches is

   procedure Put (B : Slurm.Bunches.Bunch);
   procedure Put_List (Source : Slurm.Bunches.List);

   procedure Put_All;

end Bunches;
