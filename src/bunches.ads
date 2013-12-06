with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Bunches;

package Bunches is

   procedure Put_List;

   procedure Build_List; -- Bug #1830

private

   procedure Put (B : SGE.Bunches.Bunch);

end Bunches;
