with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Resources; use SGE.Resources;

package Resources is

   Error : exception renames SGE.Resources.Resource_Error;

   procedure Put (Pos : Resource_Lists.Cursor);
   procedure Put_List (L : SGE.Resources.Hashed_List);

end Resources;
