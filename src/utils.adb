with SGE.Utils; use SGE.Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Utils is
   procedure Mark_Mismatch (Left, Right : in out String_Sets.Set) is
      use String_Sets;
      Mismatch, Marked : Set;

      procedure Find_Unbalanced (Position : String_Sets.Cursor) is
      begin
         if Mismatch.Contains (Element (Position)) then
            Marked.Include ("<em>" & Element (Position) & "</em>");
         else
            Marked.Include (Element (Position));
         end if;
      end Find_Unbalanced;

   begin
      Mismatch := Symmetric_Difference (Left, Right);
      Left.Iterate (Find_Unbalanced'Access);
      Left := Marked;
      Marked := String_Sets.Empty_Set;
      Right.Iterate (Find_Unbalanced'Access);
      Right := Marked;
   end Mark_Mismatch;
end Utils;
