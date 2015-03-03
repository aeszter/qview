with SGE.Utils; use SGE.Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with POSIX.C; use POSIX.C;
with System;
with POSIX;
use POSIX;

package body Utils is
   procedure Mark_Mismatch (Left, Right : in out String_Sets.Set) is
      use String_Sets;
      procedure Find_Unbalanced (Position : String_Sets.Cursor);

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

   function readlink (path : char_ptr; buf : System.Address; bufsize : size_t)
     return ssize_t;
   pragma Import (C, readlink, "readlink");

   procedure Read_Link (Path : String; Buffer : out String; Last : out Natural) is
      Pathname_With_NUL : constant POSIX_String := To_POSIX_String (Path) & POSIX.NUL;
      Result : ssize_t;
   begin
      Result := readlink (Pathname_With_NUL (Pathname_With_NUL'First)'Unchecked_Access,
                          Buffer (Buffer'First)'Address,
                          size_t (Buffer'Last - Buffer'First + 1));
      if Result = -1 then
         raise POSIX.POSIX_Error;
      end if;
      Last := Buffer'First + Integer (Result) - 1;
   end Read_Link;

end Utils;
