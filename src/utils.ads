with SGE.Utils;

package Utils is
   Version : String := "develop"; -- Update Bugzilla when you change this

   procedure Mark_Mismatch (Left, Right : in out SGE.Utils.String_Sets.Set);

end Utils;
