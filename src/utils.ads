with SGE.Utils;

package Utils is
   Version : String := "v3.0"; -- Update Bugzilla when you change this

   procedure Mark_Mismatch (Left, Right : in out SGE.Utils.String_Sets.Set);

   procedure Read_Link (Path : String; Buffer : out String; Last : out Natural);

end Utils;
