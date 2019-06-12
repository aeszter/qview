
package Utils is
   Version : String := "v4.2"; -- Update Bugzilla when you change this

   procedure Read_Link (Path : String; Buffer : out String; Last : out Natural);
   function To_String (Source : Integer) return String;

end Utils;
