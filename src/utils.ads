
package Utils is
   Version : String := "v4.16"; -- Update Bugzilla when you change this

   User_Error : exception;

   procedure Read_Link (Path : String; Buffer : out String; Last : out Natural);
   function To_String (Source : Integer) return String;

end Utils;
