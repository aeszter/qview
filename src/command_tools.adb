package body Command_Tools is

   --------------
   -- Sanitise --
   --  Purpose: Restrict a given string to a safe character set
   --          in order to pass it to the shell
   --------------

   function Sanitise (Input : in String) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Sanitise unimplemented");
      raise Program_Error with "Unimplemented function Sanitise";
      return Sanitise (Input);
   end Sanitise;

end Command_Tools;
