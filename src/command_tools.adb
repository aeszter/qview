with Ada.Characters.Handling;

package body Command_Tools is

   --------------
   -- Sanitise --
   --  Purpose: Restrict a given string to a safe character set
   --          in order to pass it to the shell
   --------------

   function Sanitise (Input : in String) return String is
      Output : String := Input;
   begin
      for I in Output'Range loop
         if not Ada.Characters.Handling.Is_Letter (Output (I))
           and then not Ada.Characters.Handling.Is_Decimal_Digit (Output (I)) then
            Output (I) := '_';
         end if;
      end loop;
      return Output;
   end Sanitise;

end Command_Tools;
