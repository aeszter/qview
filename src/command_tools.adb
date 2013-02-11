with Ada.Characters.Handling;

package body Command_Tools is

   --------------
   -- Sanitise --
   --  Purpose: Restrict a given string to a safe character set
   --          in order to pass it to the shell
   --------------

   function Sanitise (Input : in String) return String is
      Output : String := Input;

      function Is_Harmless_Dash (Char : in Character; Where : Positive) return Boolean is
      begin
         if Char = '-' and then
              Where > Output'First and then
           Ada.Characters.Handling.Is_Alphanumeric (Output (Where - 1)) then
            return True; -- a dash, not the first character, and the previous one is alphanumeric
            --  so this does not start a commandline switch
         else
            return False; -- not a dash, or not preceded by a harmless character
         end if;
      end Is_Harmless_Dash;


   begin
      for Pos in Output'Range loop
         if not Ada.Characters.Handling.Is_Letter (Output (Pos))
             and then not Ada.Characters.Handling.Is_Decimal_Digit (Output (Pos)) and then not
                   Is_Harmless_Dash (Char  => Output (Pos), Where => Pos) then
            Output (Pos) := '_';
         end if;
      end loop;
      return Output;
   end Sanitise;

end Command_Tools;
