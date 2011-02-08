package body Utils is

   ------------------
   -- To_Tri_State --
   ------------------

   function To_Tri_State (Truth : String) return Tri_State is
   begin
      if Truth = "true" then
         return True;
      elsif Truth = "false" then
         return False;
      else
         return Undecided;
      end if;
   end To_Tri_State;

end Utils;
