with Ada.Strings;
with Utils; use Utils.String_Lists;

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

   --------------------
   -- To_Hash_String --
   --------------------

   function To_Hash_String (S : String) return Hash_String_Type is
      First : Natural := S'First;
   begin
      if S (First) = ' ' then
         First := S'First + 1;
      end if;
      if S'Last - First + 1 > Hash_Strings.Max_Length then
         raise Ada.Strings.Length_Error;
      end if;
      return Hash_Strings.To_Bounded_String (Source => S (First .. S'Last));
   end To_Hash_String;



end Utils;
