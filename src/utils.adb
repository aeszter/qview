with Ada.Strings;
with Utils; use Utils.String_Lists;
with Ada.Text_IO;

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

   --------------
   -- Put_List --
   --------------

   procedure Put_List (List : String_Lists.List) is
      Elem : String_Lists.Cursor;
   begin
      Elem := List.First;
      Ada.Text_IO.Put ("<ul>");
      if Elem = String_Lists.No_Element then
         Ada.Text_IO.Put_Line ("<img src=""/icons/cross.png"" alt=""empty"" title=""empty"" />");
      else
         while Elem /= String_Lists.No_Element loop
            Ada.Text_IO.Put_Line ("<li>" & To_String (String_Lists.Element (Elem)) & "</li>");
            Next (Elem);
         end loop;
      end if;
      Ada.Text_IO.Put ("</ul>");
   end Put_List;


end Utils;
