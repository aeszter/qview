with HTML;
with SGE.Ranges; use SGE.Ranges.Range_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Utils; use SGE.Utils; use SGE.Utils.Hash_Strings;

package body Ranges is

   ---------
   -- Put --
   ---------

   procedure Put (S : Step_Range; Label : String) is
   begin
      HTML.Put_Paragraph (Label  => Label,
                          Contents => To_String (What => S, Short => True));
   end Put;

   --------------
   -- Put_Cell --
   --------------

   procedure Put_Cell (Data : Step_Range_List; Class : String) is
      Pos : Range_Lists.Cursor := Data.First;
      S : Unbounded_String;
   begin
      while Pos /= Range_Lists.No_Element loop
         S := S & To_String (What => Element (Pos), Short => True);
         Next (Pos);
         if Pos /= Range_Lists.No_Element then
            S := S & ",";
         end if;
      end loop;
      HTML.Put_Cell (Data => S, Class => Class);
   end Put_Cell;

end Ranges;
