with Ada.Characters.Latin_1; use Ada.Characters;

package body Spread_Sheets is
   use Cell_Lists;

   function New_Cell (Text : Unbounded_String) return Cell is
   begin
      return (Contents => Text,
             Line_Separator => False);
   end New_Cell;

   function At_End (Sheet : Spread_Sheet) return Boolean is
   begin
      return Sheet.Position = Sheet.Cells.Last;
   end At_End;

   function At_Separator (Sheet : Spread_Sheet) return Boolean is
   begin
      return Element (Sheet.Position).Line_Separator;
   end At_Separator;

   procedure Rewind (Sheet : in out Spread_Sheet) is
   begin
      Sheet.Position := Sheet.Cells.First;
   end Rewind;

   procedure Next (Sheet : in out Spread_Sheet) is
   begin
      Sheet.Position := Next (Sheet.Position);
   end Next;

   function Current (Sheet : Spread_Sheet) return String is
   begin
      if Cell_Lists.Element (Sheet.Position).Line_Separator then
         raise Output_Error with "unexpected separator found";
      else
         return To_String (Cell_Lists.Element (Sheet.Position).Contents);
      end if;
   end Current;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Sheet   : out Spread_Sheet;
      Input   : in out Plain_Pipe_Stream)
   is
      Buffer : Unbounded_String;
      C      : Character;

   begin
      while not Input.Eof loop
         Input.Next_Char (C);
         case C is
            when  Latin_1.HT =>
               Sheet.Cells.Append (New_Cell (Buffer));
               Buffer := Null_Unbounded_String;
            when Latin_1.LF =>
               if Buffer /= Null_Unbounded_String then
                  Sheet.Cells.Append (New_Cell (Buffer));
                  Buffer := Null_Unbounded_String;
               end if;
               Sheet.Cells.Append (Line_Separator);
            when others =>
               Append (Buffer, C);
         end case;
      end loop;
   end Parse;

end Spread_Sheets;
