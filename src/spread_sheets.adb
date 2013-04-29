with Ada.Characters.Latin_1; use Ada.Characters;

package body Spread_Sheets is

   function New_Cell (Text : Unbounded_String) return Cell is
   begin
      return (Contents => Text,
             Line_Separator => False);
   end New_Cell;

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
            Sheet.Cells.Append (New_Cell (Buffer));
               Buffer := Null_Unbounded_String;
               Sheet.Cells.Append (Line_Separator);
            when others =>
               Append (Buffer, C);
         end case;
      end loop;
   end Parse;

end Spread_Sheets;
