with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Plain_Pipe_Streams; use Plain_Pipe_Streams;

package Spread_Sheets is
   type Cell is
      record
         Contents : Unbounded_String := Null_Unbounded_String;
         Line_Separator : Boolean := False;
      end record;
   package Cell_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Cell);
   subtype Cell_List is Cell_Lists.List;

   type Spread_Sheet is tagged private;

   Line_Separator : Cell := (Contents       => Null_Unbounded_String,
                             Line_Separator => True);


   procedure Parse (Sheet   : out Spread_Sheet;
                    Input   : in out Plain_Pipe_Stream);

   function At_End (Sheet : Spread_Sheet) return Boolean;
   function At_Separator (Sheet : Spread_Sheet) return Boolean;
   procedure Rewind (Sheet : in out Spread_Sheet);
   procedure Next (Sheet : in out Spread_Sheet);
   function Current (Sheet : Spread_Sheet) return String;

   Output_Error : exception;
   Input_Error : exception;

private
   type Spread_Sheet is tagged record
      Cells    : Cell_List;
      Position : Cell_Lists.Cursor;
   end record;
   function New_Cell (Text : Unbounded_String) return Cell;


end Spread_Sheets;
