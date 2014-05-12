with Parser; use Parser;
with SGE.Advance_Reservations; use SGE.Advance_Reservations;

package Advance_Reservations is
   procedure Append_List (Node_Tree : Parser.Tree);
   procedure Put_List;
   procedure Put_Details;

private
   procedure Put (R : Reservation);
   procedure Put_Line (R : Reservation);
end Advance_Reservations;
