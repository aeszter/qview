with Resources; use Resources;
with Parser; use Parser;
with HTML;
with Ada.Exceptions; use Ada.Exceptions;
with Debug;
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Lightsout;

package body Queues is
   use Queue_Lists;

   procedure Put_Selected (Selector : not null access function (Q : Queue) return Boolean) is
      Position : Queue_Lists.Cursor := List.First;
   begin
      while Position /= Queue_Lists.No_Element loop
         if Selector (Element (Position)) then
            Put_For_Maintenance (Position);
         end if;
         Next (Position);
      end loop;
   end Put_Selected;

   procedure Put_For_Maintenance (Cursor : Queue_Lists.Cursor) is
      Q : Queue := Queue_Lists.Element (Cursor);
      State : String := "   ";
      Long_Name : String := To_String (Q.Long_Name);
      Separator : Positive := Ada.Strings.Fixed.Index (Source => Long_Name,
                                            Pattern => "@");
      Host_Name : String := Long_Name (Separator + 1 .. Long_Name'Last);
   begin
      if Q.Q_Type (B) then
         State (1) := 'B';
      end if;
      if Q.Q_Type (I) then
         State (2) := 'I';
      end if;
      if Q.Q_Type (P) then
         State (3) := 'P';
      end if;

      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Q.Long_Name);
      HTML.Put_Cell (State);
      HTML.Put_Cell (Data => Lightsout.Get_Maintenance (Host_Name));
      HTML.Put_Cell (Data => Lightsout.Get_Bug (Host_Name), Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put_For_Maintenance;

end Queues;
