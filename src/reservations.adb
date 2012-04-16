with HTML;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Interfaces.C;
with Ada.Calendar.Conversions;
with Ada.Strings.Hash;

package body Reservations is

   -------------
   -- Put_All --
   -------------

   procedure Put_All is
         procedure Put_Table_Header is
         begin
            HTML.Put_Heading (Title => "Reservations",
                              Level => 2);
            HTML.Begin_Div (Class => "reservation_list");
            Ada.Text_IO.Put_Line ("<table><tr>");
            HTML.Put_Header_Cell (Data     => "Job", Sortable => False);
            HTML.Put_Header_Cell (Data     => "", Sortable => False);
            HTML.Put_Header_Cell (Data     => "Start",
                                  Acronym  => "When the reservation will become effective",
                                  Sortable => False);
            HTML.Put_Header_Cell (Data => "What", Sortable => False);
            HTML.Put_Header_Cell (Data => "Where", Sortable => False);

            Ada.Text_IO.Put ("</tr>");
         end Put_Table_Header;
   begin
      Put_Table_Header;
      List.Iterate (Put'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "reservation_list");
      exception
         when E : others =>
         HTML.Error ("Error while putting reservations: "
                        & Exception_Message (E));
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div (Class => "reservation_list");
   end Put_All;

   ----------
   -- Read --
   ----------

   procedure Read is
      New_Reservation : Reservation;
      Success : Boolean;
   begin
      Open (File => Schedule_File,
            Mode => In_File,
            Name => Schedule_File_Name);
      while not End_Of_File (Schedule_File) loop
         begin
            Read_Line (Data => New_Reservation, Store_Data => Success);
            if Success then
               if Exists_At (What => New_Reservation, Iteration => Iteration_Number - 1) then
                  New_Reservation.Confirmation := True;
               end if;
               if New_Reservation.State = reserving or else
                 not Catalog.Contains ((Schedule_Run => Iteration_Number,
                                       Job_ID       => New_Reservation.Job_ID,
                                        Queue        => New_Reservation.Queue)) then
                  --  also kill STARTING jobs without reservation
                  List.Append (New_Reservation);
                  Catalog.Insert (New_Item  => List.Last_Index,
                               Key => (Schedule_Run => Iteration_Number,
                                       Job_ID       => New_Reservation.Job_ID,
                                       Queue        => New_Reservation.Queue));
               end if;
            end if;
         exception
            when Buffer_Overrun => HTML.Comment ("Line too long: skipped");
            when Improper_Line => null;
            when E : Constraint_Error => HTML.Error ("Error while processing "
                                                     & New_Reservation.State'Img
                                                 & New_Reservation.Job_ID'Img & "@"
                                                     & New_Reservation.Queue & "["
                                                       & Integer'Image (Iteration_Number) & "]: "
                                                  & Exception_Message (E));
            when others => raise;
         end;

      end loop;
      Close (Schedule_File);
   end Read;


   --------
   --  private subprograms follow
   --------

   procedure Read_Line (Data : out Reservation; Store_Data : out Boolean) is
      Line : String (1 .. 100);
      Separators : array (1 .. 9) of Natural; -- position of the colons
      Queue_Separator : Natural; -- position of the @ in the queue name
      Last       : Natural; -- length of the line read
      What            : Character;  -- distinguishes different records in the schedule file
      --  we are only interested in Q, but there are others like H, L, P
   begin
      Get_Line (Schedule_File, Line, Last);
      if Last = Line'Last then
         Skip_Line (Schedule_File);
         raise Buffer_Overrun;
      end if;
      if Line (Line'First) = ':' then
         Iteration_Number := Iteration_Number + 1;
         Store_Data := False;
         return;
      end if;
      if not Is_Digit (Line (Line'First)) then
         raise Improper_Line;
      end if;

      Separators (1) := Line'First - 1;
      for Next_Sep in Separators'First + 1 .. Separators'Last loop
         Separators (Next_Sep) := Index (Source  => Line (Separators (Next_Sep - 1) + 1 .. Last),
                                         Pattern => ":");
      end loop;
      What := Line (Separators (6) + 1);
      if What /= 'Q' then
         Store_Data := False;
         return;
      end if;
      Data.State := Reservation_State'Value (Line (Separators (3) + 1 .. Separators (4) - 1));
      if Data.State = running then
         Store_Data := False;
         return;
      end if;
      Data.Job_ID := Positive'Value (Line (Separators (1) + 1 .. Separators (2) - 1));
      Data.Column2 := Natural'Value (Line (Separators (2) + 1 .. Separators (3) - 1));
      Data.Timestamp := Positive'Value (Line (Separators (4) + 1 .. Separators (5) - 1));
      Data.Duration := Positive'Value (Line (Separators (5) + 1 .. Separators (6) - 1));
      Queue_Separator := Index (Source => Line (Separators (7) .. Separators (8)),
                                Pattern => "@");
      Data.Queue := Line (Queue_Separator + 1 .. Queue_Separator + 9);
      Data.Resource_Type := To_Unbounded_String (Line (Separators (8) + 1 .. Separators (9) - 1));
      Data.Resource_Value := Resource_Value_Type'Value (Line (Separators (9) + 1 .. Last));
      Store_Data := True;
   exception
      when E : Data_Error =>
         HTML.Comment ("Exception at " & Col (Schedule_File)'Img);
         HTML.Error ("Error while reading reservation item for job "
                     & Data.Job_ID'Img & ": " & Exception_Message (E));
      when  others =>
            raise;
   end Read_Line;


   procedure Put (Position : Lists.Cursor) is
      Res : Reservation := Lists.Element (Position);
      Line_Class : String := "res_blank";
   begin
      if Res.State = starting then
         Line_Class := "res_start";
      elsif Res.Confirmation then
         Line_Class := "res_cnfrm";
      else
         Line_Class := "res_shift";
      end if;

      Ada.Text_IO.Put ("<tr class=""" & Line_Class & """>");
      HTML.Put_Cell (Res.Job_ID'Img);
      if Res.State = starting then
         HTML.Put_Cell ("starting");
      elsif Res.Confirmation then
         HTML.Put_Cell ("confirmed");
      else
         HTML.Put_Cell ("new/shifted");
      end if;
      HTML.Put_Time_Cell (Ada.Calendar.Conversions.To_Ada_Time
                          (Interfaces.C.long (Res.Timestamp)));
      if Res.Resource_Type = "slots" then
         HTML.Put_Cell (Integer'Image (Integer (Res.Resource_Value)), Class => "right");
      else
         HTML.Put_Cell (Res.Resource_Type & " =" & Res.Resource_Value'Img);
      end if;
      HTML.Put_Cell (Res.Queue);
      Ada.Text_IO.Put ("</tr>");
   end Put;

   function Equivalent_Cards (Left, Right : Index_Card) return Boolean is
   begin
      if Left.Job_ID = Right.Job_ID and then
        Left.Schedule_Run = Right.Schedule_Run and then
        Left.Queue = Right.Queue then
         return True;
      else
         return False;
      end if;
   end Equivalent_Cards;

   function Card_Hash (Card : Index_Card) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Card.Job_ID'Img & Card.Schedule_Run'Img & Card.Queue);
   end Card_Hash;


   function Exists_At (What : Reservation; Iteration : Natural) return Boolean is
      Card : Catalogs.Cursor := Catalogs.Find (Container => Catalog,
                                               Key       => (Job_ID       => What.Job_ID,
                                                             Schedule_Run => Iteration,
                                                            Queue => What.Queue));
   begin
      if not Catalogs.Has_Element (Card) then
         return False;
      end if;
      if List.Element (Catalogs.Element (Card)) = What then
         return True;
      else
         return False;
      end if;
   end Exists_At;

end Reservations;
