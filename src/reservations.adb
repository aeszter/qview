with HTML;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Interfaces.C;
with Ada.Calendar.Conversions;
with Ada.Strings.Hash;

package body Reservations is
   use Queue_Lists;

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
            HTML.Put_Header_Cell (Data => "Slots", Sortable => False);
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
      Success : Boolean;
   begin
      Open (File => Schedule_File,
            Mode => In_File,
            Name => Schedule_File_Name);
      while not End_Of_File (Schedule_File) loop
         declare
            New_Reservation : Reservation;
            Card            : Catalogs.Cursor;

            procedure Update_Queue (Element : in out Reservation) is
            begin
               Element.Queue.Union (Source => New_Reservation.Queue);
            end Update_Queue;

         begin
            Read_Line (Data => New_Reservation, Store_Data => Success);
            if Success then
               Card := Catalog.Find (Key => (Job_ID => New_Reservation.Job_ID,
                                             Schedule_Run => Iteration_Number));
               if Catalogs.Has_Element (Card) then
                  List.Update_Element (Index => Catalogs.Element (Card),
                                       Process => Update_Queue'Access);
               else
                  HTML.Comment ("No card");
                  if Exists_At (What => New_Reservation, Iteration => Iteration_Number - 1) then
                     New_Reservation.Confirmation := True;
                  else
                     New_Reservation.Confirmation := False;
                     if Reserving_Jobs.Contains (New_Reservation.Job_ID) then
                        New_Reservation.Shifted := True;
                     end if;
                  end if;
                  if New_Reservation.State = reserving then
                     Reserving_Jobs.Include (New_Reservation.Job_ID);
                  end if;

                  if New_Reservation.State = reserving or else
                    (New_Reservation.State = starting and then
                    not Catalog.Contains ((Schedule_Run => Iteration_Number,
                                          Job_ID       => New_Reservation.Job_ID)) and then
                       Reserving_Jobs.Contains (New_Reservation.Job_ID)) then

                     List.Append (New_Reservation);
                     Catalog.Insert (New_Item  => List.Last_Index,
                                  Key => (Schedule_Run => Iteration_Number,
                                          Job_ID       => New_Reservation.Job_ID));
                  end if;
               end if;
            end if;
         exception
            when Buffer_Overrun => HTML.Comment ("Line too long: skipped");
            when Improper_Line => null;
            when E : Constraint_Error =>
               if New_Reservation.Queue.Is_Empty then
                  HTML.Error ("Error while processing "
                                                     & New_Reservation.State'Img
                                                 & New_Reservation.Job_ID'Img & "@"
                                                     & "(no queue) ["
                                                       & Integer'Image (Iteration_Number) & "]: "
                              & Exception_Message (E));
               else
                  HTML.Error ("Error while processing "
                                                     & New_Reservation.State'Img
                                                 & New_Reservation.Job_ID'Img & "@"
                                                     & New_Reservation.Queue.First_Element.Name & "["
                                                       & Integer'Image (Iteration_Number) & "]: "
                              & Exception_Message (E));
               end if;
            when others => raise;
         end;

      end loop;
      Close (Schedule_File);
   end Read;


   --------
   --  private subprograms follow
   --------

   function Precedes (Left, Right : Queue) return Boolean is
   begin
      return Left.Name < Right.Name;
   end Precedes;

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
         Search :
         for Index in Separators (Next_Sep - 1) + 1 .. Last loop
            if Line (Index) = ':' then
               Separators (Next_Sep) := Index;
               exit Search;
            end if;
         end loop Search;
--         Separators (Next_Sep) := Index (Source  => Line (Separators (Next_Sep - 1) + 1 .. Last),
--                                         Pattern => ":");
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
      if Line (Separators (8) + 1 .. Separators (9) - 1) -- resource type
        /= "slots" then
         raise Read_Error with "resource type not ""slots"" for 'Q' entry";
      end if;

      Data.Job_ID := Positive'Value (Line (Separators (1) + 1 .. Separators (2) - 1));
      Data.Column2 := Natural'Value (Line (Separators (2) + 1 .. Separators (3) - 1));
      Data.Timestamp := Positive'Value (Line (Separators (4) + 1 .. Separators (5) - 1));
      Data.Duration := Positive'Value (Line (Separators (5) + 1 .. Separators (6) - 1));
      Queue_Separator := Index (Source => Line (Separators (7) .. Separators (8)),
                                Pattern => "@");
      Data.Queue.Insert ((Name => Line (Queue_Separator + 1 .. Queue_Separator + 9),
                                Slots => Integer (Resource_Value_Type'Value (Line (Separators (9) + 1 .. Last)))));
      --  Data.Resource_Type := To_Unbounded_String (Line (Separators (8) + 1 .. Separators (9) - 1));
      --  Data.Resource_Value := Resource_Value_Type'Value (Line (Separators (9) + 1 .. Last));
      --  for now, ignore these
      --  we can change our policy later, when (and if) we take into account non-Q lines
      --  but note the performance impact of unbounded strings
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
      elsif Res.Shifted then
         Line_Class := "res_shift";
      else
         Line_Class := "res_first";
      end if;

      Ada.Text_IO.Put ("<tr class=""" & Line_Class & """>");
      HTML.Put_Cell (Res.Job_ID'Img);
      if Res.State = starting then
         HTML.Put_Cell ("starting");
      elsif Res.Confirmation then
         HTML.Put_Cell ("confirmed");
      elsif Res.Shifted then
         HTML.Put_Cell ("shifted");
      else
         HTML.Put_Cell ("new");
      end if;
      HTML.Put_Time_Cell (Ada.Calendar.Conversions.To_Ada_Time
                          (Interfaces.C.long (Res.Timestamp)));
      HTML.Put_Cell (To_String (Res.Queue));
      Ada.Text_IO.Put ("</tr>");
   end Put;

   function Equivalent_Cards (Left, Right : Index_Card) return Boolean is
   begin
      if Left.Job_ID = Right.Job_ID and then
        Left.Schedule_Run = Right.Schedule_Run then
         return True;
      else
         return False;
      end if;
   end Equivalent_Cards;

   function Equivalent_Reservations (Left, Right : Reservation) return Boolean is
   begin
      if Left.Job_ID = Right.Job_ID and then
        Left.State = Right.State and then
        Left.Timestamp = Right.Timestamp and then
        Left.Duration = Right.Duration then
         return True;
      else
         return False;
      end if;
   end Equivalent_Reservations;

   function Card_Hash (Card : Index_Card) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Card.Job_ID'Img & Card.Schedule_Run'Img);
   end Card_Hash;


   function Exists_At (What : Reservation; Iteration : Natural) return Boolean is
      Card : Catalogs.Cursor := Catalogs.Find (Container => Catalog,
                                               Key       => (Job_ID       => What.Job_ID,
                                                             Schedule_Run => Iteration));
   begin
      if not Catalogs.Has_Element (Card) then
         return False;
      end if;
      if Equivalent_Reservations (List.Element (Catalogs.Element (Card)), What) then
         return True;
      else
         return False;
      end if;
   end Exists_At;

   function To_String (Source : Queue) return String is
   begin
      return Integer'Image (Source.Slots) & '@' & Source.Name;
   end To_String;

   function To_String (Source : Queue_List; Start : Queue_Lists.Cursor) return String is
   begin
      if Start = Source.Last then
         return To_String (Queue_Lists.Element (Start));
      else
         return To_String (Queue_Lists.Element (Start)) & To_String (Source => Source,
                                                                     Start  => Queue_Lists.Next (Start));
      end if;
   end To_String;

   function To_String (Source : Queue_List) return String is
   begin
      if Source.Is_Empty then
         return "";
      else
         return To_String (Source => Source, Start => Source.First);
      end if;
   end To_String;

end Reservations;