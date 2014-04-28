with Ada.Text_IO;
with SGE.Advance_Reservations;
with HTML;
with SGE.Utils; use SGE.Utils;
with Ada.Strings.Fixed;


package body Advance_Reservations is

   procedure Put_State (R : Reservation);

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Node_Tree : Parser.Tree) is
   begin
      SGE.Advance_Reservations.Append_List (Node_Tree);
   end Append_List;

   ---------
   -- Put --
   ---------

   procedure Put (R : Reservation) is
      procedure Put_Name is
         procedure Put_Error (Message : String) is
         begin
            HTML.Comment (Message);
         end Put_Error;

      begin
         HTML.Begin_Div (Class => "ar_name");
         HTML.Put_Paragraph ("Name", Get_Name (R));
         if Has_Error_Log_Entries (R) then
            Ada.Text_IO.Put_Line ("<em>Internal error log entries present</em>");
         end if;
         Iterate_Error_Log (R, Put_Error'Access);
         HTML.End_Div (Class => "ar_name");
      end Put_Name;

      procedure Put_Meta is
      begin
         HTML.Begin_Div (Class => "ar_meta");
         HTML.Put_Paragraph ("ID", Get_ID (R));
         HTML.Put_Paragraph ("Owner",  SGE.Utils.To_String (Get_Owner (R)));
         Ada.Text_IO.Put ("<p>State: ");
         Put_State (R);
         Ada.Text_IO.Put_Line ("</p>");
         HTML.Put_Clearer;
         HTML.End_Div (Class => "ar_meta");
      end Put_Meta;

      procedure Put_Queues is
      begin
         null;
      end Put_Queues;

      procedure Put_Resources is
      begin
         null;
      end Put_Resources;

   begin
      HTML.Begin_Div (Class => "ar_info");
      Put_Name;
      Put_Meta;
      Put_Queues;
      Put_Resources;
      HTML.End_Div (Class => "ar_info");
   end Put;

   procedure Put_Line (R : Reservation) is
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => Ada.Strings.Fixed.Trim (Get_ID (R), Ada.Strings.Left),
                    Link_Param => "ar_id");
      HTML.Put_Cell (To_String (Get_Owner (R)));
      HTML.Put_Cell (Get_Name (R));
      HTML.Put_Cell (Get_State (R));
      HTML.Put_Time_Cell (Get_Start (R));
      HTML.Put_Duration_Cell (Get_Duration (R));
      Ada.Text_IO.Put ("</tr>");
   end Put_Line;

   -----------------
   -- Put_Details --
   -----------------

   procedure Put_Details is
   begin
      Iterate (Put'Access);
   end Put_Details;

   procedure Put_List is
   begin
      HTML.Begin_Div (Class => "ar_list");
      Ada.Text_IO.Put ("<table><tr>");
      HTML.Put_Header_Cell (Data => "Number");
      HTML.Put_Header_Cell (Data => "Owner");
      HTML.Put_Header_Cell (Data => "Name");
      HTML.Put_Header_Cell (Data => "State");
      HTML.Put_Header_Cell (Data => "Start");
      HTML.Put_Header_Cell (Data => "Duration");
      Ada.Text_IO.Put ("</tr>");

      Iterate (Put_Line'Access);
   end Put_List;

   procedure Put_State (R : Reservation) is
      procedure Put (What : String) renames Ada.Text_IO.put;
   begin
      Put ("<img src=""/icons/" & Get_State (R) & ".png"" ");
      Put ("alt=""" & Get_State (R) & """ title=""" & Get_State (R) & ": ");
--      if Is_Running (R) then
--         Put ("running");
--      end if;
--      if On_Hold (R) then
--         Put ("on hold");
--      end if;
--      if Has_Error (R) then
--         Put ("Error");
--      end if;
      Put (""" />");
   end Put_State;

end Advance_Reservations;
