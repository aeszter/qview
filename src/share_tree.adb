with HTML;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

package body Share_Tree is

   --------------
   -- Put_List --
   --------------

   procedure Put_List is
   begin
      HTML.Begin_Div (Class => "share_tree");
      Ada.Text_IO.Put ("<table><tr>");
      HTML.Put_Header_Cell (Data => "User");
      HTML.Put_Header_Cell (Data => "Usage");
      HTML.Put_Header_Cell (Data => "Current");
      HTML.Put_Header_Cell (Data => "Tickets");
      HTML.Put_Header_Cell (Data => "CPU",
                            Acronym => "in CPU years");
      HTML.Put_Header_Cell (Data => "LT CPU");
      HTML.Put_Header_Cell (Data => "Memory",
                            Acronym => "in TB years");
      HTML.Put_Header_Cell (Data => "IO",
                            Acronym => "in TB");
      HTML.Put_Header_Cell (Data => "Job count");
      HTML.Put_Header_Cell (Data => "raw CPU");
      Ada.Text_IO.Put ("</tr>");

      List.Iterate (Put'Access);
      --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "share_tree");
   end Put_List;

   -----------------
   -- Put_Summary --
   -----------------

   procedure Put_Summary is
   begin
      HTML.Begin_Div (ID => "sharetree_summary");
      Ada.Text_IO.Put ("<ul>");
      Ada.Text_IO.Put ("<li> CPU:");
      Ada.Text_IO.Put (Scale_CPU (Total_CPU) & " yr");
      Ada.Text_IO.Put ("</li>");

      Ada.Text_IO.Put ("<li> Mem:");
      Ada.Text_IO.Put (Scale_Memory (Total_Mem) & " TB yr");
      Ada.Text_IO.Put ("</li>");

      Ada.Text_IO.Put ("<li> I/O:");
      Ada.Text_IO.Put (Scale_IO (Total_IO) & " TB");
      Ada.Text_IO.Put ("</li>");

      Ada.Text_IO.Put ("</ul>");
      HTML.End_Div (ID => "sharetree_summary");
   end Put_Summary;

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Cells : in out Spread_Sheet) is
   begin
      Cells.Rewind;
      loop
         declare
            User : User_Node;
         begin
            User.User_Name := To_User_Name (Cells.Current);
            Cells.Next;
            User.Usage := Usage_Number'Value (Cells.Current);
            Cells.Next;
            User.CPU := Usage_Number'Value (Cells.Current);
            Total_CPU := Total_CPU + User.CPU;
            Cells.Next;
            User.LT_CPU := Usage_Number'Value (Cells.Current);
            Cells.Next;
            User.Mem := Usage_Number'Value (Cells.Current);
            Total_Mem := Total_Mem + User.Mem;
            Cells.Next;
            User.IO := Usage_Number'Value (Cells.Current);
            Total_IO := Total_IO + User.IO;
            Cells.Next;
            User.Job_Count := Usage_Integer'Value (Cells.Current);
            Cells.Next;
            List.Append (User);
            Total_Usage := Total_Usage + User.Usage;
            exit when Cells.At_End; -- just in case the data does not end in a newline
            if Cells.At_Separator then
               Cells.Next;
            else
               raise SGE.Spread_Sheets.Output_Error
               with "EOL expected, found """ & Cells.Current & """";
            end if;
            exit when Cells.At_End;
         end;
      end loop;
   exception
      when E : SGE.Spread_Sheets.Output_Error =>
         HTML.Error ("Error reading user data: " & Exception_Message (E));
      when Constraint_Error =>
         HTML.Error ("Error converting user data: " & Cells.Current);
   end Append_List;

   -------------
   -- Sort_By --
   -------------

   procedure Sort_By (Field : String; Direction : String) is
   begin
      if Field = "User" then
         Sorting_By_User.Sort (List);
      elsif Field = "Usage" then
         Sorting_By_Usage.Sort (List);
      elsif Field = "Tickets" then
         Sorting_By_Tickets.Sort (List);
      elsif Field = "CPU" then
         Sorting_By_CPU.Sort (List);
      elsif Field = "LT CPU" then
            Sorting_By_LT_CPU.Sort (List);
      elsif Field = "Memory" then
            Sorting_By_Memory.Sort (List);
      elsif Field = "IO" then
            Sorting_By_IO.Sort (List);
      elsif Field = "Job count" then
         Sorting_By_Job_Count.Sort (List);
      elsif Field = "Current" then
         Sorting_By_Occupation.Sort (List);
      else
         HTML.Error ("Sorting by " & Field & " unimplemented");
      end if;
      if Direction = "dec" then
         List.Reverse_Elements;
      end if;
   end Sort_By;

   procedure Put (Item : Share_Lists.Cursor) is
      User : constant User_Node := Share_Lists.Element (Item);
      Current_Usage : Occupation;
   begin
      if 1 > 0 then
         Ada.Text_IO.Put ("<tr class=""hot"">");
      elsif 0 > 0 then
         Ada.Text_IO.Put ("<tr class=""cold"">");
      else
         Ada.Text_IO.Put ("<tr>");
      end if;

      HTML.Put_Cell (Data => To_String (User.User_Name));
      HTML.Put_Cell (Data => Scale_Usage (User.Usage), Class => "right");
      if Occupation_List.Contains (User.User_Name) then
         Current_Usage := Occupation_List.Element (User.User_Name);
         HTML.Put_Cell (Data => Current_Usage.Slots'Img & " (" & Current_Usage.Tasks'Img & ")",
                       Class => "right");
      else
         HTML.Put_Cell ("");
      end if;
      if User.Tickets /= 0 then
         HTML.Put_Cell (Data => User.Tickets'Img,
                       Class => "right");
      else
         HTML.Put_Cell ("");
      end if;
      HTML.Put_Cell (Data    => Scale_CPU (User.CPU), Class => "right");
      HTML.Put_Cell (Data => Scale_CPU (User.LT_CPU), Class => "right");
      HTML.Put_Cell (Data    => Scale_Memory (User.Mem), Class => "right");
      HTML.Put_Cell (Data    => Scale_IO (User.IO), Class => "right");
      HTML.Put_Cell (Data => User.Job_Count'Img, Class => "right");
      HTML.Put_Cell (Data => User.CPU'Img, Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put;

   function Scale_CPU (Raw_Value : Usage_Number) return String is
   begin
      return Usage_Number'Image (Raw_Value / 3_600 / 24 / 365); -- seconds to years
   end Scale_CPU;

   function Scale_Memory (Raw_Value : Usage_Number) return String is
   begin
      return Usage_Number'Image (Raw_Value / 1_024 / 3_600 / 24 / 365); -- GBs to TBy
   end Scale_Memory;

   function Scale_IO (Raw_Value : Usage_Number) return String is
   begin
      return Usage_Number'Image (Raw_Value / 1_024); -- GB to TB
   end Scale_IO;

   function Scale_Usage (Raw_Value : Usage_Number) return String is
   begin
      return Usage_Number'Image (Raw_Value / Total_Usage * 100); -- arbitrary to percent
   end Scale_Usage;

   function Precedes_By_User (Left, Right : User_Node) return Boolean is
   begin
      return Left.User_Name < Right.User_Name;
   end Precedes_By_User;

   function Precedes_By_Usage (Left, Right : User_Node) return Boolean is
   begin
      return Left.Usage < Right.Usage;
   end Precedes_By_Usage;

   function Precedes_By_CPU (Left, Right : User_Node) return Boolean is
   begin
      return Left.CPU < Right.CPU;
   end Precedes_By_CPU;

   function Precedes_By_LT_CPU (Left, Right : User_Node) return Boolean is
   begin
      return Left.LT_CPU < Right.LT_CPU;
   end Precedes_By_LT_CPU;

   function Precedes_By_Memory (Left, Right : User_Node) return Boolean is
   begin
      return Left.Mem < Right.Mem;
   end Precedes_By_Memory;

   function Precedes_By_IO (Left, Right : User_Node) return Boolean is
   begin
      return Left.IO < Right.IO;
   end Precedes_By_IO;

   function Precedes_By_Job_Count (Left, Right : User_Node) return Boolean is
   begin
      return Left.Job_Count < Right.Job_Count;
   end Precedes_By_Job_Count;

   function Precedes_By_Occupation (Left, Right : User_Node) return Boolean is
      Left_Slots, Right_Slots : Natural := 0;
   begin
      if Occupation_List.Contains (Left.User_Name) then
         Left_Slots := Occupation_List.Element (Left.User_Name).Slots;
      end if;
      if Occupation_List.Contains (Right.User_Name) then
         Right_Slots := Occupation_List.Element (Right.User_Name).Slots;
      end if;

      return Left_Slots < Right_Slots;
   end Precedes_By_Occupation;

   function Precedes_By_Tickets (Left, Right : User_Node) return Boolean is
   begin
      return Left.Tickets < Right.Tickets;
   end Precedes_By_Tickets;

   procedure Update_Occupation (J : SGE.Jobs.Job) is
      use Occupation_Lists;
      procedure Count_Job (Key : User_Name_String; Item : in out Occupation);

      Position : Cursor;
      User : constant User_Name_String := J.Get_Owner;

      procedure Count_Job (Key : User_Name_String; Item : in out Occupation) is
         pragma Unreferenced (Key);
      begin
         Item.Slots := Item.Slots + Integer'Value (To_String (J.Get_Slot_Number));
         Item.Tasks := Item.Tasks + 1;
      end Count_Job;
   begin
      Position := Occupation_List.Find (User);
      if Position = No_Element then
         Occupation_List.Insert (Key => User, New_Item => (Slots => Integer'Value (To_String (J.Get_Slot_Number)), Tasks => 1));
      else
         Occupation_Lists.Update_Element (Container => Occupation_List,
                                          Position  => Position,
                                          Process   => Count_Job'Access);
      end if;
   end Update_Occupation;

   procedure Read_Current_Status is
   begin
      SGE.Jobs.Iterate (Update_Occupation'Access);
   end Read_Current_Status;

   procedure Read_Tickets is
      use Share_Lists;
      procedure Copy_Tickets (Item : in out User_Node);
      procedure Update_Tickets (Position : Share_Lists.Cursor);

      procedure Copy_Tickets (Item : in out User_Node) is
      begin
         Item.Tickets := Get_User_Tickets (To_String (Item.User_Name));
      end Copy_Tickets;

      procedure Update_Tickets (Position : Share_Lists.Cursor) is
      begin
         Update_Element (Container => List,
                         Position  => Position,
                         Process   => Copy_Tickets'Access);
      end Update_Tickets;

   begin
      List.Iterate (Update_Tickets'Access);
   end Read_Tickets;

end Share_Tree;
