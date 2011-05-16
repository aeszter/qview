with Ada.Text_IO;             use Ada.Text_IO;
with CGI;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with GNAT.Calendar;
with GNAT.Calendar.Time_IO;   use GNAT.Calendar.Time_IO;
with Ada.Real_Time;

package body HTML is

   --------------------
   -- Put_Search_Box --
   --------------------

   procedure Put_Search_Box is
   begin
      Put ("<li><form>");
      Put ("<input type=""text"" name=""search"" size=""8"" value=""search"">");
      Put ("</form></li>");
   end Put_Search_Box;

   --------------
   -- Put_Cell --
   --  Purpose: Write a table cell with given contents.
   --  Input: Data: Contents of the cell;
   --  Input: Tag: (optional) tag to use instead of <td> (e.g. <th>)
   --  Input: Link_Param: (optional) CGI Parameter to use in link, sc.
   --        <a href="the_url?Link_Param=Data">Data</a>
   --------------

   procedure Put_Cell
     (Data       : String;
      Link_Param : String   := "";
      Acronym    : String   := "";
      Tag        : String   := "td";
      Class      : String   := "";
      Colspan    : Positive := 1)
   is
      Close_Tag : String := "</" & Tag & ">";
   begin
      --  Start open tag
      Put ("<" & Tag);

      if Class /= "" then
         Put (" class=""" & Class & """");
      end if;
      if Colspan /= 1 then
         Put (" colspan=""" & Colspan'Img & """");
      end if;
      Put (">");
      --  Open tag ended

      --  Start link
      if Link_Param /= "" then
         Put
           ("<a href=""" &
            CGI.My_URL &
            "?" &
            Link_Param &
            "=" &
            Data &
            """>");
      end if;

      if Acronym /= "" then
         Put ("<acronym title=""" & Acronym & """>");
      end if;

      Put (Data);

      if Acronym /= "" then
         Put ("</acronym>");
      end if;

      if Link_Param /= "" then
         Put ("</a>");
      end if;
      --  Link ended

      Put_Line (Close_Tag);
   end Put_Cell;

   --------------
   -- Put_Cell --
   --------------

   procedure Put_Cell
     (Data       : Unbounded_String;
      Link_Param : String := "";
      Tag        : String := "td";
      Class      : String := "")
   is
   begin
      Put_Cell
        (Data       => To_String (Data),
         Link_Param => Link_Param,
         Tag        => Tag,
         Class      => Class);
   end Put_Cell;

   ------------------
   -- Put_Img_Cell --
   ------------------

   procedure Put_Img_Cell (Image : String) is
      Data : String :=
         "<img src=""/icons/" &
         Image &
         ".png"" alt=""" &
         Image &
         """ title=""" &
         Image &
         """ />";
   begin
      Put_Cell (Data => Data);
   end Put_Img_Cell;

   -------------------
   -- Put_Time_Cell --
   -------------------

   procedure Put_Time_Cell (Time : Calendar.Time) is
   begin
      Put_Cell (To_String (Time));
   end Put_Time_Cell;

   -----------------------
   -- Put_Duration_Cell --
   -----------------------

   procedure Put_Duration_Cell (Secs : Natural) is
      Days : Natural;
      Dur  : Duration;
   begin
      Days := Secs / 86400;
      Dur  :=
         Ada.Real_Time.To_Duration
           (Ada.Real_Time.Seconds (Secs - Days * 86_400));
      if Days > 0 then
         Put_Cell
           (Data  => Days'Img & "d " & Ada.Calendar.Formatting.Image (Dur),
            Class => "right");
      else
         Put_Cell
           (Data  => Ada.Calendar.Formatting.Image (Dur),
            Class => "right");
      end if;
   end Put_Duration_Cell;

   -----------------------
   -- Put_Duration_Cell --
   -----------------------

   procedure Put_Duration_Cell (Span : Duration) is
      Days    : Natural;
      Seconds : Duration;
   begin
      if Span < 0.0 then
         Put_Cell (Data => "<i>expired</i>", Class => "right");
      else
         Days    := Integer (Span / 86_400 + 0.5) - 1;
         Seconds := Span - Duration (Days * 86_400);
         if Days > 0 then
            Put_Cell
              (Data  => Days'Img &
                        "d " &
                        Ada.Calendar.Formatting.Image (Seconds),
               Class => "right");
         else
            Put_Cell
              (Data  => Ada.Calendar.Formatting.Image (Seconds),
               Class => "right");
         end if;
      end if;
   end Put_Duration_Cell;

   ---------------------
   -- Put_Header_Cell --
   ---------------------

   procedure Put_Header_Cell
     (Data     : String;
      Acronym  : String  := "";
      Params   : Unbounded_String;
      Sortable : Boolean := True)
   is
      Dir : String := "dec";
   begin
      if not Sortable then
         Put_Cell (Data => Data, Acronym => Acronym, Tag => "th");
      elsif Params = "" then
         Put_Cell
           (Data       => Data,
            Acronym    => Acronym,
            Link_Param => "sort",
            Tag        => "th",
            Class      => "sorter");
      elsif Param_Is ("sort", Data) then -- Sorted by this queue, Params exist
         if Standard."=" (CGI.Cookie_Value (Data & "sort"), "dec") then
            Dir := "inc";
         end if;
         --  no value means: sorted incrementally,
         --  so we switch to decremental sorting now.
         --  Of course, the same goes for Value = "inc"
         Put_Cell
           (Data       => Data,
            Acronym    => Acronym,
            Link_Param => To_String (Params & "&dir=" & Dir & "&sort"),
            Tag        => "th",
            Class      => "sorter_active");
      else
         Put_Cell
           (Data       => Data,
            Acronym    => Acronym,
            Link_Param => To_String (Params & "&sort"),
            Tag        => "th",
            Class      => "sorter");
      end if;
   end Put_Header_Cell;

   procedure Put_Navigation_Begin is
   begin
      Begin_Div (ID => "navigation");
      Ada.Text_IO.Put ("<ul>");
   end Put_Navigation_Begin;

   procedure Put_Navigation_End is
   begin
      Ada.Text_IO.Put ("</ul>");
      End_Div (ID => "navigation");
   end Put_Navigation_End;

   procedure Put_Navigation_Link (Data : String; Link_Param : String) is
   begin
      Ada.Text_IO.Put_Line
        ("<li><a href=""" &
         CGI.My_URL &
         "?" &
         Link_Param &
         """>" &
         Data &
         "</a></li>");
   end Put_Navigation_Link;

   -------------------
   -- Put_Paragraph --
   -------------------

   procedure Put_Paragraph (Label : String; Contents : String) is
   begin
      Ada.Text_IO.Put_Line ("<p>" & Label & ": " & Contents & "</p>");
   end Put_Paragraph;

   procedure Put_Paragraph (Label : String; Contents : Calendar.Time) is
   begin
      Put_Paragraph (Label => Label, Contents => To_String (Contents));
   end Put_Paragraph;

   procedure Put_Paragraph (Label : String; Contents : Unbounded_String) is
   begin
      Put_Paragraph (Label, To_String (Contents));
   end Put_Paragraph;

   procedure Put_Paragraph
     (Label    : Unbounded_String;
      Contents : Unbounded_String)
   is
   begin
      Put_Paragraph (To_String (Label), To_String (Contents));
   end Put_Paragraph;

   procedure Comment (Data : String) is
   begin
      Ada.Text_IO.Put_Line ("<!-- " & Data & " -->");
   end Comment;


   ---------
   -- Put --
   ---------

   procedure Put (Data : Tri_State) is
   begin
      Ada.Text_IO.Put ("<img src=""");
      case Data is
         when True =>
            Ada.Text_IO.Put ("/icons/tick.png"" alt=""true"" title=""true""");
         when False =>
            Ada.Text_IO.Put
              ("/icons/cross.png"" alt=""false"" title=""false""");
         when Undecided =>
            Ada.Text_IO.Put
              ("/icons/error.png"" alt=""undefined"" title=""undefined""");
      end case;
      Ada.Text_IO.Put_Line (" />");
   end Put;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        ("<p class=""error""> Error: " & CGI.HTML_Encode (Message) & "</p>");
   end Error;

   procedure Put_Heading (Title : String; Level : Positive) is
   begin
      CGI.Put_HTML_Heading (Title => Title,
                            Level => Level);
   end Put_Heading;

   procedure Put_Stylesheet (URL : String) is
   begin
      Ada.Text_IO.Put_Line
        ("<link rel=""stylesheet"" href=""" &
         URL &
         """ type=""text/css"" media=""screen"" />");
   end Put_Stylesheet;

   procedure Put_Clearer is
   begin
      Ada.Text_IO.Put_Line ("<div class=""clearer""></div>"); -- css quirk
   end Put_Clearer;

   function Param_Is (Param : String; Expected : String) return Boolean is
   begin
      return Standard."=" (CGI.Value (Param), Expected);
   end Param_Is;

   procedure Begin_Div (Class : String := ""; ID : String := "") is
      Tag : Div;
   begin
      Ada.Text_IO.Put ("<div");
      if ID /= "" then
         Ada.Text_IO.Put (" id=""" & ID & """");
      end if;
      if Class /= "" then
         Ada.Text_IO.Put (" class=""" & Class & """");
      end if;
      Ada.Text_IO.Put_Line (">");
      Tag.ID    := To_Unbounded_String (ID);
      Tag.Class := To_Unbounded_String (Class);
      Div_List.Append (Tag);
   end Begin_Div;

   procedure End_Div (Class : String := ""; ID : String := "") is
      Tag : Div := Div_List.Last_Element;
   begin
      if Class /= "" and then Class /= Tag.Class then
         HTML.Error
           ("Found <div class=""" &
            To_String (Tag.Class) &
            """> while trying to close class=""" &
            Class &
            """");
      end if;
      if ID /= "" and then ID /= Tag.ID then
         HTML.Error
           ("Found <div id=""" &
            To_String (Tag.ID) &
            """> while trying to close id=""" &
            ID &
            """");
      end if;
      if not Div_List.Is_Empty then
         Ada.Text_IO.Put_Line ("</div>");
         Div_List.Delete_Last;
      else
         HTML.Error
           ("Trying to close non-existant <div id=""" &
            ID &
            """ class=""" &
            Class &
            """>");
      end if;
   end End_Div;

   procedure Finalize_Divs (Silent : Boolean := False) is
      Tag : Div;
   begin
      if not Silent and then not Div_List.Is_Empty then
         Error ("Found unclosed <div>s");
      end if;
      while not Div_List.Is_Empty loop
         Tag := Div_List.Last_Element;
         Ada.Text_IO.Put_Line
           ("</div><!-- id=""" &
            To_String (Tag.ID) &
            """ class=""" &
            To_String (Tag.Class) &
            """ -->");
         Div_List.Delete_Last;
      end loop;
   end Finalize_Divs;

   function To_String (Time : Calendar.Time) return String is
            Year, This_Year   : Ada.Calendar.Year_Number;
      Month, This_Month : Ada.Calendar.Month_Number;
      Day, This_Day     : Ada.Calendar.Day_Number;
      Secs              : Ada.Calendar.Day_Duration;
      Diff_Days         : Calendar.Arithmetic.Day_Count;
      Diff_Seconds      : Duration;
      Diff_Leap_Seconds : Integer;
      This_Midnight, That_Midnight : Calendar.Time;
   begin
      Calendar.Split
        (Date    => Time,
         Year    => Year,
         Month   => Month,
         Day     => Day,
         Seconds => Secs);
      Calendar.Split
        (Date    => Ada.Calendar.Clock,
         Year    => This_Year,
         Month   => This_Month,
         Day     => This_Day,
         Seconds => Secs);
      That_Midnight := Calendar.Time_Of
            (Year    => Year,
             Month   => Month,
             Day     => Day,
             Seconds => 0.0);
      This_Midnight := Calendar.Time_Of
            (Year    => This_Year,
             Month   => This_Month,
             Day     => This_Day,
             Seconds => 0.0);
      Difference
        (Left => That_Midnight,
         Right => This_Midnight,
         Days => Diff_Days,
         Seconds => Diff_Seconds,
         Leap_Seconds => Diff_Leap_Seconds);

      if Diff_Days = 0 then
         return "today, " & Image (Time, "%H:%M:%S");
      elsif Diff_Days = -1 then
         return "yesterday, " & Image (Time, "%H:%M:%S");
      elsif Diff_Days = 1 then
         return "tomorrow, " & Image (Time, "%H:%M:%S");
      else
         return Image (Time, ISO_Date & " %H:%M:%S");
      end if;
   end To_String;


end HTML;
