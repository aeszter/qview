with Ada.Text_IO;             use Ada.Text_IO;
with CGI;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with GNAT.Calendar;
with GNAT.Calendar.Time_IO;   use GNAT.Calendar.Time_IO;
with Ada.Real_Time;
--  with SGE.Queues;
--  with SGE.Utils; use SGE.Utils.String_Lists; use SGE.Utils.String_Sets;
--  use SGE.Utils.String_Pairs;
with Slurm.Hostlists; use Slurm.Hostlists;
with Slurm.Utils;
with Viewer;
with Utils;
with Ada.Strings.Fixed;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Maps;

package body HTML is

   generic
      with package Lists is new Ada.Containers.Ordered_Sets (<>);
      with function Format (What : Lists.Element_Type) return String;
   function List_To_String (Source : Lists.Set; Max_Items : Positive := 99) return String;

   function Format (Item  : Slurm.Gres.Resource) return String;
   function Format (Item  : Slurm.Tres.Resource) return String;
   function Format_For_Web (Item  : Slurm.Gres.Resource) return String;
   function Format_For_Web (Item  : Slurm.Tres.Resource) return String;

   function Acronym (Short, Long : String) return String is
   begin
      return "<acronym title=""" & Long & """>" & Short & "</acronynm>";
   end Acronym;

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

   procedure Begin_Form is
   begin
      if Form_Open then
         Error ("Tried to open nested form");
      else
         Form_Open := True;
         Put_Line ("<form action=""" & CGI.My_URL & "priv"">");
      end if;
   end Begin_Form;

   procedure Bug_Ref (Bug_ID : Positive; Info : String) is
   begin
      Comment ("<a href=""" & CGI.Get_Environment ("BUGZILLA_URL")
               --  http://ram/bugzilla
               & "/show_bug.cgi?id="
               & Bug_ID'Img & """>Bug #" & Bug_ID'Img & "</a>: "
               & Info);
         pragma Compile_Time_Warning (True, "hardcoded config");
   end Bug_Ref;

   function Bug_Ref (Bug_ID : String) return String is
   begin
      return "<a href=""" & CGI.Get_Environment ("BUGZILLA_URL")
               --  http://ram/bugzilla
               & "/show_bug.cgi?id="
               & Bug_ID & """>Bug #" & Bug_ID & "</a>";
   end Bug_Ref;

   procedure Comment (Data : String) is
   begin
      Ada.Text_IO.Put_Line ("<!-- " & Data & " -->");
   end Comment;

   procedure Comment (Data : Unbounded_String) is
   begin
      Comment (To_String (Data));
   end Comment;

   function Current_URL return String is
   begin
      return CGI.My_URL & "?" & CGI.Get_Environment ("QUERY_STRING");
   end Current_URL;

   procedure End_Div (Class : String := ""; ID : String := "") is
      Tag : constant Div := Div_List.Last_Element;
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

   procedure End_Form is
   begin
      if not Form_Open then
         Error ("Trying to close non-open form");
      else
         Put_Line ("</form>");
         Form_Open := False;
      end if;
   end End_Form;

   procedure Error (Message : String) is
      Newline : constant String := "%0D%0A";
   begin
      Ada.Text_IO.Put_Line ("<p class=""error""> Error: "
                            & "<a href=""" & CGI.Get_Environment ("BUGZILLA_URL")
                            --http://ram/bugzilla
                              & "/enter_bug.cgi?"
                      & "component=qview&form_name=enter_bug"
                            & "&product=Projects"
                              & "&version=" & Utils.Version
                      & "&short_desc=" & CGI.HTML_Encode (Message)
                            & "&comment=slurmlib " & Slurm.Utils.Version
                            & Newline
                            & "Please describe what you did before the error occurred. "
                      & "Are there any extraordinary jobs in the queue?"
                      & """>"
                      & CGI.HTML_Encode (Message) & "</a></p>");
         pragma Compile_Time_Warning (True, "hardcoded config");
   end Error;

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

   function Format (Item  : Slurm.Tres.Resource) return String is
   begin
      return To_String (Item.Name) & "="
        & Ada.Strings.Fixed.Trim (Item.Number'Img, Ada.Strings.Left);
   end Format;

   function Format (Item  : Slurm.Gres.Resource) return String is
   begin
      return Ada.Strings.Fixed.Trim (Item.Number'Img, Ada.Strings.Left)
        & " " & To_String (Item.Category)
        & ":" & To_String (Item.Name);
   end Format;

   function Format_For_Web (Item  : Slurm.Tres.Resource) return String is
   begin
      return To_String (Item.Name) & "="
        & Ada.Strings.Fixed.Trim (Item.Number'Img, Ada.Strings.Left);
   end Format_For_Web;

   function Format_For_Web (Item  : Slurm.Gres.Resource) return String is
   begin
      return To_String (Item.Category)
        & ":" & To_String (Item.Name)
        & ":" & Ada.Strings.Fixed.Trim (Item.Number'Img, Ada.Strings.Left);
   end Format_For_Web;

   function Get_Action_URL (Action, Params : String) return String is
   begin
      return CGI.My_URL & "priv?act=" & Action & "&" & Params;
   end Get_Action_URL;

   function Help_Icon (Topic : String) return String is
   begin
      return "<a href=""" & CGI.Get_Environment ("HELP_URL")
--        http://wiki.mpibpc.gwdg.de/grubmueller/index.php/
        & Topic & """>"
        & "<img src=""/icons/help.png"" /></a>";
         pragma Compile_Time_Warning (True, "hardcoded config");
   end Help_Icon;

   function Img_Tag (Image : String) return String is
      Data : constant String :=
         "<img src=""/icons/" &
         Image &
         ".png"" alt=""" &
         Image &
         """ title=""" &
         Image &
         """ />";
   begin
      return Data;
   end Img_Tag;

   function List_To_String (Source : Lists.Set; Max_Items : Positive := 99) return String is
      use Lists;
      Sublist : Lists.Set := Copy (Source);
   begin
      if Length (Source) = 0 then
         return "";
      elsif Length (Source) = 1 then
         return Format (First_Element (Source));
      else
         if Max_Items > 1 then
            Delete_First (Sublist);
            return Format (First_Element (Source)) & ","
              & List_To_String (Sublist, Max_Items - 1);
         else
            return Format (First_Element (Source)) & ",...";
         end if;
      end if;
   end List_To_String;

   function Param_Is (Param : String; Expected : String) return Boolean is
   begin
      return Standard."=" (CGI.Value (Param), Expected);
   end Param_Is;

   procedure Put (Data : Boolean) is
   begin
      Ada.Text_IO.Put ("<img src=""");
      case Data is
         when True =>
            Ada.Text_IO.Put ("/icons/tick.png"" alt=""true"" title=""true""");
         when False =>
            Ada.Text_IO.Put
              ("/icons/cross.png"" alt=""false"" title=""false""");
      end case;
      Ada.Text_IO.Put_Line (" />");
   end Put;

   procedure Put_Cell
     (Data       : String;
      Link_Param : String   := "";
      Acronym    : String   := "";
      Tag        : String   := "td";
      Class      : String   := "";
      Colspan    : Positive := 1)
   is
      Close_Tag : constant String := "</" & Tag & ">";
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

   procedure Put_Clearer is
   begin
      Ada.Text_IO.Put_Line ("<div class=""clearer""></div>"); -- css quirk
   end Put_Clearer;

   procedure Put_Duration_Cell (Secs : Natural) is
      Days, Hours : Natural;
      Dur  : Duration;
   begin
      Days := Secs / 86400;
      Hours := (Secs - Days * 86_400) / 3600;
      Dur  :=
         Ada.Real_Time.To_Duration
           (Ada.Real_Time.Seconds (Secs - Days * 86_400));
      if Days > 0 then
         Put_Cell
           (Data  => Days'Img & "d" & Hours'Img & "h",
            Class => "right");
      else
         Put_Cell
           (Data  => Ada.Calendar.Formatting.Image (Dur),
            Class => "right");
      end if;
   end Put_Duration_Cell;

   procedure Put_Duration_Cell (Span : Duration) is
   begin
      if Span < 0.0 then
         Put_Cell (Data => "<i>expired</i>", Class => "right");
      elsif Span = 0.0 then
         Put_Cell ("");
      else
         Put_Cell (Data => To_String (Span),
                  Class => "right");
      end if;
   end Put_Duration_Cell;

   procedure Put_Edit_Box (Name, Default : String) is
   begin
      Put ("<input type=""text"" name=""" & Name & """ size=""8"" value="""
           & Default & """ onclick=""select()"">");
   end Put_Edit_Box;

   procedure Put_Empty_List is
   begin
      Ada.Text_IO.Put_Line ("<img src=""/icons/cross.png"" alt=""empty"" title=""empty"" />");
   end Put_Empty_List;

   procedure Put_Header_Cell
     (Data     : String;
      Acronym  : String  := "";
      Sortable : Boolean := True;
      Colspan  : Positive := 1)
   is
      Dir : String := "dec";
   begin
      if not Sortable then
         Put_Cell (Data => Data, Acronym => Acronym, Tag => "th", Colspan => Colspan);
      elsif Viewer.Params = "" then
         Put_Cell
           (Data       => Data,
            Acronym    => Acronym,
            Link_Param => "sort",
            Tag        => "th",
            Class      => "sorter",
            Colspan    => Colspan);
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
            Link_Param => Viewer.Params & "&dir=" & Dir & "&sort",
            Tag        => "th",
            Class      => "sorter_active",
            Colspan    => Colspan);
      else
         Put_Cell
           (Data       => Data,
            Acronym    => Acronym,
            Link_Param => Viewer.Params & "&sort",
            Tag        => "th",
            Class      => "sorter",
            Colspan    => Colspan);
      end if;
   end Put_Header_Cell;

   procedure Put_Heading (Title : String; Level : Positive) is
   begin
      CGI.Put_HTML_Heading (Title => Title,
                            Level => Level);
   end Put_Heading;

   procedure Put_Hidden_Form (Name, Value : String) is
   begin
      Put ("<input type=""hidden"" name=""" & Name & """ value="""
           & Value & """ />");
   end Put_Hidden_Form;

   procedure Put_Img (Name, Text, Link : String; Extra_Args : String := "") is
   begin
      Put ("<a href=""" & Link & """");
      if Extra_Args /= "" then
         Put (" " & Extra_Args);
      end if;
      Put ("><img src=""/icons/" & Name & ".png"" " &
            "alt=""" & Text & """ title=""" & Text & """ /></a>");
   end Put_Img;

   procedure Put_Img_Cell (Image : String; Extra_Text : String := "") is
   begin
      Put_Cell (Data => Img_Tag (Image) & Extra_Text);
   end Put_Img_Cell;

   procedure Put_Img_Form (Name, Text, Action : String) is
   begin
      Put ("<input type=""image"" name=""" & Action
           & """ src=""/icons/" & Name & ".png"" alt=""" & Text
           & """ title=""" & Text & """ />");
   end Put_Img_Form;

   procedure Put_Link (Label : String; ID : String; Link_Param : String) is
   begin
      Put_Paragraph (Label    => Label,
                     Contents => "<a href=""" & CGI.My_URL &
                                 "?" & Link_Param & "=" & ID & """>" & ID & "</a>");
   end Put_Link;

   procedure Put_Link (Text, Link_Param : String) is
   begin
      Ada.Text_IO.Put_Line ("<a href=""" & CGI.My_URL &
                            "?" & Link_Param & "=" & Text & """>" & Text & "</a>");
   end Put_Link;

   procedure Put_List (List : Slurm.Hostlists.Hostlist) is
      use Slurm.Hostlists.Name_Sets;

      Elem : Name_Sets.Cursor;
   begin
      Elem := List.First;
      Put_List_Head;
      if Elem = Name_Sets.No_Element then
         Put_Empty_List;
      else
         while Elem /= Name_Sets.No_Element loop
            declare
               Name : constant String := To_String (Name_Sets.Element (Elem));
            begin
               Ada.Text_IO.Put_Line ("<li<a href=""" & CGI.My_URL &
                                       "?node=""" & Name & """>" & Name & "</a></li>");
               Next (Elem);
            end;
         end loop;
      end if;
      Put_List_Tail;
   end Put_List;

   procedure Put_List (List : String_Sets.Set) is
      use String_Sets;

      Elem : String_Sets.Cursor;
   begin
      Elem := List.First;
      Put_List_Head;
      if Elem = String_Sets.No_Element then
         Put_Empty_List;
      else
         while Elem /= String_Sets.No_Element loop
            Ada.Text_IO.Put_Line ("<li>" & To_String (String_Sets.Element (Elem)) & "</li>");
            Next (Elem);
         end loop;
      end if;
      Put_List_Tail;
   end Put_List;

   procedure Put_List_Entry (Key, Element : String) is
   begin
      Ada.Text_IO.Put_Line ("<li><b>" & Key & ":</b> " & Element & "</li>");
   end Put_List_Entry;

   procedure Put_List_Head is
   begin
      Ada.Text_IO.Put ("<ul>");
   end Put_List_Head;

   procedure Put_List_Tail is
   begin
      Ada.Text_IO.Put ("</ul>");
   end Put_List_Tail;

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

   procedure Put_Opensearch (URL : String) is
   begin
      Ada.Text_IO.Put_Line
        ("<link rel=""search"" href=""" &
           URL &
           """ type=""application/opensearchdescription+xml"" " &
           " title=""qview search"" />");
   end Put_Opensearch;

   procedure Put_Paragraph (Label : String; Contents : String; Class : String := "") is
      Full_Label : constant String := (if Label = "" then "" else Label & ": ");
   begin
      if Class = "" then
         Ada.Text_IO.Put_Line ("<p>" & Full_Label & Contents & "</p>");
      else
         Ada.Text_IO.Put_Line ("<p class=""" & Class & """>" & Full_Label & Contents & "</p>");
      end if;
   end Put_Paragraph;

   procedure Put_Paragraph (Label : String; Contents : Calendar.Time; Class : String := "") is
   begin
      Put_Paragraph (Label => Label, Contents => To_String (Contents), Class => Class);
   end Put_Paragraph;

   procedure Put_Paragraph (Label : String; Contents : Duration; Class : String := "") is
   begin
      Put_Paragraph (Label    => Label,
                     Contents => To_String (Contents), Class => Class);
   end Put_Paragraph;

   procedure Put_Paragraph (Label : String; Contents : Unbounded_String; Class : String := "") is
   begin
      Put_Paragraph (Label, To_String (Contents), Class => Class);
   end Put_Paragraph;

   procedure Put_Paragraph (Label : String; Contents : Slurm.Hostlists.Node_Name;
                            Class : String := "") is
      Node_Name : constant String := To_String (Contents);
   begin
      Put_Paragraph (Label, "<a href=""" & CGI.My_URL & "?node="
                     & Node_Name & """>" & Node_Name & "</a>", Class);
   end Put_Paragraph;

   --------------
   -- Put_Cell --
   --  Purpose: Write a table cell with given contents.
   --  Input: Data: Contents of the cell;
   --  Input: Tag: (optional) tag to use instead of <td> (e.g. <th>)
   --  Input: Link_Param: (optional) CGI Parameter to use in link, sc.
   --        <a href="the_url?Link_Param=Data">Data</a>
   --------------

--     procedure Put_Cell (Data  : SGE.Host_Properties.Host_Name;
--                         Tag   : String := "td";
--                         Class : String := "") is
--        Close_Tag : constant String := "</" & Tag & ">";
--     begin
--        --  Start open tag
--        Put ("<" & Tag);
--
--        if Class /= "" then
--           Put (" class=""" & Class & """");
--        end if;
--        Put (">");
--        --  Open tag ended
--
--        Put (To_String (Data));
--
--        Put_Line (Close_Tag);
--     end Put_Cell;

   procedure Put_Paragraph
     (Label    : Unbounded_String;
      Contents : Unbounded_String;
      Class    : String := "")
   is
   begin
      Put_Paragraph (To_String (Label), To_String (Contents), Class);
   end Put_Paragraph;

   procedure Put_Paragraph (Label : String; Contents : Boolean; Class : String := "") is
   begin
      if Class = "" then
         Ada.Text_IO.Put ("<p>" & Label);
      else
         Ada.Text_IO.Put ("<p class=""" & Class & """>" & Label);
      end if;
      Put (Contents);
      Ada.Text_IO.Put_Line ("</p>");
   end Put_Paragraph;

--     procedure Put_Queue_List (List, Marks : String_Sets.Set) is
--        Elem : String_Sets.Cursor;
--     begin
--        Elem := List.First;
--        Put_List_Head;
--        if Elem = String_Sets.No_Element then
--           Put_Empty_List;
--        else
--           while Elem /= String_Sets.No_Element loop
--              declare
--                 Queue : Unbounded_String;
--                 Host  : SGE.Host_Properties.Host_Name;
--                 Marked : constant Boolean := Marks.Contains (Element (Elem));
--              begin
--                 Ada.Text_IO.Put ("<li>");
--                 if Marked then
--                    Ada.Text_IO.Put ("<em>");
--                 end if;
--                 SGE.Queues.Decompose_Long_Name (Long_Name => To_String (String_Sets.Element (Elem)),
--                                                 Queue     => Queue,
--                                                 Host      => Host);
--                 Ada.Text_IO.Put_Line (To_String (Queue) & "@" & To_String (Host));
--                 if Marked then
--                    Ada.Text_IO.Put ("</em>");
--                 end if;
--                 Ada.Text_IO.Put ("</li>");
--                 Next (Elem);
--              end;
--           end loop;
--        end if;
--        Put_List_Tail;
--     end Put_Queue_List;
--
--     procedure Put_List (List : String_Lists.List) is
--        Elem : String_Lists.Cursor;
--     begin
--        Elem := List.First;
--        Put_List_Head;
--        if Elem = String_Lists.No_Element then
--           Put_Empty_List;
--        else
--           while Elem /= String_Lists.No_Element loop
--              Ada.Text_IO.Put_Line ("<li>" & To_String (String_Lists.Element (Elem)) & "</li>");
--              Next (Elem);
--           end loop;
--        end if;
--        Put_List_Tail;
--     end Put_List;
--
--     procedure Put_List (List : String_Pairs.Map) is
--        Elem : String_Pairs.Cursor;
--     begin
--        Elem := List.First;
--        Put_List_Head;
--        if Elem = String_Pairs.No_Element then
--           Put_Empty_List;
--        else
--           while Elem /= String_Pairs.No_Element loop
--              Put_List_Entry (Key => To_String (String_Pairs.Key (Elem)),
--                              Element => To_String (String_Pairs.Element (Elem)));
--              Next (Elem);
--           end loop;
--        end if;
--        Put_List_Tail;
--     end Put_List;
--

   procedure Put_Search_Box is
   begin
      Put ("<li><form>");
      Put_Edit_Box ("search", "search");
      Put ("</form></li>");
   end Put_Search_Box;

   procedure Put_Stylesheet (URL : String) is
   begin
      Ada.Text_IO.Put_Line
        ("<link rel=""stylesheet"" href=""" &
         URL &
         """ type=""text/css"" media=""screen"" />");
   end Put_Stylesheet;

   procedure Put_Time_Cell (Time : Calendar.Time) is
   begin
      Put_Cell (To_String (Time));
   end Put_Time_Cell;

   function Strip_Parameter (Source, Key : String) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      Buffer : Unbounded_String;
      I      : Natural;
      E      : Natural := Source'First;
   begin
      I := Index (Source  => Source,
                  Pattern => Key);
      if I = 0 then
         return Source;
      end if;
      while I > 0 loop
         if Source (I - 1) = '&' then
            I := I - 1;
         end if;
         Append (Buffer, Source (E ..  I - 1));
         E := Index (Source => Source (I + 1 .. Source'Last),
                     Set    => To_Set ('&'));
         if E = 0 then
            E := Source'Last;
         end if;
         I := Index (Source  => Source (E + 1 .. Source'Last),
                     Pattern => Key);
      end loop;
      Append (Buffer, Source (E + 1 .. Source'Last));
      return To_String (Buffer);
   end Strip_Parameter;

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
      if Year = 1970 and then Month = 1 and then Day = 1 then
         return "<i>unknown</i>";
      end if;
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

   function To_String (Span : Duration) return String is
      Days    : Natural;
      Seconds : Duration;
   begin
      Days    := Integer (Span / 86_400 + 0.5) - 1;
      Seconds := Span - Duration (Days * 86_400);
      if Days > 0 then
         return Days'Img & "d " & Ada.Calendar.Formatting.Image (Seconds);
      else
         return Ada.Calendar.Formatting.Image (Seconds);
      end if;
   end To_String;

   function Gres_To_String is
     new List_To_String (Lists => Slurm.Gres.Lists,
                         Format => Format);
   function Tres_To_String  is
     new List_To_String (Lists => Slurm.Tres.Lists,
                         Format => Format);

   function Gres_To_Web_String is
     new List_To_String (Lists => Slurm.Gres.Lists,
                         Format => Format_For_Web);
   function Tres_To_Web_String  is
     new List_To_String (Lists => Slurm.Tres.Lists,
                         Format => Format_For_Web);

   function To_String (List : Slurm.Tres.List; Max_Items : Positive := 99) return String
                       renames Tres_To_String;
   function To_String (List : Slurm.Gres.List; Max_Items : Positive := 99) return String
                       renames Gres_To_String;

   function To_Web_String (List : Slurm.Tres.List; Max_Items : Positive := 99) return String
                       renames Tres_To_Web_String;
   function To_Web_String (List : Slurm.Gres.List; Max_Items : Positive := 99) return String
                       renames Gres_To_Web_String;

--     function To_String (Host_Name    : SGE.Host_Properties.Host_Name;
--                         Mark_As_Link : Boolean := True) return String is
--        use SGE.Host_Properties;
--     begin
--        return "<a href=""" & CGI.My_URL & "?host=" & Value (Host_Name)
--          & """" & (if Mark_As_Link then "" else "class=""unmarked""")
--          & ">" & Value (Host_Name) & "</a>";
--     end To_String;

end HTML;
