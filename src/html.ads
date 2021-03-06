with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Calendar;
with CGI; use CGI;
with Slurm.Node_Properties;
with Slurm.Utils; use Slurm.Utils;
with Slurm.Gres;
with Slurm.Tres;

package HTML is
   procedure Begin_Form;
   procedure End_Form;
   procedure Put_Cell (Data       : String;
                       Link_Param : String := "";
                       Acronym    : String := "";
                       Tag        : String := "td";
                       Class      : String := "";
                       Colspan    : Positive := 1);
   --  write out a single table cell, optionally changing the tag (to th)
   --  optionally, link the contents as "Link_Param=Data"
   procedure Put_Cell (Data       : Unbounded_String;
                       Link_Param : String := "";
                       Tag        : String := "td";
                       Class      : String := "");
--     procedure Put_Cell (Data       : SGE.Host_Properties.Host_Name;
--                         Tag        : String := "td";
--                         Class      : String := "");

   procedure Put_Img_Cell (Image : String; Extra_Text : String := "");
   procedure Put_Img (Name, Text, Link : String; Extra_Args : String := "");
   procedure Put_Img_Form (Name, Text, Action, Value : String);

   function Img_Tag (Image : String) return String;

   procedure Put_Hidden_Form (Name, Value : String);

   procedure Put_Time_Cell (Time : Calendar.Time);
   procedure Put_Duration_Cell (Secs : Natural);
   procedure Put_Duration_Cell (Span : Duration);
   procedure Put_Header_Cell (Data     : String;
      Acronym  : String  := "";
      Sortable : Boolean := True;
                              Colspan  : Positive := 1);
   procedure Put_Edit_Box (Name, Default : String);
   procedure Put_Search_Box;
   --  Purpose: put a text input element used to search for a job or user
   --  Parameters: none

   procedure Put_Navigation_Begin;
   procedure Put_Navigation_End;
   procedure Put_Navigation_Link (Data : String; Link_Param : String);
   --  navigation section

   procedure Put_Paragraph (Label : String; Contents : String);
   procedure Put_Paragraph (Label : String; Contents : Calendar.Time);
   procedure Put_Paragraph (Label : String; Contents : Duration);
   procedure Put_Paragraph (Label : String; Contents : Boolean);
   procedure Put_Paragraph (Label : String; Contents : Unbounded_String);
   procedure Put_Paragraph (Label : Unbounded_String; Contents : Unbounded_String);
   procedure Put_Link (Label : String; ID : String; Link_Param : String);
   procedure Put_Link (Text, Link_Param : String);
   procedure Comment (Data : String);
   procedure Comment (Data : Unbounded_String);
   procedure Bug_Ref (Bug_ID : Positive; Info : String);
   procedure Put (Data : Boolean);
--
--     procedure Put_Queue_List (List, Marks : String_Sets.Set);
   procedure Put_List (List : String_Sets.Set);
   procedure Put_List (List : Slurm.Node_Properties.Name_Set);
--     procedure Put_List (List : String_Pairs.Map);
   --  output a string list, set, or map as a <ul>; if List is empty, output a proper icon instead
   procedure Put_List_Head;
   procedure Put_List_Tail;
   procedure Put_List_Entry (Key, Element : String);
   procedure Put_Empty_List;

   procedure Put_Opensearch (URL : String);
   procedure Put_Stylesheet (URL : String);
   procedure Put_Clearer;
   procedure Error (Message : String);
   procedure Put_Heading (Title : String; Level : Positive);

   function Param (Name : String; Index : in Positive := 1;
               Required : in Boolean := False) return String renames CGI.Value;
   function Param_Is (Param : String; Expected : String) return Boolean;
   procedure Begin_Div (Class : String := ""; ID : String := "");
   procedure End_Div (Class : String := ""; ID : String := "");
   procedure Finalize_Divs (Silent : Boolean := False);
   function Encode (S : String) return String renames CGI.HTML_Encode;
   function Acronym (Short, Long : String) return String;
   --  Purpose: Compose an <acronym> tag
   --  Returns: A string of the form <acronym title="long">short</acronym>
   --  for use in Ada.Text_IO.Put or other HTML subprograms

   function Help_Icon (Topic : String) return String;
   --  Purpose: generate a string that contains html tags to display a help
   --  icon and link to an external help page (in the wiki)
   --  Parameter Topic: Help topic to be included in the link
   --  Returns: generated string, for inclusion in Put_Cell, Put_Paragraph
   --  or the like

--     function To_String (Host_Name    : SGE.Host_Properties.Host_Name;
--                         Mark_As_Link : Boolean := True) return String;
   function To_String (Time : Calendar.Time) return String;
   function To_String (Span : Duration) return String;
   function To_String (List : Slurm.Gres.List; Max_Items : Positive := 99) return String;
   function To_String (List : Slurm.Tres.List; Max_Items : Positive := 99) return String;
   function To_Web_String (List : Slurm.Gres.List; Max_Items : Positive := 99) return String;
   function To_Web_String (List : Slurm.Tres.List; Max_Items : Positive := 99) return String;
   function Get_Action_URL (Action, Params : String) return String;
   function Current_URL return String;
private
   type Div is record
      Class : Unbounded_String;
      ID    : Unbounded_String;
   end record;

   package Div_Lists is new Doubly_Linked_Lists (Element_Type => Div);

   Div_List : Div_Lists.List;
   Form_Open : Boolean := False;
end HTML;
