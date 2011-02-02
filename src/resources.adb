with HTML;
with Ada.Calendar; use Ada.Calendar;
use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Real_Time;
with Resources; use Resources.Resource_Lists;
with Ada.Containers; use Ada.Containers;

package body Resources is

   ------------------
   -- New_Resource --
   --  Purpose: Create a new resource with given name and value
   --  Parameter Name: name of the new resource
   --  Parameter Value: value of the new resource
   --  Returns: the newly created resource
   ------------------

   function New_Resource (Name : Unbounded_String; Value : Unbounded_String)
                          return Resource is
      R : Resource;
   begin
      R.Name := Name;
      R.Value := Value;
      return R;
   end New_Resource;

   ------------------
   -- New_Resource --
   --  Purpose: Create a new resource with given name and value
   --  Parameter Name: name of the new resource
   --  Parameter Value: value of the new resource
   --  Returns: the newly created resource
   ------------------

   function New_Resource (Name : String; Value : String)
                          return Resource is
   begin
      return New_Resource (Name  => To_Unbounded_String (Name),
                           Value => To_Unbounded_String (Value));
   end New_Resource;

   -----------------------
   -- Put               --
   --  Purpose: Output one resource as a paragraph
   --  Parameter R: the resource to print
   -----------------------

   procedure Put (R : Resource) is
      Label : Unbounded_String := R.Name;
      Value : Unbounded_String := R.Value;
      Days  : Natural;
      Secs  : Natural;
      Dur   : Duration;
   begin
      if Label = "h_rt" then
         Secs := Integer'Value (To_String (Value));
         Days := Secs / 86400;
         Dur := Ada.Real_Time.To_Duration (Ada.Real_Time.Seconds (Secs - Days*86400));
         if Days > 0 then
            Value := To_Unbounded_String (Days'Img & "d " & Image (Dur));
         else
            Value := To_Unbounded_String (Image (Dur));
         end if;
         Label := To_Unbounded_String ("<acronym title=""hard runtime limit"">h_rt</acronym>");
      end if;
      HTML.Put_Paragraph (Label, Value);
   end Put;

   function To_Unbounded_String (L : Resource_Lists.List) return Unbounded_String is
      S : Unbounded_String := Null_Unbounded_String;
      Cursor : Resource_Lists.Cursor;
   begin
      if L.Is_Empty then
         return Null_Unbounded_String;
      end if;
      Cursor := L.First;
      loop
         S := S & Element (Cursor).Name & ": " & Element (Cursor).Value;
         exit when Cursor = L.Last;
         Cursor := Next (Cursor);
         S := S & "; ";
      end loop;
      return S;
   end To_Unbounded_String;


   ----------
   -- Sort --
   ----------

   procedure Sort (L : in out Resource_Lists.List) is
   begin
      Sorting.Sort (L);
   end Sort;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Resource_Lists.List) return Boolean is
   begin
      return Left = Right;
   end Equal;

   --------------
   -- Precedes --
   --------------

   function Precedes (Left, Right : Resource) return Boolean is
   begin
      return Left.Name < Right.Name;
   end Precedes;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Resource) return Boolean is
   begin
      if Precedes (Left, Right) then
         return True;
      elsif Precedes (Right, Left) then
         return False;
      end if;
      return Left.Value < Right.Value;
   end "<";

   --------------
   -- Precedes --
   --------------

   function Precedes (Left, Right : Resource_Lists.List) return Boolean is
      L_Cursor, R_Cursor : Resource_Lists.Cursor;
   begin
      if Left.Length < Right.Length then
         return True;
      elsif Left.Length > Right.Length then
         return False;
      elsif Left.Is_Empty then
         return False;
      end if;
      L_Cursor := Left.First;
      R_Cursor := Right.First;
      loop
         if Element (L_Cursor) < Element (R_Cursor) then
            return True;
         elsif Element (R_Cursor) < Element (L_Cursor) then
            return False;
         end if;
         exit when L_Cursor = Left.Last or R_Cursor = Right.Last;
         L_Cursor := Next (L_Cursor);
         R_Cursor := Next (R_Cursor);
      end loop;
      return False;
   end Precedes;

end Resources;
