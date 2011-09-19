with HTML;
with Ranges; use Ranges.Range_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Ranges is

   ---------------
   -- New_Range --
   --  Purpose: Create a new slot range with given values
   --  Parameter Min: lower bound of slots
   --  Parameter Max: upper bound of slots
   --  Parameter Step: stride length of slots
   --  Returns: the newly created slot range
   ---------------

   function New_Range
     (Min, Step, Max : Natural)
      return Step_Range
   is
      R : Step_Range;
   begin
      R.Min := Min;
      R.Max := Max;
      R.Step := Step;
      return R;
   end New_Range;

   -------------------
   -- To_Step_Range --
   -------------------

   function To_Step_Range (From : String) return Step_Range is
      R : Step_Range;
      Dash, Colon : Natural;
   begin
      Dash := Index (Source  => From,
                     Pattern => "-");
      Colon := Index (Source  => From,
                      Pattern => ":");
      if Dash = 0 and then Colon = 0 then
         R.Min := Natural'Value (From);
         R.Max := R.Min;
         R.Step := 1;
      else
         if Dash = 0 then
            raise Constraint_Error;
         elsif
           Colon = 0 then
            raise Constraint_Error;
         elsif Dash <= From'First then
            raise Constraint_Error;
         elsif Colon <= Dash + 1 then
            raise Constraint_Error;
         elsif From'Last <= Colon then
            raise Constraint_Error;
         end if;
         --  Sanity checks: everything but a pure number or a pattern of the
         --  form [0-9]+-[0-9]+:[0-9]+ is illegal
         R.Min := Natural'Value (From (From'First .. Dash - 1));
         R.Max := Natural'Value (From (Dash + 1 .. Colon - 1));
         R.Step := Natural'Value (From (Colon + 1 .. From'Last));
      end if;
      return R;
   end To_Step_Range;

   ------------------------
   -- To_Step_Range_List --
   ------------------------

   function To_Step_Range_List (From : String) return Step_Range_List is
      List : Step_Range_List;
      Comma, Prev : Natural := From'First - 1;
   begin
      loop
         Comma := Index (Source  => From (Prev + 1 .. From'Last),
                           Pattern => ",");
         if Comma = 0 then
            List.Append (New_Item => To_Step_Range (From (Prev + 1 .. From'Last)));
            return List;
         else
            List.Append (New_Item => To_Step_Range (From (Prev + 1 .. Comma - 1)));
         end if;
         Prev := Comma;
      end loop;
   end To_Step_Range_List;


   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (What : Step_Range) return Boolean is
   begin
      if What.Max < What.Min then
         raise Constraint_Error with What.Min'Img & "-" & What.Max'Img
           & ":" & What.Step'Img;
      end if;
      if What.Step = 0 then
         return True;
      else
         return False;
      end if;

   end Is_Empty;

   function Is_Empty (What : Step_Range_List) return Boolean is
      Cursor : Range_Lists.Cursor := What.First;
   begin
      if Is_Empty (What) then
         return True;
      end if;
      while Cursor /= No_Element loop
         if not Is_Empty (Element (Cursor)) then
            return False;
         end if;
         Next (Cursor);
      end loop;
      return True;
   end Is_Empty;



   -----------
   -- Count --
   -----------

   function Count (What : Step_Range) return Natural is
   begin
      if Is_Empty (What) then
         return 1;
      else
         return (What.Max - What.Min) / What.Step + 1;
      end if;
   end Count;

   function Count (What : Step_Range_List) return Natural is
      N : Natural := 0;
      Cursor : Range_Lists.Cursor := What.First;
   begin
      while Cursor /= Range_Lists.No_Element loop
         N := N + Count (Element (Cursor));
         Next (Cursor);
      end loop;
      return N;
   end Count;

      ------------------
   -- Is_Collapsed --
   ------------------

   function Is_Collapsed (What : Step_Range) return Boolean is
   begin
      if Is_Empty (What) then
         return False;
      elsif What.Min = What.Max then
         return True;
      else
         return False;
      end if;
   end Is_Collapsed;

   function Is_Collapsed (What : Step_Range_List) return Boolean is
   begin
      if What.Length = 1
        and then Is_Collapsed (What.First_Element) then
         return True;
      else
         return False;
      end if;
   end Is_Collapsed;

   ---------
   -- Put --
   ---------

   procedure Put (S : Step_Range) is
   begin
      HTML.Put_Paragraph (Label  => "Slots",
                          Contents => To_String (What => S, Short => True));

   end Put;

   --------------
   -- Put_Cell --
   --------------

   procedure Put_Cell (Data : Range_Lists.List; Class : String) is
      Pos : Range_Lists.Cursor := Data.First;
      S : Unbounded_String;
   begin
      while Pos /= Range_Lists.No_Element loop
         S := S & To_String (What => Element (Pos), Short => True);
         Next (Pos);
         if Pos /= Range_Lists.No_Element then
            S := S & ",";
         end if;
      end loop;
      HTML.Put_Cell (Data => S, Class => Class);
   end Put_Cell;

   ---------------
   -- To_String --
   ---------------

   function To_String (What : Step_Range; Short : Boolean) return String is
   begin
      if Short and then What.Min = What.Max then
         return What.Min'Img;
      elsif What.Step = 1 then
         return What.Min'Img & " -" & What.Max'Img;
      else
         return What.Min'Img & " -" & What.Max'Img
                & " Stride " & What.Step'Img;
      end if;
   end To_String;

   function To_Unbounded_String (What : Step_Range; Short : Boolean) return Unbounded_String is
   begin
      return To_Unbounded_String (To_String (What => What, Short => Short));
   end To_Unbounded_String;


   ----------
   -- Hash --
   ----------

   function Hash (List : Range_Lists.List) return String is
      Temp : Ada.Containers.Hash_Type := 0;
      Pos : Range_Lists.Cursor := List.First;
   begin
      while Pos /= Range_Lists.No_Element loop
         Temp := Temp xor Hash (Element (Pos));
         Next (Pos);
      end loop;
      HTML.Comment ("Hash (List) => " & Temp'Img);
      return Temp'Img;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (S : Step_Range) return Hash_Type is
      Str : Unbounded_String := To_Unbounded_String (What => S, Short => False);
   begin
      HTML.Comment (To_String ("Hash (" & Str & ") =>" & Hash (Str)'Img));
      return Hash (Str);
   end Hash;

   ---------
   -- Min --
   ---------

   function Min (List : Step_Range_List) return Natural is
   begin
      if List.Is_Empty then
         raise Program_Error with "Tried to get Min of empty list";
      else
         return List.First_Element.Min;
      end if;
   end Min;


end Ranges;
