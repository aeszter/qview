with HTML;
with Slots; use Slots.Slot_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package body Slots is

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
      return Slots
   is
      R : Slots;
   begin
      R.Min := Min;
      R.Max := Max;
      R.Step := Step;
      return R;
   end New_Range;

   ---------
   -- Put --
   ---------

   procedure Put (S : Slots) is
   begin
      HTML.Put_Paragraph (Label  => "Slots",
                          Contents => To_String (What => S, Short => False));

   end Put;

   --------------
   -- Put_Cell --
   --------------

   procedure Put_Cell (Data : Slot_Lists.List; Class : String) is
      Pos : Slot_Lists.Cursor := Data.First;
      S : Unbounded_String;
   begin
      while Pos /= Slot_Lists.No_Element loop
         S := S & To_String (What => Element (Pos), Short => True);
         Next (Pos);
         if Pos /= Slot_Lists.No_Element then
            S := S & ",";
         end if;
      end loop;
      HTML.Put_Cell (Data => S, Class => Class);
   end Put_Cell;

   ---------------
   -- To_String --
   ---------------

   function To_String (What : Slots; Short : Boolean) return String is
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

   function To_Unbounded_String (What : Slots; Short : Boolean) return Unbounded_String is
   begin
      return To_Unbounded_String (To_String (What => What, Short => Short));
   end To_Unbounded_String;


   ----------
   -- Hash --
   ----------

   function Hash (List : Slot_Lists.List) return String is
      Temp : Ada.Containers.Hash_Type := 0;
      Pos : Slot_Lists.Cursor := List.First;
   begin
      while Pos /= Slot_Lists.No_Element loop
         Temp := Temp xor Hash (Element (Pos));
         Next (Pos);
      end loop;
      HTML.Comment ("Hash (List) => " & Temp'Img);
      return Temp'Img;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (S : Slots) return Hash_Type is
      Str : Unbounded_String := To_Unbounded_String (What => S, Short => False);
   begin
      HTML.Comment (To_String ("Hash (" & Str & ") =>" & Hash (Str)'Img));
      return Hash (Str);
   end Hash;


end Slots;
