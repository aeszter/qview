with HTML;

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
   --  Purpose: Output one slot range as a paragraph
   --  Parameter S: The slot range to print
   ---------

   procedure Put (S : Slots) is
   begin
      if S.Min = S.Max then
         HTML.Put_Paragraph (Label    => "Slots",
                             Contents => S.Min'Img);
      elsif S.Step = 1 then
         HTML.Put_Paragraph (Label    => "Slots",
                             Contents => S.Min'Img & " -" & S.Max'Img);
      else
         HTML.Put_Paragraph (Label    => "Slots",
                             Contents => S.Min'Img & " -" & S.Max'Img
                                         & " Stride " & S.Step'Img);
      end if;

   end Put;

end Slots;
