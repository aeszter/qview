with HTML;
with Ada.Calendar; use Ada.Calendar;
use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Real_Time;

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


end Resources;
