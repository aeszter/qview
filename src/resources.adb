with HTML;
with Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with SGE.Resources; use SGE.Resources.Resource_Lists;
with SGE.Utils; use SGE.Utils; use SGE.Utils.Hash_Strings;

package body Resources is

   -----------------------
   -- Put               --
   --  Purpose: Output one resource as a paragraph
   --  Parameter R: the resource to print
   -----------------------

   procedure Put (Pos : Resource_Lists.Cursor) is
      Label : constant Unbounded_String := Key (Pos);
      Res   : constant Resource := Element (Pos);
      Value : constant Unbounded_String := Res.Value;

   begin
      if Label = "h_rt" then
         HTML.Put_Paragraph (Label    => "<acronym title=""hard runtime limit"">h_rt</acronym>",
                             Contents => Value);
      elsif Res.Boolean_Valued then
         Ada.Text_IO.Put ("<p>" & To_String (Label) & ": ");
         HTML.Put (Res.State);
         Ada.Text_IO.Put ("</p>");
      else
         HTML.Put_Paragraph (Label, Value);
      end if;
   end Put;

   procedure Put_List (L : SGE.Resources.Hashed_List) is
   begin
      L.Iterate (Put'Access);
   end Put_List;

end Resources;
