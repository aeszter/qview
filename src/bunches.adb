with Ada.Text_IO;
with Slurm.Bunches; use Slurm.Bunches;
with CGI;
with HTML;

package body Bunches is

   procedure Put_Error (Message : String);

   procedure Put (B : Slurm.Bunches.Bunch) is
   begin
      if B.Has_Errors then
         Ada.Text_IO.Put ("<tr class=""program_error"">");
         B.Iterate_Errors (Put_Error'Access);
      elsif not Has_Waiting (B) then
         Ada.Text_IO.Put ("<tr class=""job-held"">");
      else
         Ada.Text_IO.Put ("<tr>");
      end if;

      HTML.Put_Cell (Data => "<a href=""" & CGI.My_URL & "?jobs=bunch"
               & "&cpus=" & Get_CPUs (B)'Img
               & "&gres=" & Get_Gres (B)
               & "&tres=" & Get_TRES (B)
               & """><img src=""/icons/arrow_right.png"" /></a>");
      HTML.Put_Cell (Data => Get_CPUs (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Gres (B));
      HTML.Put_Cell (Data => Get_TRES (B));
      HTML.Put_Cell (Data => Get_Total_Jobs (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Waiting_Jobs (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Depending_Jobs (B)'Img, Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put;

   procedure Put_All is
   begin
      Put_List (Slurm.Bunches.Load);
   end Put_All;

   procedure Put_Error (Message : String) is
   begin
      HTML.Comment (Message);
   end Put_Error;

   procedure Put_List (Source : Slurm.Bunches.List) is
   begin
      Ada.Text_IO.Put_Line ("<table><tr>");
      HTML.Put_Cell (Data => "<acronym title=""click on arrow to view job list"">"
                     & "Detail</acronym>", Tag => "th");
      HTML.Put_Cell (Data => "Slots", Tag => "th");
      HTML.Put_Cell (Data => "Gres", Tag => "th");
      HTML.Put_Cell (Data => "TRES", Tag => "th");
      HTML.Put_Cell (Data => "Total", Tag => "th");
      HTML.Put_Cell (Data => "Waiting", Tag => "th");
      HTML.Put_Cell (Data => "Dependency", Tag => "th");
      Ada.Text_IO.Put_Line ("</tr>");
      Iterate (Source, Put'Access);
      Ada.Text_IO.Put_Line ("</table>");
   end Put_List;

end Bunches;
