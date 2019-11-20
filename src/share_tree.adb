with HTML;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Slurm.Utils;

package body Share_Tree is

   procedure Put (Item : Slurm.Share_Tree.Lists.Cursor) is
      use Slurm.Share_Tree;

      User : constant User_Node := Lists.Element (Item);
   begin
      Ada.Text_IO.Put ("<tr>");

      HTML.Put_Cell (Data => Slurm.Utils.To_String (User.User_Name), Link_Param => "user");
      HTML.Put_Cell (Data => User.Raw_Shares'Img, Class => "right");
      HTML.Put_Cell (Data => User.Norm_Shares'Img, Class => "right");
      HTML.Put_Cell (Data => User.Raw_Usage'Img, Class => "right");
      HTML.Put_Cell (Data => User.Effective_Usage'Img, Class => "right");
      HTML.Put_Cell (Data => User.Fairshare'Img, Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put;

   procedure Put_All (Sort_Field, Sort_Direction : String) is
   begin
      Slurm.Share_Tree.Load;
      if Sort_Field /= "" then
         Sort_By (Field     => Sort_Field,
                  Direction => Sort_Direction);
      end if;
      HTML.Begin_Div (Class => "share_tree");
      Ada.Text_IO.Put ("<table><tr>");
      HTML.Put_Header_Cell (Data => "User");
      HTML.Put_Header_Cell (Data => "Shares",
                            Colspan => 2);
      HTML.Put_Header_Cell (Data => "Usage",
                            Colspan => 2);
      HTML.Put_Header_Cell (Data => "Fairshare");
      Ada.Text_IO.Put ("</tr><tr>");
      HTML.Put_Header_Cell (""); -- User
      HTML.Put_Header_Cell ("Raw", Sortable => False); -- Shares
      HTML.Put_Header_Cell ("Normalised", Sortable => False); -- Shares
      HTML.Put_Header_Cell ("Raw", Sortable => False); -- Usage
      HTML.Put_Header_Cell ("Effective", Sortable => False); -- Usage
      HTML.Put_Header_Cell (""); -- Fairshare
      Ada.Text_IO.Put ("</tr>");

      Slurm.Share_Tree.Iterate (Put'Access);
      --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "share_tree");
   end Put_All;

   procedure Sort_By (Field : String; Direction : String) is
      use Slurm.Share_Tree;
   begin
      if Field = "User" then
         Sort_By_User;
      elsif Field = "Usage" then
         Sort_By_Usage;
      elsif Field = "Fairshare" then
         Sort_By_Fairshare;
      elsif Field = "Shares" then
         Sort_By_Shares;
      else
         HTML.Error ("Sorting by " & Field & " unimplemented");
      end if;
      if Direction = "dec" then
         Reverse_Order;
      end if;
   end Sort_By;

end Share_Tree;
