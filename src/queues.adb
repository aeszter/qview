with Parser; use Parser;
with HTML;
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Lightsout;
with CGI;

package body Queues is

   procedure Put_Selected (Selector : not null access function (Q : Queue) return Boolean) is
   begin
      Iterate (Process => Put_For_Maintenance'Access, Selector => Selector);
   end Put_Selected;

   procedure Put_For_Maintenance (Q : Queue) is
      Long_Name : constant String := Get_Long_Name (Q);
      Separator : constant Positive := Ada.Strings.Fixed.Index (Source => Long_Name,
                                            Pattern => "@");
      Host_Name : constant String := Long_Name (Separator + 1 .. Long_Name'Last);
      Cluster_Queue : constant String := Long_Name (Long_Name'First .. Separator - 1);
      Display_Name  : constant String := Cluster_Queue & "@"
                        & "<a href=""" & CGI.My_URL & "?host=" & Host_Name & """>"
                        & Host_Name & "</a>";
   begin
      Ada.Text_IO.Put ("<tr>");
      if Has_Error (Q) then
         HTML.Put_Cell (Display_Name & " <a href="""
                        & HTML.Get_Action_URL (Action => "cq",
                                               Params => "q=" & Long_Name)
                        & """>clear error</a>");
      elsif Has_Disabled (Q) then
         HTML.Put_Cell (Display_Name & " <a href="""
                        & HTML.Get_Action_URL (Action => "eq",
                                               Params => "q=" & Long_Name)
                        & """>enable</a>");
      else
         HTML.Put_Cell (Display_Name);
      end if;
      HTML.Put_Cell (Get_Type (Q));
      HTML.Put_Cell (Data => Lightsout.Get_Maintenance (Host_Name));
      HTML.Put_Cell (Data => Lightsout.Get_Bug (Host_Name), Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put_For_Maintenance;

   procedure Append_List (Input_Nodes : Node_List) is
   begin
      HTML.Bug_Ref (Bug_ID => 1830,
                    Info   => "Queues.Append_List called");
      SGE.Queues.Append_List (Input_Nodes);
   end Append_List;

end Queues;
