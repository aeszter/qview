with Parser; use Parser;
with HTML;
with Ada.Text_IO;
with Lightsout;
with SGE.Host_Properties;

package body Queues is

   procedure Put_Selected (Selector : not null access function (Q : Queue) return Boolean) is
   begin
      Iterate (Process => Put_For_Maintenance'Access, Selector => Selector);
   end Put_Selected;

   procedure Put_For_Maintenance (Q : Queue) is
      use SGE.Host_Properties;
      Host          : constant Host_Name := Get_Host_Name (Q);
      Cluster_Queue : constant String := Get_Name (Q);
      Display_Name  : constant String := Cluster_Queue & "@"
                        & HTML.To_String (Host);
      Link_Name     : constant String := Cluster_Queue & "@" & Value (Host);
   begin
      Ada.Text_IO.Put ("<tr>");
      if Has_Error (Q) then
         HTML.Put_Cell (Display_Name & " <a href="""
                        & HTML.Get_Action_URL (Action => "cq",
                                               Params => "q=" & Link_Name)
                        & """>clear error</a>");
      elsif Has_Disabled (Q) then
         HTML.Put_Cell (Display_Name & " <a href="""
                        & HTML.Get_Action_URL (Action => "eq",
                                               Params => "q=" & Link_Name)
                        & """>enable</a>");
      else
         HTML.Put_Cell (Display_Name);
      end if;
      HTML.Put_Cell (Get_Type (Q));
      HTML.Put_Cell (Data => Lightsout.Get_Maintenance (Host));
      HTML.Put_Cell (Data => Lightsout.Get_Bug (Host), Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put_For_Maintenance;

   procedure Append_List (Input_Nodes : Node_List) is
   begin
      HTML.Bug_Ref (Bug_ID => 1830,
                    Info   => "Queues.Append_List called");
      SGE.Queues.Append_List (Input_Nodes);
   end Append_List;

end Queues;
