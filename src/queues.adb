with Parser; use Parser;
with HTML;
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Lightsout;

package body Queues is

   procedure Put_Selected (Selector : not null access function (Q : Queue) return Boolean) is
   begin
      Iterate (Process => Put_For_Maintenance'Access, Selector => Selector);
   end Put_Selected;

   procedure Put_For_Maintenance (Q : Queue) is
      Long_Name : String := Get_Long_Name (Q);
      Separator : Positive := Ada.Strings.Fixed.Index (Source => Long_Name,
                                            Pattern => "@");
      Host_Name : String := Long_Name (Separator + 1 .. Long_Name'Last);
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Long_Name);
      HTML.Put_Cell (Get_Type (Q));
      HTML.Put_Cell (Data => Lightsout.Get_Maintenance (Host_Name));
      HTML.Put_Cell (Data => Lightsout.Get_Bug (Host_Name), Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put_For_Maintenance;

   procedure Append_List (Input_Nodes : Node_List) is
   begin
      HTML.Bug_Ref (Bug_ID => 1831,
                    Info   => "Queues.Append_List called");
      SGE.Queues.Append_List (Input_Nodes);
   end Append_List;

end Queues;
