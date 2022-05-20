with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Viewer;
with HTML;
with CSS;
with Actions;
with Opensearch;

procedure Qview is
begin
   if HTML.Param_Is ("act", "") then
      Actions.Assert_No_Root;
   end if;
   if HTML.Param_Is ("css", "y") then
      CSS.Put;
   elsif HTML.Param_Is ("opensearch", "y") then
      Opensearch.Put;
   elsif not HTML.Param_Is ("act", "") then
      Actions.Invoke (HTML.Param ("act"));
   else
      Viewer.View;
   end if;
exception
      when E : others =>
      Ada.Text_IO.Put_Line ("Fatal error: " & Exception_Message (E));
end Qview;
