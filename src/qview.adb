with Viewer;
with HTML;
with CSS;
with Actions;
with Opensearch;

procedure Qview is
begin
   if HTML.Param_Is ("css", "y") then
      CSS.Put;
   elsif HTML.Param_Is ("opensearch", "y") then
      Opensearch.Put;
   elsif not HTML.Param_Is ("act", "") then
      Actions.Invoke (HTML.Param ("act"));
   else
      Viewer.View;
   end if;
end Qview;
