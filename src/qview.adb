with Viewer;
with HTML;
with CSS;
with Actions;

procedure Qview is
begin
   if HTML.Param_Is ("css", "y") then
      CSS.Put;
   elsif not HTML.Param_Is ("act", "") then
      Actions.Invoke (HTML.Param ("act"));
   else
      Viewer.View;
   end if;
end Qview;
