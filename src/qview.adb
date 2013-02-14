with Viewer;
with HTML;
with CSS;

procedure Qview is
begin
   if HTML.Param_Is ("css", "y") then
      CSS.Put;
   else
      Viewer.View;
   end if;
end Qview;
