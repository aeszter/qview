with HTML;
with DOM.Core; use DOM.Core;
with SGE.Parser;

package body Parser is
   function Setup (Command  : String := "qstat";
                          Selector : String) return Document is
   begin
      HTML.Comment (Command & " " & Selector);
      return SGE.Parser.Setup (Command  => Command,
                               Selector => Selector);
   end Setup;

   function Setup_No_XML (Command  : String;
                          Selector : String) return SGE.Spread_Sheets.Spread_Sheet is
      Exit_Status : Natural;
      Output : SGE.Spread_Sheets.Spread_Sheet;
   begin
      HTML.Comment (Command & " " & Selector);
      SGE.Parser.Setup_No_XML (Command  => Command,
                               Selector => Selector,
                               Output   => Output,
                               Exit_Status => Exit_Status);
      if Exit_Status /= 0 then
         raise SGE.Parser.Parser_Error with Exit_Status'Img;
      end if;
      return Output;
   end Setup_No_XML;

end Parser;
