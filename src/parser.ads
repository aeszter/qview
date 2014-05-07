with DOM.Core;
with SGE.Spread_Sheets;
with SGE.Parser;


package Parser is

   Resource_Selector : constant String := "-F h_rt,eth,ib,ibs,ssd,gpu,mem_total,num_proc,cm,q,slots";

   subtype Tree is SGE.Parser.Tree;
   subtype Attr is SGE.Parser.Attr;
   subtype Node is SGE.Parser.Node;
   subtype Node_List is SGE.Parser.Node_List;

   function Setup (Command  : String := "qstat";
                   Selector : String) return DOM.Core.Document;
   function Setup_No_XML (Command  : String;
                          Selector : String) return SGE.Spread_Sheets.Spread_Sheet;

end Parser;
