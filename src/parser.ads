with DOM.Core;
with SGE.Spread_Sheets;
with SGE.Parser;
with SGE.Taint; use SGE.Taint;


package Parser is

   Resource_Selector : constant Trusted_String := Implicit_Trust ("-F h_rt,eth,ib,ibs,ssd,gpu,mem_total,num_proc,cm,gm,q,slots,gpu_mem");

   subtype Tree is SGE.Parser.Tree;
   subtype Attr is SGE.Parser.Attr;
   subtype Node is SGE.Parser.Node;
   subtype Node_List is SGE.Parser.Node_List;

   function Setup (Command  : Trusted_Command_Name := Cmd_Qstat;
                   Selector : Trusted_String) return DOM.Core.Document;
   function Setup_No_XML (Command  : Trusted_Command_Name;
                          Selector : Trusted_String) return SGE.Spread_Sheets.Spread_Sheet;

end Parser;
