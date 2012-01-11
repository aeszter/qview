with DOM.Core; with DOM.Core.Documents;
with DOM.Core.Nodes;


package Parser is
   subtype Tree is DOM.Core.Document;
   subtype Attr is DOM.Core.Attr;
   subtype Node is DOM.Core.Node;

   function Get_Attr (N : Node; Name : String) return Attr;
   function Value (N : Node) return String renames DOM.Core.Nodes.Node_Value;
   function First_Child (N : Node) return Node renames DOM.Core.Nodes.First_Child;
-- return Get_Named_Item (Attributes (N), Name);

   function Setup (Command  : String := "qstat";
                   Selector : String) return DOM.Core.Document;
   function Get_Elements_By_Tag_Name
     (Doc : DOM.Core.Document; Tag_Name : DOM.Core.DOM_String := "*")
      return DOM.Core.Node_List
      renames DOM.Core.Documents.Get_Elements_By_Tag_Name;

   function Get_Job_Nodes_From_Qstat_J (Doc : DOM.Core.Document)
                                           return DOM.Core.Node_List;
   function Get_Job_Nodes_From_Qstat_U (Doc : DOM.Core.Document)
                                           return DOM.Core.Node_List;

   private
   sgeroot : constant String := "/cm/shared/apps/sge/current";
   Resource_Selector : constant String := "-F h_rt,eth,ib,mem_total,num_proc,cm,q";

end Parser;
