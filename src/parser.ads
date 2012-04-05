with DOM.Core; with DOM.Core.Documents;
with DOM.Core.Nodes;


package Parser is
   subtype Tree is DOM.Core.Document;
   subtype Attr is DOM.Core.Attr;
   subtype Node is DOM.Core.Node;
   subtype Node_List is DOM.Core.Node_List;

   Parser_Error : exception;

   function Get_Attr (N : Node; Name : String) return Attr;
   function Value (N : Node) return String renames DOM.Core.Nodes.Node_Value;
   function Name (N : Node) return String renames DOM.Core.Nodes.Node_Name;
   function First_Child (N : Node) return Node renames DOM.Core.Nodes.First_Child;
   function Length (List : Node_List) return Natural renames DOM.Core.Nodes.Length;
   function Has_Child_Nodes (N : Node) return Boolean renames DOM.Core.Nodes.Has_Child_Nodes;
   function Child_Nodes (N : Node) return Node_List renames DOM.Core.Nodes.Child_Nodes;
   function Item (List : Node_List; Index : Natural) return Node renames DOM.Core.Nodes.Item;

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

   Resource_Selector : constant String := "-F h_rt,eth,ib,ibs,mem_total,num_proc,cm,q";

private
   sgeroot : constant String := "/cm/shared/apps/sge/current";

end Parser;
