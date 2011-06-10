with DOM.Core; with DOM.Core.Documents;


package Parser is
   subtype Tree is DOM.Core.Document;

   sgeroot : constant String := "/cm/shared/apps/sge/current";
   Resource_Selector : constant String := "-F h_rt,eth,ib,mem_total,num_proc,cm,q";


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

end Parser;
