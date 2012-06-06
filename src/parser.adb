with DOM.Readers;
with HTML;
with Sax.Readers;
with DOM.Core; use DOM.Core;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with Pipe_Streams; use Pipe_Streams;
with Ada.Exceptions; use Ada.Exceptions;

package body Parser is

   Reader      : DOM.Readers.Tree_Reader;
   -----------
   -- Setup --
   -----------
   function Setup (Command  : String := "qstat";
                          Selector : String) return Document is
      SGE_Command : Pipe_Stream;
   begin
      SGE_Command.Set_Public_Id (Command);
      HTML.Comment (Command & " " & Selector);
      SGE_Command.Execute (Command => sgeroot & "/bin/lx26-amd64/" & Command,
                           Arguments => Selector & " -xml",
                           Environment => "SGE_ROOT=" & sgeroot);
      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (SGE_Command);
      SGE_Command.Close;
      return Reader.Get_Tree;
   exception
      when Failed_Creation_Error =>
         raise Parser_Error with "Failed to spawn """ & Command
           & """ with """ & Selector & """";
      when Exception_Error =>
         raise Parser_Error with """" & Command
           & """ terminated because of an unhandled exception";
      when Sax.Readers.XML_Fatal_Error =>
         raise Parser_Error;
      when E : others => raise Parser_Error with "Error when calling "
           & Command & " with " & Selector & ": "
           & Exception_Message (E);
   end Setup;

   procedure Free is
   begin
      DOM.Readers.Free (Reader);
   end Free;

   -----------------------------------
   -- Get_Job_Elements_From_Qstat_J --
   -----------------------------------

   function Get_Job_Nodes_From_Qstat_J (Doc : Document)
                                           return Node_List is
      Job_Info, Elements : Node_List;
   begin
      Job_Info := Get_Elements_By_Tag_Name (Doc => Doc, Tag_Name => "djob_info");
      Elements := Child_Nodes (Item (Job_Info, 0));
      --  Elements may contain #text nodes. It is the caller's
      --  responsibility to remove these
      return Elements;
   end Get_Job_Nodes_From_Qstat_J;



   -----------------------------------
   -- Get_Job_Elements_From_Qstat_U --
   -----------------------------------

   function Get_Job_Nodes_From_Qstat_U (Doc : Document)
                                           return Node_List is
   begin
      return Get_Elements_By_Tag_Name (Doc => Doc, Tag_Name => "job_list");
   end Get_Job_Nodes_From_Qstat_U;

   function Get_Attr (N : Node; Name : String) return Attr is
   begin
      return Get_Named_Item (Attributes (N), Name);
   end Get_Attr;
end Parser;
