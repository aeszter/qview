with Ada.Text_IO; use Ada.Text_IO;
with CGI;

package body Opensearch is
   Short_Name  : constant String := "qview";
   Description : constant String := "Search for jobs, nodes, or users on owl";
   Contact     : constant String := "aeszter@mpibpc.mpg.de";

   procedure Put is
   begin
      CGI.Put_CGI_Header ("Content-type: application/opensearchdescription+xml");
      Put_Line ("<?xml version=""1.0"" encoding=""UTF-8""?>");
      Put_Line ("<OpenSearchDescription xmlns=""http://a9.com/-/spec/opensearch/1.1/"">");
      Put_Line ("<ShortName>" & Short_Name & "</ShortName>");
      Put_Line ("<Description>" & Description & "</Description>");
      Put_Line ("<Contact>" & Contact & "</Contact>");
      Put_Line ("<Url type=""text/html"" ");
      Put_Line ("     template=""http://owl-monitor.mpibpc.intern/cgi-bin/qview?search={searchTerms}""/>");
      Put_Line ("<Image height=""16"" width=""16"" type=""image/x-icon"">http://owl-monitor.mpibpc.intern/favicon.ico</Image>");
      Put_Line ("</OpenSearchDescription>");
   end Put;
end Opensearch;
