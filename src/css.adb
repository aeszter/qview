with Ada.Text_IO;
with CGI;

package body CSS is

   procedure Put_Anchor;
   procedure Put_Body;

   Header_Border       : constant String := "2px solid #444";
   Standard_Border     : constant String := "2px solid #a0a0a0;";

   Full_Width          : constant String := "width: 79em;";
   Content_Width       : constant String := "width: 75em;";
   Widest_Box_Width    : constant String := "width: 69em;";
   Small_Box_Width     : constant String := "width: 13em;";

   Standard_Background : constant String := "background: #e0e0e0;";
   Box_Background      : constant String := "background: #e8e8e8;";


   procedure Put_Line (Item : String) is
   begin
      if Block_Is_Open then
         Ada.Text_IO.Put ("  ");
      end if;

      Ada.Text_IO.Put_Line (Item);
   end Put_Line;

   procedure Open_Block (Name : String) is
   begin
      Put_Line ("");
      Put_Line (Name & " {");
      Block_Is_Open := True;
   end Open_Block;

   procedure Close_Block is
   begin
      Block_Is_Open := False;
      Put_Line ("}");
   end Close_Block;

   procedure Put_Anchor is
   begin
      Open_Block ("a.unmarked");
      Put_Line ("color: #000;");
      Put_Line ("text-decoration: none;");
      Close_Block;
   end Put_Anchor;

   procedure Hide_List is
   begin
      Put_Line ("display: inline;");
      Put_Line ("list-style-type: none;");
      Put_Line ("list-style-image: none;");
      Put_Line ("margin: 5px 10px 0 10px;");
      Put_Line ("line-height: 12px;");
   end Hide_List;

   --------------
   -- Put_Body --
   --------------

   procedure Put_Body is
   begin
      Open_Block ("body");
      --/*font-size: 62.5%; */ /* Resets 1em to 10px */
      Put_Line ("font-family: Times, 'Times New Roman', serif;");
      Put_Line ("background: #757677;");
      Put_Line ("color: #222;");
      Put_Line ("text-align: center;");
      Close_Block;
   end Put_Body;

   procedure Put_Page is
   begin
      Open_Block ("#page");
      Put_Line ("padding: 0 0 20px 0;");
      Put_Line (Full_Width);
      Put_Line ("position: relative;");
      Put_Line ("text-align: left;");
      Put_Line ("background: #d5d6d7;");
      Close_Block;
   end Put_Page;

   procedure Put_Header is
   begin
      Open_Block ("#header");
      Put_Line ("position: relative;");
      Put_Line ("float: center;");
      Put_Line ("margin: 0;");
      Put_Line ("padding: 1px 0 0 0;");
      Put_Line ("height: 7.5em;");
      Put_Line (Full_Width);
      Put_Line ("border-bottom: " & Header_Border & ";");
      Close_Block;
   end Put_Header;

   procedure  Put_Footer is
   begin
      Open_Block ("#footer");
      Put_Line ("float: left;");
      Put_Line ("border-top: " & Header_Border & ";");
      Put_Line ("height: 4em;");
      Put_Line (Full_Width);
      Put_Line ("margin-top: 20px;");
      Put_Line ("clear: both;");
      Close_Block;
      Open_Block ("#footer li");
      Put_Line ("font-size: 0.7em;");
      Hide_List;
      Close_Block;

      Open_Block ("#footer a, #footer a:visited");
      Put_Line ("text-decoration: none;");
      Close_Block;
   end Put_Footer;

   procedure Put_Navigation is
   begin
      Open_Block ("#navigation");
      Put_Line ("font-size: 0.75em;");
      Put_Line ("float: center;");
      Put_Line ("position: relative;");
      Put_Line ("border: " & Standard_Border);
      Put_Line ("padding: 4px;");
      Put_Line ("margin: 15px;");
      Put_Line (Standard_Background);
      Close_Block;
      Open_Block ("#navigation ul, #navigation li, #navigation form");
      Hide_List;
      Close_Block;

      Open_Block ("#navigation a, #navigation a:visited");
      Put_Line ("text-decoration: none;");
      Put_Line ("font-weight: bold;");
      Put_Line ("line-height: 1.6em;");
      Put_Line ("color: #555;");
      Close_Block;

      Open_Block ("#navigation a:hover");
      Put_Line ("text-decoration: none;");
      Put_Line ("font-weight: bold;");
      Put_Line ("color: #00f;");
      Close_Block;

      Open_Block ("#job_summary li, #partition_summary li, #sharetree_summary li");
      Hide_List;
      Close_Block;
   end Put_Navigation;


   procedure Put_Content is
   begin
      Open_Block ("#content");
      Put_Line ("font-size: 1.0em;");
      Put_Line ("float: center;");
      Put_Line ("position: relative;");
      Put_Line ("left: 25px;");
      Put_Line ("top: 20px;");
      Put_Line ("margin-bottom: 20px;");
      Put_Line ("padding: 0px;");
      Put_Line (Content_Width);
      Close_Block;
   end Put_Content;

   procedure Put_Cluster_Queues is
   begin
      Open_Block (".cqueues");
      Put_Line ("position: relative;");
      Put_Line ("border: " & Standard_Border);
      Put_Line (Standard_Background);
      Put_Line ("padding: 5px;");
      Put_Line ("width: 30em;");
      Close_Block;
   end Put_Cluster_Queues;

   procedure Put_Job_Actions is
   begin
      Open_Block ("#job_actions, #host_actions, #unlocker");
      Put_Line ("width: 16px;");
      Put_Line (Box_Background);
      Put_Line ("position: relative;");
      Put_Line ("float: left;");
      Put_Line ("margin: 3px;");
      Put_Line ("padding: 3px;");
      Put_Line ("overflow: hidden;");
      Close_Block;

      Open_Block ("#host_actions");
      Put_Line ("width: 80px;");
      Close_Block;

      Open_Block ("#job_actions, #host_actions");
      Put_Line ("display: none;");
      Close_Block;

      Open_Block ("#job_actions:target, #host_actions:target");
      Put_Line ("display: block;");
      Close_Block;

      Open_Block (".action_and_name");
      Put_Line ("display: -webkit-flex;");
      Put_Line ("display: flex;");
      Put_Line ("margin-bottom: 10px;");
      Close_Block;
   end Put_Job_Actions;

   procedure Put_Job_Name is
   begin
      Open_Block (".job_name, .ar_name, .host_name");
      Put_Line ("font-weight: bold;");
      Put_Line ("width: 63em;");
      Put_Line ("position: relative;");
      Put_Line ("float: left;");
      Put_Line (Box_Background);
      Put_Line ("margin: 3px;");
      Put_Line ("overflow: hidden;");
      Close_Block;
   end Put_Job_Name;

   procedure Put_Job_Meta is
   begin
      Open_Block (".job_meta, .ar_meta, .job_usage, .host_properties, .host_jobs");
      Put_Line ("position: relative;");
      Put_Line ("float: left;");
      Put_Line (Box_Background);
      Put_Line ("margin: 3px;");
      Close_Block;
      Open_Block (".job_meta, .ar_meta, .job_usage, .host_properties");
      Put_Line (Small_Box_Width);
      Close_Block;
      Open_Block (".host_jobs");
      Put_Line ("width : 16em;");
      Close_Block;
   end Put_Job_Meta;

   procedure Put_Job_Files is
   begin
      Open_Block (".job_files, .host_res");
      Put_Line ("clear: both;");
      Put_Line ("position: relative;");
      Put_Line ("bottom: 0;");
      Put_Line (Widest_Box_Width);
      Put_Line (Box_Background);
      Put_Line ("margin: 3px;");
      Put_Line ("padding: 3px;");
      Close_Block;
   end Put_Job_Files;

   procedure Put_Job_Queue is
   begin
      Open_Block (".job_queue, .ar_queue, .host_queues");
      Put_Line (Small_Box_Width);
      Put_Line ("position: relative;");
      Put_Line ("float: left;");
      Put_Line ("background: #e8e8e8;");
      Put_Line ("margin: 3px;");
      Close_Block;
   end Put_Job_Queue;

   procedure Put_Job_Resources is
   begin
      Open_Block (".job_resources, .ar_resources, .job_context");
      Put_Line ("position: relative;");
      Put_Line ("width: 17em;");
      Put_Line (Box_Background);
      Put_Line ("margin: 3px;");
      Close_Block;
      Open_Block (".res_and_context");
      Put_Line ("float: left;");
      Close_Block;
   end Put_Job_Resources;

   procedure Put_List is
   begin
      Open_Block (".job_info, .ar_info, .host_info, .job_list, .ar_list, .partitions, .bunches");
      Put_Line ("position: relative;");
      Put_Line ("border: " & Standard_Border);
      Put_Line (Standard_Background);
      Put_Line ("padding: 5px;");
      Put_Line ("margin: 10px 0 10px 0;");
      Close_Block;
   end Put_List;

   procedure Put_Maintenance is
   begin
      Open_Block (".maintenance");
      Put_Line ("border: " & Standard_Border);
      Put_Line (Standard_Background);
      Put_Line ("padding: 5px;");
      Put_Line ("margin: 10px 0 10px 0;");
      Close_Block;
   end Put_Maintenance;


   procedure Put_Table_Padding is
   begin
      Open_Block (".partitions td, .bunches td");
      Put_Line ("padding: 1px 3px 1px 5px;");
      Close_Block;
   end Put_Table_Padding;

   procedure Put_Table_Cells is
      procedure Put_Background (Name : String; Colour : String) is
      begin
         Open_Block (Name);
         Put_Line ("background: " & Colour & ";");
         Close_Block;
      end Put_Background;

   begin
      Put_Background ("tr.available, tr.res_cnfrm", "#0d0");
      Put_Background ("tr.job-error, tr.res_shift", "#d00");
      Put_Background ("tr.slots_available, tr.res_first", "#dd0");
      Put_Background ("tr.res_start", "#33f");
      Put_Background ("tr.res_dupli", "#d0d");
      Put_Background ("tr.program_error", "#d0d");
      Put_Background ("td.right_active", "#0d0");

      Open_Block ("tr.offline, tr.job-held, tr.job-quota");
      Put_Line ("color: #777;");
      Put_Line ("font-style: italic;");
      Close_Block;
      Open_Block ("th");
      Put_Line ("font-size: 0.9em;");
      Close_Block;
      Put_Background ("th.delimited", "#8b8");
      Open_Block ("td.right, td.right_active");
      Put_Line ("text-align: right;");
      Close_Block;
      Open_Block ("td");
      Put_Line ("padding: 0 5px 0 5px;");
      Put_Line ("font-family: Arial, Helvetica, Sans-Serif;");
      Put_Line ("font-size: 0.9em;");
      Close_Block;

      Put_Background (".pct_cold, .load_cold", "#89f");
      Put_Background (".pct_low, .load_normal", "#4f4");
      Put_Background (".pct_med, .load_low", "#ff0");
      Put_Background (".pct_high, .load_high", "#f90");
      Put_Background (".pct_hot, .load_extreme", "#d00");
   end Put_Table_Cells;

   procedure Put_Headings is
   begin
      Open_Block ("h1");
      Put_Line ("text-align: center;");
      Close_Block;
      Open_Block ("h3");
      Put_Line ("margin: 5px 5px 8px 5px;");
      Put_Line ("font-size: 0.9em;");
      Close_Block;
      Open_Block ("h4");
      Put_Line ("font-size: 0.6em;");
      Put_Line ("font-style: italic;");
      Put_Line ("color: #888;");
      Close_Block;
   end Put_Headings;

   procedure Put_Job_Lists is
   begin
      Open_Block (".job_info ul, .ar_info ul");
      Put_Line ("margin-top: 0;");
      Put_Line ("padding-left: 15px;");
      Put_Line ("list-style-type: none;");
      Put_Line ("list-style-image: none;");
      Put_Line ("font-size: 0.85em;");
      Close_Block;
   end Put_Job_Lists;

   procedure Put_Job_Paragraphs is
   begin
      Open_Block (".job_info p, .ar_info p, .host_info p");
      Put_Line ("margin: 5px;");
      Put_Line ("height: 1.2em;");
      Put_Line ("font-size: 0.85em;");
      Put_Line ("padding: 0;");
      Put_Line ("line-height: 1em;");
      Close_Block;

      Open_Block (".job_name p, .ar_name p");
      Put_Line ("margin: 10px;");
--  /*line-height: 10px;*/
      Close_Block;

      Open_Block ("p.message");
      Put_Line ("font-weight: normal;");
      Put_Line ("font-style: italic;");
      Close_Block;
   end Put_Job_Paragraphs;

   procedure Put_Images is
   begin
      Open_Block ("#content img");
      Put_Line ("border: 0;");
      Put_Line ("margin: 0;");
      Put_Line ("padding: 0;");
      Put_Line ("position: relative;");
      Close_Block;

      Open_Block (".job_info img, .job_list img, .partitions img, .host_list img, .bunches img, #job_summary img");
      Put_Line ("vertical-align: bottom;");
      Close_Block;
   end Put_Images;

   procedure Put_Sorters is
   begin
      Open_Block (".sorter a:visited, .sorter a");
      Put_Line ("text-decoration: none;");
      Put_Line ("line-height: 1.6em;");
      Put_Line ("color: #555;");
      Close_Block;
      Open_Block (".sorter a:hover");
      Put_Line ("text-decoration: none;");
      Put_Line ("color: #00f;");
      Close_Block;

      Open_Block (".sorter_active a, .sorter_active a:visited");
      Put_Line ("text-decoration: none;");
      Put_Line ("color: #d11;");
      Close_Block;
   end Put_Sorters;

   procedure Put_Error is
   begin
      Open_Block (".error");
      Put_Line ("color :#e00;");
      Put_Line ("font-weight: bold;");
      Put_Line ("font-style: italic;");
      Close_Block;
   end Put_Error;

   procedure Put_Clearer is
   begin
      Open_Block (".clearer");
      Put_Line ("display: hidden;");
      Put_Line ("clear: both;");
      Close_Block;
   end Put_Clearer;

   ---------
   -- Put --
   ---------

   procedure Put is
   begin
      CGI.Put_CGI_Header;
      Put_Body;
      Put_Page;
      Put_Header;
      Put_Footer;
      Put_Navigation;
      Put_Anchor;
      Put_Content;
      Put_Cluster_Queues;
      Put_Job_Actions;
      Put_Job_Name;
      Put_Job_Meta;
      Put_Job_Files;
      Put_Job_Queue;
      Put_Job_Resources;
      Put_List;
      Put_Maintenance;
      Put_Table_Padding;
      Put_Table_Cells;
      Put_Headings;
      Put_Job_Lists;
      Put_Job_Paragraphs;
      Put_Images;
      Put_Sorters;
      Put_Error;
      Put_Clearer;
   end Put;

end CSS;
