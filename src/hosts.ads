with Ada.Containers.Doubly_Linked_Lists;
with DOM.Core; use DOM.Core;
with Queues; use Queues;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils;

package Hosts is

   type Job is record
      Master : Boolean;
      ID     : Positive;
      Slaves : Natural;
   end record;

   function Equal (Left, Right : Job) return Boolean;

   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job,
                                             "="          => Equal);
   subtype Job_List is Job_Lists.List;

   type Fixed is delta 0.01 digits 5 range 0.0 .. 100.0;
   type Percent is range 0 .. 100;


   type Host is record
      Name       : Unbounded_String;
      Jobs       : Job_List;
      Properties : Queue;
      Load       : Fixed;
      Mem_Used   : Gigs;
      Swap_Total : Gigs;
      Swap_Used  : Gigs;
      Queues     : Utils.String_List;
   end record;


   procedure Add_Slave_Process (J : in out Job);

   ------------------
   -- Append_Queue --
   --  Purpose: Store information about one queue associated with this host
   --  Parameter H: The Host object to store the information
   --  Parameter Name: The name of the queue
   --  Parameter State: The state of the queue
   ------------------

   procedure Append_Queue (H : out Host; State : String);

   function New_Job (ID : Positive; Master : Boolean) return Job;
   function New_Job (ID : Positive; PE_Master : String) return Job;


   procedure Append_List (Host_Nodes : Node_List);
   procedure Prune_List (Net, Cores, Memory, Runtime : String);
   procedure Parse_Resource (H : in out Host; N : Node);
   ---------------------
   -- Parse_Hostvalue --
   --  Purpose: Given a Node of an XML DOM tree,
   --  read host attributes
   --  Parameter H : The Host record to update
   --  Parameter V : The Node to read from
   ---------------------
   procedure Parse_Queue (H : in out Host; N : Node);
   ---------------------
   -- Parse_Queue --
   --  Purpose: Given a Node of an XML DOM tree,
   --  read queue attributes
   --  Parameter H : The Host record to update
   --  Parameter V : The Node to read from
   ---------------------
   procedure Parse_Hostvalue (H : in out Host; N : Node);
   procedure Parse_Job (H : in out Host; N : Node);


   package Host_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Host);


   procedure Put (Cursor : Host_Lists.Cursor);
   procedure Put_Jobs (Cursor : Job_Lists.Cursor);
   ----------------
   -- Put_Status --
   --  Purpose: Output one queue status
   --  Parameter Cursor: List Cursor pointing to the queue to output
   ----------------
   procedure Put_Status (Cursor : Utils.String_Lists.Cursor);
   procedure Compactify (List : in out Job_List);
   procedure Update_Used_Slots (H : in out Host);

   function Load_Per_Core (H : Host) return Fixed;
   function Mem_Ratio (H : Host) return Fixed;
   function Mem_Percentage (H : Host) return Percent;
   function Swap_Ratio (H : Host) return Fixed;
   function Swap_Percentage (H : Host) return Percent;
   function Color_Class (P : Percent) return String;
   function Color_Class (Load : Fixed) return String;

   function Precedes_By_Free (Left, Right : Host) return Boolean;
   function Precedes_By_Net (Left, Right : Host) return Boolean;
   function Precedes_By_Cores (Left, Right : Host) return Boolean;
   function Precedes_By_RAM (Left, Right : Host) return Boolean;
   function Precedes_By_Load (Left, Right : Host) return Boolean;
   function Precedes_By_Mem (Left, Right : Host) return Boolean;
   function Precedes_By_Swap (Left, Right : Host) return Boolean;
   function Precedes_By_Name (Left, Right : Host) return Boolean;

   ------------------
   -- Sort_By      --
   --  Purpose:  Sort the host list by any column/field
   --  Parameter Field: Title of the column to sort by
   ------------------
   procedure Sort_By (Field : String; Direction : String);

   package By_Free is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Free);
   package By_Net is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Net);
   package By_Cores is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Cores);
   package By_RAM is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_RAM);
   package By_Load is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Load);
   package By_Mem is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Mem);
   package By_Swap is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Swap);
   package By_Name is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Name);

   Host_List : Host_Lists.List;


end Hosts;
