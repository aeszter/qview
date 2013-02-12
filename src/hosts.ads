with Ada.Containers.Doubly_Linked_Lists;
with Host_Properties; use Host_Properties;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Resources;
with Parser; use Parser;

package Hosts is

   type Job is private;
   type Host is private;


   procedure Put_All;
   procedure Put_Selected (Selector : not null access function (H : Host) return Boolean);

   procedure Append_List (Host_Nodes : Node_List);
   procedure Prune_List (Requirements : Set_Of_Properties; Queue_Name : String);

   procedure Update_Used_Slots (H : in out Host);

   ------------------
   -- Sort_By      --
   --  Purpose:  Sort the host list by any column/field
   --  Parameter Field: Title of the column to sort by
   ------------------
   procedure Sort_By (Field : String; Direction : String);

private
   type Fixed is delta 0.01 digits 5 range 0.0 .. 100.0;
   type Percent is range 0 .. 100;


   type Job is record
      Master  : Boolean;
      ID      : Positive;
      Task_ID : Natural;
      Slaves  : Natural;
   end record;

   -----------------------
   --
   --  Jobs
   --
   -----------------------

   function Equal (Left, Right : Job) return Boolean;
   procedure Add_Slave_Process (J : in out Job);
   procedure Set_Master (J : in out Job; PE_Master : String);

   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job,
                                             "="          => Equal);
   subtype Job_List is Job_Lists.List;


   -----------------------
   --
   --  Queues
   --
   -----------------------
   package Queue_States is
      new Generic_Bounded_Length (Max => 5);
   package Queue_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String,
                                      Element_Type => Queue_States.Bounded_String,
                                     "=" => Queue_States."=");
   subtype Queue_Map is Queue_Maps.Map;

   ------------------
   -- Append_Queue --
   --  Purpose: Store information about one queue associated with this host
   --  Parameter H: The Host object to store the information
   --  Parameter Name: The name of the queue
   --  Parameter State: The state of the queue
   ------------------
   procedure Append_Queue (H : out Host; Name, State : String);

   ----------------
   -- Put_Queue --
   --  Purpose: Output one queue name and status
   --  Parameter Cursor: Map Cursor pointing to the queue to output
   ----------------
   procedure Put_Queue (Cursor : Queue_Maps.Cursor);


   type Host is record
      Name       : Unbounded_String;
      Jobs       : Job_List;
      Properties : Set_Of_Properties;
      Load       : Fixed;
      Slots_Used : Natural := 0;
      Mem_Used   : Resources.Gigs;
      Swap_Total : Resources.Gigs;
      Swap_Used  : Resources.Gigs;
      Queues     : Queue_Map;
   end record;


   package Host_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Host);

   Host_List : Host_Lists.List;

   procedure Put (Cursor : Host_Lists.Cursor);
   procedure Put_Jobs (Cursor : Job_Lists.Cursor);
   procedure Compactify (List : in out Job_List);

   ------------------------------------
   --
   --  Sorting
   --
   ------------------------------------
   function Precedes_By_Free (Left, Right : Host) return Boolean;
   function Precedes_By_Net (Left, Right : Host) return Boolean;
   function Precedes_By_Cores (Left, Right : Host) return Boolean;
   function Precedes_By_RAM (Left, Right : Host) return Boolean;
   function Precedes_By_Load (Left, Right : Host) return Boolean;
   function Precedes_By_Mem (Left, Right : Host) return Boolean;
   function Precedes_By_Swap (Left, Right : Host) return Boolean;
   function Precedes_By_Name (Left, Right : Host) return Boolean;


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

   -----------------------
   --
   --  Getters
   --
   -----------------------
   function Load_Per_Core (H : Host) return Fixed;
   function Mem_Ratio (H : Host) return Fixed;
   function Mem_Percentage (H : Host) return Percent;
   function Swap_Ratio (H : Host) return Fixed;
   function Swap_Percentage (H : Host) return Percent;
   function Color_Class (P : Percent) return String;
   function Color_Class (Load : Fixed) return String;
   function Get_Free_Slots (H : Host) return Natural;


   -----------------------
   --
   --  Parsing
   --
   -----------------------
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

end Hosts;
