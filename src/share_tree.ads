with SGE.Spread_Sheets; use SGE.Spread_Sheets;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Bounded;
with SGE.Utils; use SGE.Utils;

package Share_Tree is
   procedure Put_List;
   procedure Put_Summary;
   procedure Append_List (Cells : in out Spread_Sheet);
   procedure Sort_By (Field : String; Direction : String);

   type User_Node is private;


private
   package User_Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 8);
   subtype User_Name_String is User_Name_Strings.Bounded_String;

   type User_Node is record
      User_Name : User_Name_String;
      Usage     : Usage_Number;
      CPU       : Usage_Number;
      LT_CPU    : Usage_Number;
      Job_Count : Usage_Integer;
      Mem       : Usage_Number;
      IO        : Usage_Number;
   end record;

   package Share_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => User_Node);

   List : Share_Lists.List;
   Total_Usage : Usage_Number;
   Total_CPU, Total_Mem, Total_IO : Usage_Number;

   procedure Put (Item : Share_Lists.Cursor);

   function Scale_CPU (Raw_Value : Usage_Number) return String;
   function Scale_Usage (Raw_Value : Usage_Number) return String;
   function Scale_Memory (Raw_Value : Usage_Number) return String;
   function Scale_IO (Raw_Value : Usage_Number) return String;

   function Precedes_By_User (Left, Right : User_Node) return Boolean;
   function Precedes_By_Usage (Left, Right : User_Node) return Boolean;
   function Precedes_By_CPU (Left, Right : User_Node) return Boolean;
   function Precedes_By_LT_CPU (Left, Right : User_Node) return Boolean;
   function Precedes_By_Memory (Left, Right : User_Node) return Boolean;
   function Precedes_By_IO (Left, Right : User_Node) return Boolean;
   function Precedes_By_Job_Count (Left, Right : User_Node) return Boolean;

   package Sorting_By_User is new Share_Lists.Generic_Sorting ("<" => Precedes_By_User);
   package Sorting_By_Usage is new Share_Lists.Generic_Sorting ("<" => Precedes_By_Usage);
   package Sorting_By_CPU is new Share_Lists.Generic_Sorting ("<" => Precedes_By_CPU);
   package Sorting_By_LT_CPU is new Share_Lists.Generic_Sorting ("<" => Precedes_By_LT_CPU);
   package Sorting_By_Memory is new Share_Lists.Generic_Sorting ("<" => Precedes_By_Memory);
   package Sorting_By_IO is new Share_Lists.Generic_Sorting ("<" => Precedes_By_IO);
   package Sorting_By_Job_Count is new Share_Lists.Generic_Sorting ("<" => Precedes_By_Job_Count);

end Share_Tree;
