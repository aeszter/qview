with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Resources; use Resources;
with DOM.Core;

package Host_Properties is
   type Set_Of_Properties is private;
   function Get_Memory (Props :  Set_Of_Properties) return Gigs;
   function Get_Cores (Props : Set_Of_Properties) return Positive;
   function Get_Network (Props : Set_Of_Properties) return Network;
   function Get_Model (Props : Set_Of_Properties) return CPU_Model;
   function Get_Runtime (Props : Set_Of_Properties) return String;
   procedure Set_Memory (Props : in out Set_Of_Properties;
                         S     : String);
   procedure Set_Cores (Props : in out Set_Of_Properties; Cores : Positive);
   procedure Set_Network (Props : in out Set_Of_Properties; Net : Network);
   procedure Set_Model (Props : in out Set_Of_Properties; Model : String);
   procedure Set_Model (Props : in out Set_Of_Properties; Model : CPU_Model);
   procedure Set_Runtime (Props : in out Set_Of_Properties; Runtime : Unbounded_String);

   procedure Init (Props : out Set_Of_Properties;
                   Net, Memory, Cores, Model : String);

   function "<" (Left, Right : Set_Of_Properties) return Boolean;

   procedure Parse_Resource (Props : in out Set_Of_Properties;
                             N     : DOM.Core.Node);
   ---------------------
   -- Parse_Resource --
   --  Purpose: Given a Node of an XML DOM tree,
   --  read host properties
   --  Parameter Props : The Set_Of_Properties to update
   --  Parameter V : The Node to read from
   ---------------------


private
   type Set_Of_Properties is record
      Network               : Resources.Network := none;
      Model                 : Resources.CPU_Model := none;
      Memory                : Resources.Gigs := 0.0;
      Cores                 : Positive := 1;
      Runtime               : Unbounded_String;
   end record;
end Host_Properties;
