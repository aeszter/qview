with DOM.Core; use DOM.Core;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with HTML;
with Ada.Exceptions; use Ada.Exceptions;
with Utils; use Utils;

package body Host_Properties is

   -----------------
   -- Get_Runtime --
   -----------------

   function Get_Runtime (Props : Set_Of_Properties) return String is
   begin
      return To_String (Props.Runtime);
   end Get_Runtime;

   ----------------
   -- Get_Memory --
   ----------------

   function Get_Memory (Props :  Set_Of_Properties) return Gigs is
   begin
      return Props.Memory;
   end Get_Memory;

   ---------------
   -- Get_Cores --
   ---------------

   function Get_Cores (Props : Set_Of_Properties) return Positive is
   begin
      return Props.Cores;
   end Get_Cores;

   -----------------
   -- Get_Network --
   -----------------

   function Get_Network (Props : Set_Of_Properties) return Network is
   begin
      return Props.Network;
   end Get_Network;

   function Get_Model (Props : Set_Of_Properties) return CPU_Model is
   begin
      return Props.Model;
   end Get_Model;

   procedure Set_Cores (Props : in out Set_Of_Properties; Cores : Positive) is
   begin
      Props.Cores := Cores;
   end Set_Cores;

   procedure Set_Network (Props : in out Set_Of_Properties; Net : Network) is
   begin
      Props.Network := Net;
   end Set_Network;

   procedure Set_Model (Props : in out Set_Of_Properties; Model : String) is
   begin
      Props.Model := CPU_Model'Value (Model);
   end Set_Model;

   procedure Set_Model (Props : in out Set_Of_Properties; Model : CPU_Model) is
   begin
      Props.Model := Model;
   end Set_Model;

   procedure Set_Runtime (Props : in out Set_Of_Properties; Runtime : Unbounded_String) is
   begin
      Props.Runtime := Runtime;
   end Set_Runtime;


   procedure Init (Props : out Set_Of_Properties;
                   Net, Memory, Cores, Model : String) is
   begin
      Set_Network (Props, Network'Value (Net));
      Set_Memory (Props, Memory);
      Set_Cores (Props => Props,
                 Cores => Positive'Value (Cores));
      Set_Model (Props => Props,
                 Model => Model);
   end Init;

   procedure Set_Memory (Props : in out Set_Of_Properties;
                         S     : String) is
   begin
      if S /= "" then
         Props.Memory := To_Gigs (S);
      else
         Props.Memory := 0.0;
      end if;
   end Set_Memory;

   function "<" (Left, Right : Set_Of_Properties) return Boolean is
   begin
      if Left.Network < Right.Network then
         return True;
      elsif Left.Network > Right.Network then
         return False;
      elsif Left.Model < Right.Model then
         return True;
      elsif Left.Model > Right.Model then
         return False;
      elsif Left.Memory < Right.Memory then
         return True;
      elsif Left.Memory > Right.Memory then
         return False;
      elsif Left.Cores < Right.Cores then
         return True;
      elsif Left.Cores > Right.Cores then
         return False;
      elsif Left.Runtime < Right.Runtime then
         return True;
      elsif Left.Runtime > Right.Runtime then
         return False;
      else
         return False;
      end if;
   end "<";


   ---------------------
   -- Parse_Resources --
   --  Purpose: Given a Node of an XML DOM tree,
   --  read in resources of a Host
   --  Parameter H: The Host to update
   --  Parameter V: The XML Node to read from
   ---------------------

   procedure Parse_Resource (Props : in out Set_Of_Properties;
                             N     : DOM.Core.Node) is
      A : Attr;
   begin
      A := Get_Named_Item (Attributes (N), "name");
      if Value (A) = "num_proc" then
         Props.Cores := Integer (Fixed'Value (Value (First_Child (N))));
            --  Fixed'Value is important here, as SGE interprets numerical
            --  resources as rational numbers
      elsif
        Value (A) = "ethernet" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 then
            --  Fixed'Value is important here, as SGE interprets boolean
            --  resources as rational numbers (0.000000 or 1.000000)
            Props.Network := eth;
         end if;
      elsif
         Value (A) = "infiniband" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 then
            --  see above for Fixed'Value
            Props.Network := ib;
         end if;
      elsif Value (A) = "cpu_model" then
         Props.Model := To_Model (Value (First_Child (N)));
      elsif Value (A) = "mem_total" then
         Props.Memory := To_Gigs (Value (First_Child (N)));
      else
         HTML.Put_Paragraph (Value (A), Value (First_Child (N)));
      end if;
   exception
      when E : others =>
         HTML.Error ("Unable to read resource: " & Exception_Message (E));
   end Parse_Resource;

end Host_Properties;
