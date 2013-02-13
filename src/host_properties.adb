with Parser; use Parser;
with HTML;
with Ada.Exceptions; use Ada.Exceptions;


package body Host_Properties is


   function Has_SSD (Props : Set_Of_Properties) return Boolean is
   begin
      return Props.SSD;
   end Has_SSD;

   function Has_GPU (Props : Set_Of_Properties) return Boolean is
   begin
      return Props.GPU;
   end Has_GPU;


   function Get_Load_One (Props : Set_Of_Properties) return Fixed is
   begin
      return Props.Load_One;
   end Get_Load_One;
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

   procedure Set_SSD (Props : in out Set_Of_Properties) is
   begin
      Props.SSD := True;
   end Set_SSD;

   procedure Set_GPU (Props : in out Set_Of_Properties) is
   begin
      Props.GPU := True;
   end Set_GPU;


   procedure Init (Props : out Set_Of_Properties;
                   Net, Memory, Cores, Model, SSD, GPU : String) is
   begin
      Set_Network (Props, Network'Value (Net));
      Set_Memory (Props, Memory);
      Set_Cores (Props => Props,
                 Cores => Positive'Value (Cores));
      Set_Model (Props => Props,
                 Model => Model);
      if GPU = "TRUE" then
         Set_GPU (Props => Props);
      end if;
      if SSD = "TRUE" then
         Set_SSD (Props => Props);
      end if;
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
      elsif Left.SSD < Right.SSD then
         return True;
      elsif Left.SSD > Right.SSD then
         return False;
      elsif Left.GPU < Right.GPU then
         return True;
      elsif Left.GPU > Right.GPU then
         return False;

      else
         return False;
      end if;
   end "<";

   function Get_Mismatch (Left, Right : Set_Of_Properties) return String is
   begin
      if Left.Network /= Right.Network then
         return Left.Network'Img & "/=" & Right.Network'Img;
      elsif Left.Model /= Right.Model then
         return Left.Model'Img & "/=" & Right.Model'Img;
      elsif Left.Memory /= Right.Memory then
         return Left.Memory'Img & "/=" & Right.Memory'Img;
      elsif Left.Cores /= Right.Cores then
         return Left.Cores'Img & "/=" & Right.Cores'Img;
      elsif Left.Runtime /= Right.Runtime then
         return To_String (Left.Runtime) & "/=" & To_String (Right.Runtime);
      elsif Left.SSD /= Right.SSD then
         return "SSD: " & Left.SSD'Img & "/=" & Right.SSD'Img;
      elsif Left.GPU /= Right.GPU then
         return "GPU: " & Left.GPU'Img & "/=" & Right.GPU'Img;
      else
         return "matching properties";
      end if;
   end Get_Mismatch;

   ---------------------
   -- Parse_Resources --
   --  Purpose: Given a Node of an XML DOM tree,
   --  read in resources of a Host
   --  Parameter H: The Host to update
   --  Parameter V: The XML Node to read from
   ---------------------

   procedure Parse_Resource (Props : in out Set_Of_Properties;
                             N     : Parser.Node) is
      A : Parser.Attr;
   begin
      A := Get_Attr (N, "name");
      if Value (A) = "num_proc" then
         Props.Cores := Integer (Fixed'Value (Value (First_Child (N))));
            --  Fixed'Value is important here, as SGE interprets numerical
            --  resources as rational numbers
            --  Is this code dead? Cf. Bug #1499
         HTML.Comment ("Not dead -- see Bug #1499");
      elsif Value (A) = "load_short" then
         Props.Load_One := Fixed'Value (Value (First_Child (N)));
      elsif Value (A) = "load_medium" then
         Props.Load_Five := Fixed'Value (Value (First_Child (N)));
      elsif
        Value (A) = "ethernet" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 then
            --  Fixed'Value is important here, as SGE interprets boolean
            --  resources as rational numbers (0.000000 or 1.000000)
            Props.Network := eth;
         end if;
      elsif
         Value (A) = "infiniband" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 and then
            Props.Network = none then
            --  see above for Fixed'Value
            Props.Network := ib;
         end if;
      elsif Value (A) = "ib-switch" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 then
            --  see above for Fixed'Value
            Props.Network := ibswitch;
         end if;
      elsif Value (A) = "cpu_model" then
         Props.Model := To_Model (Value (First_Child (N)));
      elsif Value (A) = "mem_total" then
         Props.Memory := To_Gigs (Value (First_Child (N)));
      elsif Value (A) = "gpu" then
         Props.GPU := True;
      elsif Value (A) = "ssd" then
         Props.SSD := True;
      else
         HTML.Error ("Unknown Resource encountered while parsing host");
         HTML.Put_Paragraph (Value (A), Value (First_Child (N)));
      end if;
   exception
      when E : others =>
         HTML.Error ("Unable to read resource: "
                     & Value (A) & " " & Exception_Message (E));
   end Parse_Resource;

   function To_String (Props : Set_Of_Properties) return String is
   begin
      return "(net=>" & Props.Network'Img
        &",model=>" & Props.Model'Img
        & ",mem=>" & Props.Memory'Img
        & ",cores=>" & Props.Cores'Img
        & "rt=>" & To_String (Props.Runtime)
      &")";
   end To_String;

end Host_Properties;
