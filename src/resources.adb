with HTML;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers; use Ada.Containers;
with Resources; use Resources.Resource_Lists;
with Utils; use Utils;

package body Resources is

   ------------------
   -- New_Resource --
   --  Purpose: Create a new resource with given name and value
   --  Parameter Name: name of the new resource
   --  Parameter Value: value of the new resource
   --  Returns: the newly created resource
   ------------------

   function New_Resource (Name  : Unbounded_String;
                          Value : Unbounded_String;
                          Boolean_Valued : Boolean;
                          State : Tri_State)
                          return Resource is
      R : Resource;
   begin
      R.Name := Name;
      if R.Name = "h_rt" then
         R.Numerical := Integer'Value (To_String (Value));
         R.Value := To_Unbounded_String (Format_Duration (R.Numerical));
      else
         R.Value := Value;
         R.Numerical := 0;
         R.Boolean_Valued := Boolean_Valued;
         R.State := State;
      end if;
      return R;
   end New_Resource;

   ------------------
   -- New_Resource --
   --  Purpose: Create a new resource with given name and value
   --  Parameter Name: name of the new resource
   --  Parameter Value: value of the new resource
   --  Returns: the newly created resource
   ------------------

   function New_Resource (Name : String; Value : String)
                          return Resource is
      Boolean_Valued : Boolean := False;
      State : Tri_State := Undecided;
   begin
      if Value = "TRUE" then
         Boolean_Valued := True;
         State := True;
      elsif Value = "FALSE" then
         Boolean_Valued := True;
         State := False;
      end if;
      return New_Resource (Name  => To_Unbounded_String (Name),
                           Value => To_Unbounded_String (Value),
                           Boolean_Valued => Boolean_Valued,
                           State => State);
   end New_Resource;

   ----------
   -- Hash --
   --  Purpose: Calculate a hash value for a given resource list
   --  Parameter List: The resource list to consider
   --  Returns: Hash value as a string
   ----------

   function Hash (List : Resource_Lists.List) return String is
      Temp : Ada.Containers.Hash_Type := 0;
      Pos : Resource_Lists.Cursor := List.First;
   begin
      while Pos /= Resource_Lists.No_Element loop
         Temp := Temp xor Hash (Element (Pos));
         Next (Pos);
      end loop;
      return Temp'Img;
   end Hash;

   ----------
   -- Hash --
   --  Purpose: Calculate a hash value for a given resource
   --  Parameter R : The resource to consider
   --  Returns: Hash value
   ----------

   function Hash (R : Resource) return Hash_Type is
   begin
      return Hash (R.Name) xor Hash (R.Value);
   end Hash;


   -----------------------
   -- Put               --
   --  Purpose: Output one resource as a paragraph
   --  Parameter R: the resource to print
   -----------------------

   procedure Put (R : Resource) is
      Label : Unbounded_String := R.Name;
      Value : Unbounded_String := R.Value;
   begin
      if Label = "h_rt" then
         HTML.Put_Paragraph (Label    => "<acronym title=""hard runtime limit"">h_rt</acronym>",
                             Contents => Value);
      elsif R.Boolean_Valued then
         Ada.Text_IO.Put ("<p>" & To_String (Label) & ": ");
         HTML.Put (R.State);
         Ada.Text_IO.Put ("</p>");
      else
         HTML.Put_Paragraph (Label, Value);
      end if;
   end Put;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String (L : Resource_Lists.List) return Unbounded_String is
      S : Unbounded_String := Null_Unbounded_String;
      Cursor : Resource_Lists.Cursor;
   begin
      if L.Is_Empty then
         return Null_Unbounded_String;
      end if;
      Cursor := L.First;
      loop
         S := S & Element (Cursor).Name & ": " & Element (Cursor).Value;
         exit when Cursor = L.Last;
         Cursor := Next (Cursor);
         S := S & "; ";
      end loop;
      return S;
   end To_Unbounded_String;

   ---------------
   -- To_String --
   -- Purpose: Convert a Resource_List to a String for output
   ---------------

   function To_String (L : Resource_Lists.List) return String is
   begin
      return To_String (To_Unbounded_String (L));
   end To_String;

   ---------------------
   -- Format_Duration --
   ---------------------

   function Format_Duration (Secs : Natural) return String is
      Days  : Natural;
      Dur   : Duration;
   begin
         Days := Secs / 86400;
         Dur := Ada.Real_Time.To_Duration (Ada.Real_Time.Seconds (Secs - Days*86400));
         if Days > 0 then
            return Days'Img & "d " & Image (Dur);
         else
            return Image (Dur);
         end if;
   end Format_Duration;


   ----------
   -- Sort --
   ----------

   procedure Sort (L : in out Resource_Lists.List) is
   begin
      Sorting.Sort (L);
   end Sort;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Resource_Lists.List) return Boolean is
   begin
      return Left = Right;
   end Equal;

   --------------
   -- Precedes --
   --------------

   function Precedes (Left, Right : Resource) return Boolean is
   begin
      return Left.Name < Right.Name;
   end Precedes;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Resource) return Boolean is
   begin
      if Precedes (Left, Right) then
         return True;
      elsif Precedes (Right, Left) then
         return False;
      end if;
      return Left.Value < Right.Value;
   end "<";

   --------------
   -- Precedes --
   --------------

   function Precedes (Left, Right : Resource_Lists.List) return Boolean is
      L_Cursor, R_Cursor : Resource_Lists.Cursor;
   begin
      if Left.Length < Right.Length then
         return True;
      elsif Left.Length > Right.Length then
         return False;
      elsif Left.Is_Empty then
         return False;
      end if;
      L_Cursor := Left.First;
      R_Cursor := Right.First;
      loop
         if Element (L_Cursor) < Element (R_Cursor) then
            return True;
         elsif Element (R_Cursor) < Element (L_Cursor) then
            return False;
         end if;
         exit when L_Cursor = Left.Last or else R_Cursor = Right.Last;
         L_Cursor := Next (L_Cursor);
         R_Cursor := Next (R_Cursor);
      end loop;
      return False;
   end Precedes;

   --------------
   -- To_Model --
   --------------

   function To_Model (S : String) return CPU_Model is
   begin
      if S = "" then
         return none;
      elsif S = "italy" then
         return italy;
      elsif S = "woodcrest" then
         return woodcrest;
      elsif S = "clovertown" then
         return clovertown;
      elsif S = "harpertown" then
         return harpertown;
      elsif S = "magny-cours" then
         return magnycours;
      else
         raise Constraint_Error;
      end if;
   end To_Model;

   function To_Model (S : Unbounded_String) return CPU_Model is
   begin
      return To_Model (To_String (S));
   end To_Model;

   function To_String (Model : CPU_Model) return String is
   begin
      return Model'Img;
   end To_String;

   ----------------
   -- To_Network --
   --  Purpose : Convert from a String to a Network type
   --  Parameter S: the String to read
   --  returns: The network determined from S
   --  Raises: Constraint_Error if S is not one of "NONE", "IB", "ETH"
   ----------------

   function To_Network (S : String) return Network is
   begin
      if S = "NONE" then
         return none;
      elsif S = "IB" then
         return ib;
      elsif S = "ETH" then
         return eth;
      else
         raise Constraint_Error with "Unknown network " & S;
      end if;
   end To_Network;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (List : Resource_Lists.List; Name : Unbounded_String)
                       return Unbounded_String is
      Res : Resource;
      Cursor : Resource_Lists.Cursor;
   begin
      Cursor := First (List);
      loop
         Res := Element (Cursor);
         if Res.Name = Name then
            return Res.Value;
         end if;
         exit when Cursor = Last (List);
         Next (Cursor);
      end loop;
      raise Resource_Error with "Resource """ & To_String (Name) & """ not found";
   end Get_Value;

   function Get_Value (List : Resource_Lists.List; Name : String)
                       return Unbounded_String is
   begin
      return Get_Value (List, To_Unbounded_String (Name));
   end Get_Value;

   function Get_Numerical (List : Resource_Lists.List; Name : String)
                           return Integer is
   begin
      return Get_Numerical (List, To_Unbounded_String (Name));
   end Get_Numerical;

   function Get_Numerical (List : Resource_Lists.List; Name : Unbounded_String)
                           return Integer is
      Res : Resource;
      Cursor : Resource_Lists.Cursor;
   begin
      Cursor := First (List);
      loop
         exit when Cursor = No_Element;
         Res := Element (Cursor);
         if Res.Name = Name then
            return Res.Numerical;
         end if;
         Next (Cursor);
      end loop;
      raise Resource_Error with "Resource """ & To_String (Name) & """ not found";
   end Get_Numerical;


end Resources;
