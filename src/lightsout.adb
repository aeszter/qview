with DOM; use DOM;
with DOM.Core; use DOM.Core;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with DOM.Readers;
with Sax.Readers; use Sax.Readers;
with Input_Sources.File; use Input_Sources.File;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Elements;
with Ada.Characters;
with Ada.Characters.Handling;

package body Lightsout is
   procedure Add_Host (Name : String; Mode : String; Bug : Natural);
   function To_Host_Name (From : String) return Host_Name;

   Lightsout_File : constant String := "/etc/lights-out.xml";

   procedure Clear is
   begin
      List.Clear;
   end Clear;

   function Get_Bug (From : String) return String is
      Bug_ID : constant Natural := Get_Bug_ID (From);
   begin
      if Bug_ID > 0 then
         return "<a href=""" & Ada.Strings.Unbounded.To_String (Bugzilla_URL)
           & "/show_bug.cgi?id=" & Bug_ID'Img
         & """>Bug" & Bug_ID'Img & "</a>";
      else
         return "";
      end if;
   end Get_Bug;

   function Get_Bug_ID (From : String) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (Integer'Image (Get_Bug_ID (From)), Left);
   end Get_Bug_ID;

   function Get_Bug_ID (From : String) return Natural is
      Name : constant Host_Name := To_Host_Name (From);
   begin
      if List.Contains (Name) then
         return List.Element (Name).Bug;
      else
         return 0;
      end if;
   end Get_Bug_ID;


   function Get_Maintenance (From : String) return String is
      Name : constant Host_Name := To_Host_Name (From);
      Maint_Status : Maintenance := none;
   begin
      if List.Contains (Name) then
         Maint_Status := List.Element (Name).Maintain;
      end if;
      case Maint_Status is
         when none =>
            return "";
         when ignore =>
            return "<acronym title=""ignored by lightsout"">"
              & "<img src=""/icons/ignore.png""></acronym>";
         when disable =>
            return "<acronym title=""disable for maintenance"">"
              & "<img src=""/icons/disable.png""></acronym>";
         when off =>
            return "<acronym title=""poweroff for maintenance"">"
              & "<img src=""/icons/off.png""></acronym>";
      end case;
   end Get_Maintenance;

   procedure Read is
      Reader                : DOM.Readers.Tree_Reader;
      All_Nodes             : Node_List;
      Config_Node, One_Node : Node;
      File                  : File_Input;
   begin
      Open (Filename => Lightsout_File,
            Input => File);

      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (File);
      Close (Input => File);
      XML_Doc := Reader.Get_Tree;
      Config_Node := First_Child (XML_Doc);
      All_Nodes := Child_Nodes (Config_Node);

      if Name (Config_Node) /= "config" then
         raise Config_Error with "Found unexpected """ & Name (Config_Node)
           & """ at top level in lightsout config file";
      end if;

      for I in 0 .. Length (All_Nodes) - 1 loop
         One_Node := Item (All_Nodes, I);
         if Node_Name (One_Node) = "nodegroup" then
            declare
               Group_Nodes, Twin_Nodes : Node_List;
               Group_Node, Sub_Node    : Node;
               Maint_Attr              : Attr;
               Bug_Attr                : Attr;
               Bug_ID                  : Natural;
            begin
               Group_Nodes := Child_Nodes (One_Node);
               for J in 0 .. Length (Group_Nodes) - 1 loop
                  Group_Node := Item (Group_Nodes, J);
                  if Name (Group_Node) = "target"
                 or else Name (Group_Node) = "minimum"
                 or else Name (Group_Node) = "maximum"
                  then
                     null; -- ignore: only for lightsout itself
                  elsif Name (Group_Node) = "nodename" then
                     Maint_Attr := Get_Named_Item (Attributes (Group_Node), "maint");
                     Bug_Attr := Get_Named_Item (Attributes (Group_Node), "bug");
                     if Bug_Attr = null then
                        Bug_ID := 0;
                     else
                        Bug_ID := Integer'Value (Value (Bug_Attr));
                     end if;
                     if Maint_Attr = null then
                        Add_Host (Name => Value (First_Child (Group_Node)),
                                  Mode => "none", Bug => Bug_ID);
                     else
                        Add_Host (Name => Value (First_Child (Group_Node)),
                                  Mode => Value (Maint_Attr), Bug => Bug_ID);
                     end if;
                  elsif Name (Group_Node) = "twin" then
                     Maint_Attr := Get_Named_Item (Attributes (Group_Node), "maint");
                     Bug_Attr := Get_Named_Item (Attributes (Group_Node), "bug");
                     if Bug_Attr = null then
                        Bug_ID := 0;
                     else
                        Bug_ID := Integer'Value (Value (Bug_Attr));
                     end if;
                     Twin_Nodes := Child_Nodes (Group_Node);
                     for J in 0 .. Length (Twin_Nodes) - 1 loop
                        Sub_Node := Item (Twin_Nodes, J);
                        if Name (Sub_Node) = "nodename" then
                           if Maint_Attr = null then
                              Add_Host (Value (First_Child (Sub_Node)), "none", Bug_ID);
                           else
                              Add_Host (Value (First_Child (Sub_Node)), Value (Maint_Attr), Bug_ID);
                           end if;
                        end if;
                     end loop;
                  elsif Name (Group_Node) = "#text" or else
                    Name (Group_Node) = "#comment"
                  then
                     null; -- ignore
                  else
                     raise Config_Error with "Found unexpected """
                       & Name (Group_Node) & """ in <nodegroup>";
                  end if;
               end loop;
            end;
         elsif Name (One_Node) = "bugzilla" then
            Bugzilla_URL := Ada.Strings.Unbounded.To_Unbounded_String
              (Value (First_Child (One_Node)));
         elsif Node_Name (One_Node) = "#text" or else
           Node_Name (One_Node) = "#comment"
         then
            null; -- ignore
         else
            raise Config_Error with "Found unexpected """
              & Node_Name (One_Node) & """ in <config> while reading config file";
         end if;
      end loop;

   exception
         when Config_Error => raise;
      when E : others =>
         raise Config_Error with "Unable to read config file: " & Exception_Message (E);
   end Read;

   procedure Add_Host (Name : String; Mode : String; Bug : Natural) is
   begin
      List.Insert (Key => To_Host_Name (Name),
                   New_Item => (Maintain => Maintenance'Value (Mode), Bug => Bug));
   end Add_Host;

   function To_Host_Name (From : String) return Host_Name is
      Period : Natural := Ada.Strings.Fixed.Index (Source  => From,
                                                    Pattern => ".");
   begin
      if Period < 1 then
         Period := From'Last + 1;
      end if;
      return Host_Names.To_Bounded_String (Source => From (From'First .. Period - 1),
                                           Drop   => Ada.Strings.Error);
   end To_Host_Name;

   function To_String (Source : Maintenance) return String is
   begin
      return Ada.Characters.Handling.To_Lower (Source'Img);
   end To_String;

   procedure Set_Maintenance (Node_Name, Bug : String; To : Maintenance) is
      All_Nodes : Node_List;
      One_Node  : Node;
      use DOM.Core.Elements;
   begin
      All_Nodes := Get_Elements_By_Tag_Name (Doc      => XML_Doc,
                                             Tag_Name => "nodename");
      for I in 0 .. Length (All_Nodes) - 1 loop
         One_Node := Item (All_Nodes, I);
         if Has_Child_Nodes (One_Node)
           and then Value (First_Child (One_Node)) = Node_Name
         then
            case To is
               when none  =>
                  Remove_Attribute (Elem => One_Node,
                                    Name => "maint");
                  Remove_Attribute (Elem => One_Node,
                                    Name => "bug");
               when others =>
                  Set_Attribute (One_Node, "maint", To_String (To));
                  Set_Attribute (One_Node, "bug", Bug);
            end case;
            return;
         end if;
      end loop;
      raise Constraint_Error with "could not find " & Node_Name;
   end Set_Maintenance;

   procedure Write is
      use Ada.Streams.Stream_IO;

      File_Handle : File_Type;
   begin
      Create (File_Handle, Out_File, Lightsout_File);
      DOM.Core.Nodes.Write (Stream                => Stream (File_Handle),
                            N                     => XML_Doc,
                            Print_XML_Declaration => False);
      Close (File_Handle);
   end Write;

end Lightsout;
