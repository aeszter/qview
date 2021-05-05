with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;
with DOM.Core;
with SGE.Host_Properties;

package Lightsout is
   type Maintenance is (none, ignore, disable, off);
   subtype Host_Name is SGE.Host_Properties.Host_Name;

   procedure Lock;
   procedure Unlock;
   procedure Read;
   procedure Write;
   procedure Clear;
   function Get_Maintenance (From : Host_Name) return String;
   procedure Set_Maintenance (Node_Name, Bug : String; To : Maintenance);
   function Get_Bug (From : Host_Name) return String;
   function Get_Bug_ID (From : Host_Name) return String;
   function Get_Bug_ID (From : Host_Name) return Natural;
   function To_String (Source : Maintenance) return String;
   function Has_Reason (From : Host_Name) return Boolean;
   function Get_Reason (From : Host_Name) return String;

   Config_Error : exception;

private

   type Host is record
      Maintain : Maintenance;
      Bug      : Natural := 0;
      Reason   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Lists is new Ada.Containers.Ordered_Maps (Element_Type => Host,
                                                     Key_Type     => Host_Name,
                                                    "<" => SGE.Host_Properties."<");

   List         : Lists.Map;
   Bugzilla_URL : Ada.Strings.Unbounded.Unbounded_String;
   XML_Doc      : DOM.Core.Document;


end Lightsout;
