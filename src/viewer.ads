with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with DOM.Core;

package Viewer is
   sgeroot : constant String := "/cm/shared/apps/sge/current";
   procedure View;
   procedure Set_Params (Params : String);
   function Setup_Parser (Selector : String) return DOM.Core.Document;
   package String_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Unbounded_String);
private
   Assumption_Error  : exception;
   My_Params : Unbounded_String;
end Viewer;
