
package Actions is
   procedure Invoke (What : String)
     with Pre => (What /= "");

   Permission_Error : exception;
end Actions;
