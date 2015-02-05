
package Actions is
   procedure Invoke (What : String)
     with Pre => (What /= "");
end Actions;
