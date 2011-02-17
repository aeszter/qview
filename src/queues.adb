with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Resources; use Resources;

package body Queues is

   ---------------
   -- New_Queue --
   --  Purpose: Create a new queue with the given resources and slots
   --  Parameter Used: Number of slots in use
   --  Parameter Reserved: Number of slots used for advance reservations
   --  Parameter Total: Number of total slots in queue
   --  Parameter Memory: RAM in queue
   --  Parameter Cores: number of cores in queue
   --  Parameter Network: type of network in queue
   --  Parameter Runtime: runtime limit of queue
   --  Returns:  the newly created queue
   ---------------

   function New_Queue
     (Used, Reserved, Total : Natural;
      State                 : String;
      Memory                : String;
      Cores                 : Natural;
      Network               : Resources.Network;
      Model                 : Resources.CPU_Model;
      Runtime               : Unbounded_String)
      return Queue
   is
      Q : Queue;
   begin
      Q.Used     := Used;
      Q.Reserved := Reserved;
      Q.Total    := Total;
      Q.Offline  := False;
      Q.Suspended := False;
      if State /= "" and then
        Index (Source  => State,
                Pattern => "u") /= 0 then
         Q.Offline := True;
      elsif Index (Source => State, Pattern => "d") /= 0 then
         Q.Suspended := True;
      end if;

      if Memory /= "" then
         Q.Memory := To_Gigs (Memory);
      else
         Q.Memory := 0.0;
      end if;

      Q.Network  := Network;
      Q.Model    := Model;
      Q.Runtime  := Runtime;
      if Cores = 0 then
         Q.Cores := Q.Total;
      else
         Q.Cores := Cores;
      end if;

      return Q;
   end New_Queue;

   -------------
   -- To_Gigs --
   -------------

   function To_Gigs (Memory : String) return Gigs is
   begin
      if Memory (Memory'Last) = 'G' then
         return Gigs'Value (Memory (Memory'First .. Memory'Last - 1));
      elsif Memory (Memory'Last) = 'M' then
         return Gigs'Value (Memory (Memory'First .. Memory'Last - 1)) / 1024.0;
      else
         raise Constraint_Error with "unknown memory encountered: " & Memory;
      end if;
   end To_Gigs;


   ---------------------------
   -- Precedes_By_Resources --
   ---------------------------

   function Precedes_By_Resources (Left, Right : Queue) return Boolean is
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
   end Precedes_By_Resources;

end Queues;
