with Ada.Text_IO;
with Machines;

package body Machines
  with SPARK_Mode => On
is
   procedure Initialize (Self : in out Machine) is
   begin
      Register (Self, "+", Op_Add'Access);
      Register (Self, "-", Op_Subtract'Access);
      Register (Self, "*", Op_Multiply'Access);
      Register (Self, "/", Op_Divide'Access);
      Register (Self, "negate", Op_Negate'Access);
      Register (Self, "swap", Op_Swap'Access);
      Register (Self, "over", Op_Over'Access);
      Register (Self, "rot", Op_Rotate'Access);
      Register (Self, "dup", Op_Dupe'Access);
      Register (Self, "drop", Op_Drop'Access);
   end Initialize;

   procedure Push (Self : in out Machine; Element : Bounded_Value) is
   begin
      if Is_Stack_Full (Self) then
         Self.Status := Stack_Overflow;
         return;
      end if;

      Self.Top := Self.Top + 1;
      pragma Assert (Self.Top in Stack_Index);
      Self.Stack (Self.Top) := Element;
      pragma Assert (Self.Stack (Self.Top) = Element);
   end Push;

   procedure Pop (Self : in out Machine; Count : Element_Count) is
   begin
      if Stack_Size (Self) >= Count then
         Self.Top := Self.Top - Count;
      else
         Self.Status := Stack_Underflow;
      end if;
   end Pop;

   procedure Execute (Self : in out Machine; Op : Word_Id) is
   begin
      if Is_Stopped (Self) then
         if Op = Reset then
            Self.Status := Ok;
         elsif Op = Dump_Stack then
            Ada.Text_IO.Put_Line ("Status: " & Self.Status'Image);
            for Index in reverse 1 .. Self.Top loop
               Ada.Text_IO.Put_Line (Self.Stack (Index)'Image);
            end loop;
         end if;
         return;
      end if;

      if Op >= Self.Words.Words_Used then
         Ada.Text_IO.Put_Line ("Unknown word:" & Op'Image);
         Self.Status := Unknown_Word;
         return;
      end if;

      case Op is
         --  Access to subprogram with global effects is not allowed in SPARK

         when Print =>
            Op_Print (Self);

         when Dump_Stack =>
            Ada.Text_IO.Put_Line
              (Self.Status'Image
               & " : "
               & Stack_Size (Self)'Image
               & "/"
               & Max_Stack_Size'Image);
            for Index in 1 .. Self.Top loop
               Ada.Text_IO.Put ("[ ");
               Ada.Text_IO.Put (Self.Stack (Index)'Image);
               Ada.Text_IO.Put (" ]");
            end loop;
            Ada.Text_IO.Put_Line (" (top) ");
            Ada.Text_IO.New_Line;

         when Other_Machine_Ops =>
            Ada.Text_IO.Put_Line ("Ignored:" & Op'Image);

         when others =>
            if Self.Words.Words (Op).Builtin /= null then
               Self.Words.Words (Op).Builtin (Self);
            end if;
      end case;
   end Execute;

   procedure Op_Add (Self : in out Machine) is
      Left, Right : Value;
   begin
      if Stack_Size (Self) < 2 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Right := Peek (Self, 0);
      Left := Peek (Self, 1);
      if Left in Addend
        and then Right in Addend
        and then Left + Right in Bounded_Value
      then
         Pop (Self, 2);
         Push (Self, Left + Right);
      else
         Self.Status := Value_Out_Of_Bounds;
      end if;
   end Op_Add;

   procedure Op_Subtract (Self : in out Machine) is
      Left, Right : Value;
   begin
      if Stack_Size (Self) < 2 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      pragma Assert (Stack_Size (Self) >= 2);

      Right := Peek (Self, 0);
      Left := Peek (Self, 1);
      if Left in Minuend
        and then Right in Subtrahend
        and then Left - Right in Bounded_Value
      then
         Pop (Self, 2);
         Push (Self, Left - Right);
      else
         Self.Status := Value_Out_Of_Bounds;
      end if;
   end Op_Subtract;

   procedure Op_Multiply (Self : in out Machine) is
      Left, Right : Value;
   begin
      if Stack_Size (Self) < 2 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Right := Peek (Self, 0);
      Left := Peek (Self, 1);
      if Left in Multiplier
        and then Right in Multiplicand
        and then Left * Right in Bounded_Value
      then
         Pop (Self, 2);
         Push (Self, Left * Right);
      else
         Self.Status := Value_Out_Of_Bounds;
      end if;
   end Op_Multiply;

   procedure Op_Divide (Self : in out Machine) is
      Left, Right : Value;
   begin
      if Stack_Size (Self) < 2 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Right := Peek (Self, 0);
      Left := Peek (Self, 1);
      if Left in Dividend
        and then Right not in Prohibited_Divisor
        and then Left / Right in Bounded_Value
      then
         Pop (Self, 2);
         Push (Self, Left / Right);
      else
         Self.Status := Value_Out_Of_Bounds;
      end if;
   end Op_Divide;

   procedure Op_Negate (Self : in out Machine) is
      Element : Value;
   begin
      if Is_Stack_Empty (Self) then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Element := Peek (Self);
      Pop (Self, 1);
      Push (Self, -Element);
   end Op_Negate;

   procedure Op_Swap (Self : in out Machine) is
      A, B : Value;
   begin
      if Stack_Size (Self) < 2 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      A := Peek (Self, 0);
      B := Peek (Self, 1);
      Pop (Self, 2);
      Push (Self, A);
      Push (Self, B);
   end Op_Swap;

   procedure Op_Over (Self : in out Machine) is
      Element : Value;
   begin
      if Stack_Size (Self) < 2 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      if Is_Stack_Full (Self) then
         Self.Status := Stack_Overflow;
         return;
      end if;

      Element := Peek (Self, 1);
      Push (Self, Element);

      pragma Assert (Peek (Self, 0) = Peek (Self, 2));
   end Op_Over;

   procedure Op_Rotate (Self : in out Machine) is
      Element1, Element2, Element3 : Value;
   begin
      if Stack_Size (Self) < 3 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Element1 := Peek (Self, 2);
      Element2 := Peek (Self, 1);
      Element3 := Peek (Self, 0);
      Pop (Self, 3);

      Push (Self, Element2);
      Push (Self, Element3);
      Push (Self, Element1);
   end Op_Rotate;

   procedure Op_Dupe (Self : in out Machine) is
   begin
      if Is_Stack_Full (Self) then
         Self.Status := Stack_Overflow;
         return;
      end if;

      if Is_Stack_Empty (Self) then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Push (Self, Peek (Self));
   end Op_Dupe;

   procedure Op_Drop (Self : in out Machine) is
   begin
      if Is_Stack_Empty (Self) then
         Self.Status := Machines.Stack_Underflow;
         return;
      end if;

      Self.Top := Self.Top - 1;
   end Op_Drop;

   procedure Op_Print (Self : in out Machine) is
      Element : Value;
   begin
      if Is_Stack_Empty (Self) then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Element := Peek (Self, 0);
      Pop (Self, 1);
      Ada.Text_IO.Put_Line (Element'Image);
   end Op_Print;

   function To_Machine_Op (Self : Machine; Input : String) return Word_Id is
   begin
      if Input'Length not in Word_Length then
         return Error;
      end if;

      if Input = "." then
         return Print;
      elsif Input = "dump" then
         return Dump_Stack;
      elsif Input = "reset" then
         return Reset;
      end if;

      return Lookup (Self.Words, Input);
   end To_Machine_Op;

   procedure Register
     (Self : in out Machine; Name : String; Proc : not null Op_Procedure) is
   begin
      if Can_Allocate_Word (Self.Words)
        and then Name'Length in Word_Length
        and then Can_Allocate_Name (Self.Words, Word_Length (Name'Length))
      then
         Allocate_Word (Self.Words, Name, Proc);
      else
         Self.Status := Word_Space_Exceeded;
      end if;
   end Register;

   procedure Allocate_Word
     (Table : in out Word_Table; Name : String; Proc : Op_Procedure) is
   begin
      declare
         New_Word    : constant Word_Index :=
           Word_Index (Table.Words_Used + 1);
         Name_Length : constant Word_Length := Word_Length (Name'Length);
      begin
         Table.Words (New_Word).Name_Start := Table.Name_Space_Used + 1;
         Table.Words (New_Word).Name_End :=
           Name_Index (Table.Name_Space_Used + Name_Length);
         Table.Words (New_Word).Builtin := Proc;
         Table.Names
           (Table.Name_Space_Used
            + 1
            .. Table.Name_Space_Used + Name_Space_Count (Name_Length)) :=
           Name (Name'First .. Name'Last);
         Table.Name_Space_Used :=
           Table.Name_Space_Used + Name_Space_Count (Name_Length);
         Table.Words_Used := Table.Words_Used + 1;
      end;
   end Allocate_Word;

   function Lookup (Table : Word_Table; Name : String) return Word_Id is
   begin
      for Index in reverse Word_Index'First .. Table.Words_Used loop
         if Table.Names
              (Table.Words (Index).Name_Start .. Table.Words (Index).Name_End)
           = Name
         then
            return Index;
         end if;
      end loop;
      return Error;
   end Lookup;
end Machines;
