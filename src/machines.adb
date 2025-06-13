with Ada.Text_IO;
with Machines;

package body Machines
  with SPARK_Mode => On
is
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

   procedure Execute (Self : in out Machine; Op : Machine_Op) is
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

      case Op is
         when Add =>
            Op_Add (Self);

         when Subtract =>
            Op_Subtract (Self);

         when Multiply =>
            Op_Multiply (Self);

         when Divide =>
            Op_Divide (Self);

         when Negate =>
            Op_Negate (Self);

         when Over =>
            Op_Over (Self);

         when Rotate =>
            Op_Rotate (Self);

         when Dupe =>
            Op_Dupe (Self);

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

         when others =>
            Ada.Text_IO.Put_Line ("Ignored: " & Op'Image);
      end case;
   end Execute;

   procedure Op_Add (Self : in out Machine) is
      Left, Right : Value;
   begin
      pragma Assert (Is_Running (Self));
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

   procedure Op_Over (Self : in out Machine) is
      Element : Value;
   begin
      if Stack_Size (Self) < 2 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      if Is_Stack_Full (Self) then
         Self.Status := Machines.Stack_Overflow;
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

   function To_Machine_Op (Input : String) return Machine_Op is
   begin
      if Input = "+" then
         return Add;
      elsif Input = "-" then
         return Subtract;
      elsif Input = "*" then
         return Multiply;
      elsif Input = "/" then
         return Divide;
      elsif Input = "negate" then
         return Negate;
      elsif Input = "rot" then
         return Rotate;
      elsif Input = "." then
         return Print;
      elsif Input = "dup" then
         return Dupe;
      elsif Input = "dump" then
         return Dump_Stack;
      elsif Input = "reset" then
         return Reset;
      else
         return Error;
      end if;
   end To_Machine_Op;

end Machines;
