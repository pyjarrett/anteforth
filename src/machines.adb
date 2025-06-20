with Ada.Text_IO;
with Machines;

package body Machines
  with SPARK_Mode => On
is
   procedure Initialize (Self : in out Machine) is
   begin
      Self.Word_Name_Storage := [others => ' '];
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

      if Op >= Word_Id (Self.Next_Free_Word_Index) then
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

         when others =>
            Ada.Text_IO.Put_Line ("Ignored: " & Op'Image);
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

   function To_Machine_Op (Input : String) return Word_Id is
   begin
      -- TODO: This should be a lookup in reverse order from the most recently registered word.
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
      elsif Input = "swap" then
         return Swap;
      elsif Input = "over" then
         return Over;
      elsif Input = "rot" then
         return Rotate;
      elsif Input = "dupe" then
         return Dupe;
      elsif Input = "drop" then
         return Drop;
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

   procedure Register
     (Self : in out Machine; Name : String; Proc : not null Op_Procedure)
   is
      Last_Word_Index : constant Word_Index := Self.Next_Free_Word_Index;
   begin
      if Word_Index'Last - Last_Word_Index >= Self.Next_Free_Word_Index then
         -- Ran out of word storage.
         return;
      end if;

      if Positive'Last - Name'Length + 1
        >= Positive (Self.Next_Free_Word_Index)
      then
         return;
      end if;

      Self.Next_Free_Word_Index := Self.Next_Free_Word_Index - 1 + Name'Length;
      Self.Word_Name_Storage
        (Positive (Self.Next_Free_Word_Index) .. Positive (Last_Word_Index)) :=
        Name (Name'First .. Name'Last);
      Self.Next_Free_Word_Index := Last_Word_Index;

      Self.Words (Last_Word_Index) := (others => <>);
      Self.Words (Last_Word_Index).Builtin := Proc;
      Self.Words (Last_Word_Index).Immediate := False;
      Self.Words (Last_Word_Index).Name_Start := 0;
      Self.Words (Last_Word_Index).Name_Length := Name'Length;

      Self.Words (Last_Word_Index).Data_Position := 0;
      Self.Words (Last_Word_Index).Data_Position_Length := 0;
   end Register;
end Machines;
