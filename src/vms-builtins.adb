with Ada.Text_IO;

package body VMS.Builtins
  with SPARK_Mode => On
is
   subtype Addend is Bounded_Value;
   subtype Minuend is Addend;
   subtype Subtrahend is Minuend;
   subtype Multiplier is Addend;
   subtype Multiplicand is Addend;
   subtype Dividend is Addend;
   subtype Prohibited_Divisor is Unbounded_Value range 0 .. 0;

   procedure Register_Builtins (V : in out VM) is
      --  Reduce number of steps required to prove this procedure by splitting
      --  it up into smaller procedures.
      procedure Register_Intrinsics (V : in out VM)
      with
        Global => null,
        Post   =>
          Same_State (V, V'Old)
          and then Same_TIB (V, V'Old)
          and then Same_Params (V, V'Old)
          and then Same_Returns (V, V'Old)
          and then Same_Instructions (V, V'Old)
          and then Same_Address_Interpreter (V, V'Old)
      is
      begin
         Register (V, "RESET", Clear_Error);
         Register (V, "WORDS", Words);
         Register (V, "DUMP", Dump_VM);
         Register (V, ".", Print);
         Register (V, ".S", Print_Stack);
         Register (V, "CR", CR);
      end Register_Intrinsics;

      procedure Register_Comparison (V : in out VM)
      with
        Global => null,
        Post   =>
          Same_State (V, V'Old)
          and then Same_TIB (V, V'Old)
          and then Same_Params (V, V'Old)
          and then Same_Returns (V, V'Old)
          and then Same_Instructions (V, V'Old)
          and then Same_Address_Interpreter (V, V'Old)
      is
      begin
         Register (V, "0=", Builtins.Op_Zero_Equal'Access);
         Register (V, "0<>", Builtins.Op_Zero_Not_Equal'Access);
         Register (V, "0<", Builtins.Op_Zero_Less_Than'Access);
         Register (V, "0>", Builtins.Op_Zero_Greater_Than'Access);

         Register (V, "=", Builtins.Op_Equal'Access);
         Register (V, "<>", Builtins.Op_Not_Equal'Access);
         Register (V, "<", Builtins.Op_Less_Than'Access);
         Register (V, ">", Builtins.Op_Greater_Than'Access);

         Register (V, "+", Builtins.Op_Add'Access);
         Register (V, "-", Builtins.Op_Subtract'Access);
         Register (V, "*", Builtins.Op_Multiply'Access);
         Register (V, "/", Builtins.Op_Divide'Access);
      end Register_Comparison;

      procedure Register_Stack_Ops (V : in out VM)
      with
        Global => null,
        Post   =>
          Same_State (V, V'Old)
          and then Same_TIB (V, V'Old)
          and then Same_Params (V, V'Old)
          and then Same_Returns (V, V'Old)
          and then Same_Instructions (V, V'Old)
          and then Same_Address_Interpreter (V, V'Old)
      is
      begin
         Register (V, "NEGATE", Builtins.Op_Negate'Access);
         Register (V, "SWAP", Builtins.Op_Swap'Access);
         Register (V, "OVER", Builtins.Op_Over'Access);
         Register (V, "ROT", Builtins.Op_Rotate'Access);
         Register (V, "DUP", Builtins.Op_Dupe'Access);
         Register (V, "DROP", Builtins.Op_Drop'Access);
         Register (V, ">R", Builtins.Op_Push_To_Return_Stack'Access);
         Register (V, "R>", Builtins.Op_Push_From_Return_Stack'Access);
      end Register_Stack_Ops;

      procedure Register_Compilation (V : in out VM)
      with
        Global => null,
        Post   =>
          Same_State (V, V'Old)
          and then Same_TIB (V, V'Old)
          and then Same_Params (V, V'Old)
          and then Same_Returns (V, V'Old)
          and then Same_Instructions (V, V'Old)
          and then Same_Address_Interpreter (V, V'Old)
      is
      begin
         Register (V, "(", Builtins.Comment'Access, Immediate => True);
         Register (V, ":", Builtins.Colon'Access);
         Register (V, ";", Builtins.Semicolon'Access, Immediate => True);
         Register (V, "[", Op_Left_Bracket'Access, Immediate => True);
         Register (V, "]", Op_Right_Bracket'Access, Immediate => True);
         Register (V, "LIT", Builtins.Op_Literal'Access, Has_Value => True);
         Register (V, "EXIT", Builtins.Op_Exit'Access);
         Register (V, "?EXIT", Builtins.Op_Conditional_Exit'Access);
      end Register_Compilation;

      procedure Register_Control_Flow (V : in out VM)
      with
        Global => null,
        Post   =>
          Same_State (V, V'Old)
          and then Same_TIB (V, V'Old)
          and then Same_Params (V, V'Old)
          and then Same_Returns (V, V'Old)
          and then Same_Instructions (V, V'Old)
          and then Same_Address_Interpreter (V, V'Old)
      is
      begin
         Register (V, "BRANCH", Builtins.Op_Branch'Access, Has_Value => True);
         Register
           (V,
            "0BRANCH",
            Builtins.Op_Branch_If_False'Access,
            Has_Value => True);
         Register (V, "IF", Builtins.Op_If'Access, Immediate => True);
         Register (V, "THEN", Builtins.Op_Then'Access, Immediate => True);
         Register (V, "ELSE", Builtins.Op_Else'Access, Immediate => True);
         Register (V, "RECURSE", Recurse'Access, Immediate => True);
         Register (V, "BEGIN", Builtins.Op_Begin'Access, Immediate => True);
         Register (V, "UNTIL", Builtins.Op_Until'Access, Immediate => True);
      end Register_Control_Flow;

   begin
      Register_Intrinsics (V);
      Register_Comparison (V);
      Register_Stack_Ops (V);
      Register_Compilation (V);
      Register_Control_Flow (V);
   end Register_Builtins;

   procedure Op_Add (V : in out VM) is
      Left, Right : Unbounded_Value;
   begin
      if Param_Stack_Size (V) < 2 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      pragma Assert (Param_Stack_Size (V) >= 2);
      Right := Param_Peek (V, 0);
      Left := Param_Peek (V, 1);
      if Left in Addend
        and then Right in Addend
        and then Left + Right in Bounded_Value
      then
         pragma Assert (Param_Stack_Size (V) >= 2);
         Param_Multipop (V, 2);
         Param_Push (V, Left + Right);
      else
         V.Status := Value_Out_Of_Bounds;
      end if;
   end Op_Add;

   procedure Op_Subtract (V : in out VM) is
      Left, Right : Unbounded_Value;
   begin
      if Param_Stack_Size (V) < 2 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      pragma Assert (Param_Stack_Size (V) >= 2);

      Right := Param_Peek (V, 0);
      Left := Param_Peek (V, 1);
      if Left in Minuend
        and then Right in Subtrahend
        and then Left - Right in Bounded_Value
      then
         Param_Multipop (V, 2);
         Param_Push (V, Left - Right);
      else
         V.Status := Value_Out_Of_Bounds;
      end if;
   end Op_Subtract;

   procedure Op_Multiply (V : in out VM) is
      Left, Right : Unbounded_Value;
   begin
      if Param_Stack_Size (V) < 2 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      Right := Param_Peek (V, 0);
      Left := Param_Peek (V, 1);
      if Left in Multiplier
        and then Right in Multiplicand
        and then Left * Right in Bounded_Value
      then
         Param_Multipop (V, 2);
         Param_Push (V, Left * Right);
      else
         V.Status := Value_Out_Of_Bounds;
      end if;
   end Op_Multiply;

   procedure Op_Divide (V : in out VM) is
      Left, Right : Unbounded_Value;
   begin
      if Param_Stack_Size (V) < 2 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      Right := Param_Peek (V, 0);
      Left := Param_Peek (V, 1);
      if Left in Dividend
        and then Right not in Prohibited_Divisor
        and then Left / Right in Bounded_Value
      then
         Param_Multipop (V, 2);
         Param_Push (V, Left / Right);
      else
         V.Status := Value_Out_Of_Bounds;
      end if;
   end Op_Divide;

   procedure Op_Negate (V : in out VM) is
      Element : Unbounded_Value;
   begin
      if Param_Stack_Size (V) = 0 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      Element := Param_Peek (V);
      Param_Multipop (V, 1);
      Param_Push (V, -Element);
   end Op_Negate;

   procedure Op_Swap (V : in out VM) is
      A, B : Unbounded_Value;
   begin
      if Param_Stack_Size (V) < 2 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      A := Param_Peek (V, 0);
      B := Param_Peek (V, 1);
      Param_Multipop (V, 2);
      pragma Assert (Is_Running (V));
      Param_Push (V, A);
      Param_Push (V, B);
   end Op_Swap;

   procedure Op_Over (V : in out VM) is
      Element : Unbounded_Value;
   begin
      if Param_Stack_Size (V) < 2 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      if Param_Stack_Size (V) = Max_Param_Stack_Size then
         V.Status := Param_Stack_Overflow;
         return;
      end if;

      Element := Param_Peek (V, 1);
      Param_Push (V, Element);

      pragma Assert (Param_Peek (V, 0) = Param_Peek (V, 2));
   end Op_Over;

   procedure Op_Rotate (V : in out VM) is
      Element1, Element2, Element3 : Unbounded_Value;
   begin
      if Param_Stack_Size (V) < 3 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      Element1 := Param_Peek (V, 2);
      Element2 := Param_Peek (V, 1);
      Element3 := Param_Peek (V, 0);
      Param_Multipop (V, 3);
      Param_Push (V, Element2);
      Param_Push (V, Element3);
      Param_Push (V, Element1);
   end Op_Rotate;

   procedure Op_Dupe (V : in out VM) is
   begin
      if Param_Stack_Size (V) = Max_Param_Stack_Size then
         V.Status := Param_Stack_Overflow;
         return;
      end if;

      if Param_Stack_Size (V) = 0 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      Param_Push (V, Param_Peek (V));
   end Op_Dupe;

   procedure Op_Drop (V : in out VM) is
   begin
      if Param_Stack_Size (V) = 0 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      V.Param_Top := V.Param_Top - 1;
   end Op_Drop;

   procedure Op_Push_To_Return_Stack (V : in out VM) is
   begin
      if Param_Stack_Size (V) = 0 then
         Stop
           (V,
            Param_Stack_Underflow,
            "Cannot move to return stack, parameter stack is empty.");
         return;
      end if;

      if Return_Stack_Size (V) = Max_Return_Stack_Size then
         Stop
           (V,
            Return_Stack_Overflow,
            "Cannot move to return stack, return stack is full.");
         return;
      end if;

      V.Returns_Top := @ + 1;
      V.Returns (V.Returns_Top) := Param_Peek (V);
      Param_Multipop (V, 1);
   end Op_Push_To_Return_Stack;

   procedure Op_Push_From_Return_Stack (V : in out VM) is
   begin
      if Return_Stack_Size (V) = 0 then
         Stop
           (V,
            Param_Stack_Underflow,
            "Cannot move to param stack, return stack is empty.");
         return;
      end if;

      if Param_Stack_Size (V) = Max_Param_Stack_Size then
         Stop
           (V,
            Return_Stack_Overflow,
            "Cannot move to param stack, param stack is full.");
         return;
      end if;

      Param_Push (V, V.Returns (V.Returns_Top));
      V.Returns_Top := @ - 1;
   end Op_Push_From_Return_Stack;

   procedure Op_Literal (V : in out VM) is
      Value : Cell := 0;
   begin
      if Is_Compiling (V) then
         if V.Num_Instructions + 2 > Max_Instructions then
            V.Status := Instruction_Space_Exceeded;
            return;
         end if;

         pragma Assert (V.Num_Instructions + 2 <= Max_Instructions);

         if Param_Stack_Size (V) > 0 then
            Param_Pop (V, Value);
            Append_Instruction (V, Cell (Lookup (V.Words, "LIT")));
            Append_Instruction (V, Value);
         else
            V.Status := Param_Stack_Underflow;
         end if;
      else
         if Param_Stack_Size (V) < Max_Param_Stack_Size then
            --  IP will be pointing after the literal instruction at this point.
            Param_Push (V, V.Instructions (Positive (V.IP)));
            Step_IP (V);
         else
            V.Status := Param_Stack_Overflow;
         end if;
      end if;
   end Op_Literal;

   procedure Comment (V : in out VM) is
      Tk : Terminal_Input_Buffers.Token;
   begin
      while Terminal_Input_Buffers.Has_Next (V.TIB) loop
         Terminal_Input_Buffers.Next_Token (V.TIB, Tk);
         declare
            Tk_String : constant String :=
              Terminal_Input_Buffers.Image (Tk, V.TIB);
         begin
            if Tk_String = ")" then
               return;
            end if;
         end;
      end loop;

      --  Comment was not terminated.
      V.Status := Syntax_Error;
   end Comment;

   procedure Colon (V : in out VM) is
      Tk : Terminal_Input_Buffers.Token;
   begin
      if Is_Compiling (V) then
         --  Double entry into the compiling state.
         V.Status := Syntax_Error;
         return;
      end if;

      if not Terminal_Input_Buffers.Has_Next (V.TIB) then
         V.Status := Syntax_Error;
         return;
      end if;

      Terminal_Input_Buffers.Next_Token (V.TIB, Tk);
      declare
         Tk_Image : constant String :=
           Terminal_Input_Buffers.Image (Tk, V.TIB);
      begin
         if Terminal_Input_Buffers.Is_Number (Tk_Image)
           or else Tk_Image'Length not in Word_Length
         then
            --  Can't redefine numbers! or have empty names.
            V.Status := Syntax_Error;
            return;
         end if;

         if not Can_Allocate_Name (V.Words, Tk_Image'Length) then
            V.Status := Name_Space_Exceeded;
            return;
         end if;

         if not Can_Allocate_Word (V.Words) then
            V.Status := Word_Space_Exceeded;
            return;
         end if;

         if V.Num_Instructions >= Max_Instructions then
            V.Status := Instruction_Space_Exceeded;
            return;
         end if;

         Allocate_Word
           (V.Words,
            Tk_Image,
            Code      =>
              (Form => Form_Instructions, Start => V.Num_Instructions + 1),
            Immediate => False,
            Has_Value => False);
      end;
      V.Compiling := True;
   end Colon;

   procedure Semicolon (V : in out VM) is
   begin
      if not V.Compiling then
         V.Status := Syntax_Error;
         return;
      end if;

      if Can_Append_Instructions (V, 1) then
         --  TODO: Check validity of this.
         Append_Instruction (V, Cell (Lookup (V.Words, "EXIT")));
         V.Compiling := False;
      else
         V.Status := Instruction_Space_Exceeded;
      end if;
   end Semicolon;

   procedure Op_Left_Bracket (V : in out VM) is
   begin
      V.Compiling := False;
   end Op_Left_Bracket;

   procedure Op_Right_Bracket (V : in out VM) is
   begin
      V.Compiling := True;
   end Op_Right_Bracket;

   procedure Recurse (V : in out VM) is
   begin
      if not Is_Compiling (V) then
         Stop (V, Syntax_Error, "Can only RECURSE when compiling.");
         return;
      end if;

      if not Can_Append_Instructions (V, 1) then
         Stop
           (V,
            Instruction_Space_Exceeded,
            "No space to append RECURSE instruction.");
         return;
      end if;

      Append_Instruction (V, Cell (V.Words.Words_Used));
   end Recurse;

   procedure Op_Exit (V : in out VM) is
   begin
      if V.Returns_Top < 1 then
         V.Status := Return_Stack_Underflow;
         return;
      end if;

      if V.Returns_Top > Max_Return_Stack_Size then
         V.Status := Return_Stack_Overflow;
         return;
      end if;

      pragma Assert (V.Returns_Top >= 1);
      pragma Assert (V.Returns_Top <= Max_Return_Stack_Size);

      if V.Returns (Positive (V.Returns_Top)) not in Instruction_Address
        or else V.Returns (Positive (V.Returns_Top)) > V.Num_Instructions
      then
         Stop
           (V,
            Invalid_Operation,
            "Return address is not a valid instruction address.");
         return;
      end if;

      V.IP := V.Returns (Positive (V.Returns_Top));
      V.Returns_Top := @ - 1;
   end Op_Exit;

   procedure Op_Conditional_Exit (V : in out VM) is
      Condition : Cell;
   begin
      if Param_Stack_Size (V) = 0 then
         Stop (V, Param_Stack_Underflow, "No condition to check before ?EXIT");
         return;
      end if;

      if V.Returns_Top < 1 then
         V.Status := Return_Stack_Underflow;
         return;
      end if;

      if V.Returns_Top > Max_Return_Stack_Size then
         V.Status := Return_Stack_Overflow;
         return;
      end if;

      if V.Returns (Positive (V.Returns_Top)) not in Instruction_Address
        or else V.Returns (Positive (V.Returns_Top)) > V.Num_Instructions
      then
         Stop
           (V,
            Invalid_Operation,
            "Return address is not a valid instruction address.");
         return;
      end if;

      Condition := Param_Peek (V);
      Param_Multipop (V, 1);
      if Condition = 0 then
         V.IP := V.Returns (Positive (V.Returns_Top));
         V.Returns_Top := @ - 1;
      end if;
   end Op_Conditional_Exit;

   procedure Op_If (V : in out VM) is
      Branch_Word : Word_Id;
   begin
      if Param_Stack_Size (V) = Max_Param_Stack_Size then
         V.Status := Param_Stack_Overflow;
         return;
      end if;

      if not Is_Compiling (V) then
         Stop (V, Invalid_Operation, "Try to run an IF.");
         return;
      end if;

      if V.Num_Instructions > Max_Instructions - 2 then
         V.Status := Instruction_Space_Exceeded;
         return;
      end if;

      --  Look up branching instruction
      Branch_Word := Lookup (V.Words, "0BRANCH");

      if not Is_Word (V, Branch_Word) then
         Stop (V, Invalid_Operation, "Could not find 0BRANCH word.");
         return;
      end if;

      Append_Instruction (V, Cell (Branch_Word));

      --  Placeholder for the jump distance
      Append_Instruction (V, 0);

      --  Mark the location where the branch instruction is
      Param_Push (V, V.Num_Instructions);

   end Op_If;

   procedure Op_Then (V : in out VM) is
      Origin : Cell;
      Target : Cell;
   begin
      if Param_Stack_Size (V) = 0 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      if not Is_Compiling (V) then
         Stop (V, Invalid_Operation, "Trying to run a THEN.");
         return;
      end if;

      Origin := Param_Peek (V);
      if Origin not in Instruction_Address then
         --  The origin to write is somehow invalid?
         V.Status := Invalid_Operation;
         Stop
           (V,
            Invalid_Operation,
            "Origin jump is not an instruction address.");
         return;
      end if;

      if Origin >= V.Num_Instructions then
         V.Status := Invalid_Operation;
         Stop
           (V,
            Invalid_Operation,
            "Jump origin is not a valid instruction: "
            & Origin'Image
            & " of "
            & V.Num_Instructions'Image);
         return;
      end if;

      Target := V.Num_Instructions - Origin + 1;
      pragma Assert (Target + Origin <= V.Num_Instructions + 1);
      if (Origin + Target) not in Instruction_Address then
         --  This might be trying to jump off the end of the instruction array.
         V.Status := Invalid_Operation;
         return;
      end if;

      Param_Multipop (V, 1);
      Set_Instruction (V, Origin, Target);
   end Op_Then;

   procedure Op_Else (V : in out VM) is
      Origin           : Cell;
      Distance_To_Else : Cell;
      Branch_Word      : Word_Id;
   begin
      if Param_Stack_Size (V) = 0 then
         Stop
           (V,
            Param_Stack_Underflow,
            "ELSE is missing a jump point from end of IF");
         return;
      end if;

      if not Is_Compiling (V) then
         V.Status := Invalid_Operation;
         Stop (V, Invalid_Operation, "ELSE is only valid when compiling.");
         return;
      end if;

      --  This is the instruction location to jump the THEN branch over the
      --  ELSE branch.
      Origin := Param_Peek (V);
      if Origin not in Instruction_Address then
         --  The origin to write is somehow invalid?
         Stop (V, Invalid_Operation, "Jump point over ELSE must be valid.");
         return;
      end if;

      if Origin >= V.Num_Instructions then
         Stop
           (V,
            Invalid_Operation,
            "Jump point over ELSE must be an assigned instruction.");
         return;
      end if;

      if V.Num_Instructions > Max_Instructions - 2 then
         V.Status := Instruction_Space_Exceeded;
         Stop
           (V,
            Instruction_Space_Exceeded,
            "Not enough instruction space to write ELSE jump.");
         return;
      end if;

      Param_Multipop (V, 1);

      --  Adds an unconditional branch over the else branch, to be patched when
      --  THEN is found.
      Branch_Word := Lookup (V.Words, "BRANCH");
      if not Is_Word (V, Branch_Word) then
         Stop (V, Invalid_Operation, "Could not find BRANCH word.");
         return;
      end if;
      Append_Instruction (V, Cell (Branch_Word));
      Append_Instruction (V, 0);
      --  Remember the jump from location.
      Param_Push (V, V.Num_Instructions);

      Distance_To_Else := V.Num_Instructions - Origin + 1;
      pragma Assert (Distance_To_Else + Origin <= V.Num_Instructions + 1);
      if (Origin + Distance_To_Else) not in Instruction_Address then
         Stop
           (V,
            Invalid_Operation,
            "ELSE jump is not valid from "
            & Origin'Image
            & " with delta "
            & Distance_To_Else'Image);
         return;
      end if;

      --  Patch the origin IF to jump to this ELSE branch.
      Set_Instruction (V, Origin, Distance_To_Else);

      pragma Assert (Distance_To_Else in Instruction_Address);
      pragma Assert (V.Instructions (Integer (Origin)) = Distance_To_Else);
      pragma
        Assert
          (Is_Valid_Jump
             (V, Cell (Origin + V.Instructions (Integer (Origin)))));
   end Op_Else;

   procedure Op_Begin (V : in out VM) is
   begin
      if not Is_Compiling (V) then
         Stop
           (V, Invalid_Operation, "BEGIN can only be used while compiling.");
         return;
      end if;

      if Param_Stack_Size (V) = Max_Param_Stack_Size then
         Stop
           (V,
            Param_Stack_Overflow,
            "Cannot push jump location, param stack is full.");
         return;
      end if;

      Param_Push (V, V.Num_Instructions);
   end Op_Begin;

   procedure Op_Until (V : in out VM) is
      Target      : Cell := 0;
      Branch_Word : Word_Id;
   begin
      if not Is_Compiling (V) then
         Stop
           (V, Invalid_Operation, "UNTIL can only be used while compiling.");
         return;
      end if;

      if Param_Stack_Size (V) = 0 then
         Stop
           (V,
            Param_Stack_Underflow,
            "No jump back address was written to stack.");
         return;
      end if;

      Param_Pop (V, Target);

      --  Adds a loopback branch if the loop condition is not met.
      Branch_Word := Lookup (V.Words, "0BRANCH");
      if not Is_Word (V, Branch_Word) then
         Stop (V, Invalid_Operation, "Could not find 0BRANCH word.");
         return;
      end if;

      if Max_Instructions - 2 < V.Num_Instructions then
         Stop
           (V,
            Instruction_Space_Exceeded,
            "No space to add loop back branch.");
         return;
      end if;

      Append_Instruction (V, Cell (Branch_Word));

      if Target - V.Num_Instructions < Cell'First then
         Stop
           (V,
            Invalid_Operation,
            "Backwards jump is further than number of previous instructions: "
            & Target'Image
            & " vs "
            & V.Num_Instructions'Image);
         return;
      end if;

      Append_Instruction (V, Target - V.Num_Instructions);
   end Op_Until;

   procedure Op_Zero_Equal (V : in out VM) is
      Value : Cell;
   begin
      if Param_Stack_Size (V) = 0 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      Value := (if Param_Peek (V) = 0 then -1 else 0);
      Param_Multipop (V, 1);
      Param_Push (V, Value);
   end Op_Zero_Equal;

   procedure Op_Zero_Not_Equal (V : in out VM) is
      Value : Cell;
   begin
      if Param_Stack_Size (V) = 0 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      Value := (if Param_Peek (V) /= 0 then -1 else 0);
      Param_Multipop (V, 1);
      Param_Push (V, Value);
   end Op_Zero_Not_Equal;

   procedure Op_Zero_Less_Than (V : in out VM) is
      Value : Cell;
   begin
      if Param_Stack_Size (V) = 0 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      Value := (if Param_Peek (V) < 0 then -1 else 0);
      Param_Multipop (V, 1);
      Param_Push (V, Value);
   end Op_Zero_Less_Than;

   procedure Op_Zero_Greater_Than (V : in out VM) is
      Value : Cell;
   begin
      if Param_Stack_Size (V) = 0 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      Value := (if Param_Peek (V) > 0 then -1 else 0);
      Param_Multipop (V, 1);
      Param_Push (V, Value);
   end Op_Zero_Greater_Than;

   procedure Op_Equal (V : in out VM) is
      A, B : Cell;
   begin
      if Param_Stack_Size (V) <= 1 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      A := Param_Peek (V, 1);
      B := Param_Peek (V, 0);
      Param_Multipop (V, 2);
      Param_Push (V, (if A = B then -1 else 0));
   end Op_Equal;

   procedure Op_Not_Equal (V : in out VM) is
      A, B : Cell;
   begin
      if Param_Stack_Size (V) <= 1 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      A := Param_Peek (V, 1);
      B := Param_Peek (V, 0);
      Param_Multipop (V, 2);
      Param_Push (V, (if A /= B then -1 else 0));
   end Op_Not_Equal;

   procedure Op_Less_Than (V : in out VM) is
      A, B : Cell;
   begin
      if Param_Stack_Size (V) <= 1 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;

      A := Param_Peek (V, 1);
      B := Param_Peek (V, 0);
      Param_Multipop (V, 2);
      Param_Push (V, (if A < B then -1 else 0));
   end Op_Less_Than;

   procedure Op_Greater_Than (V : in out VM) is
      A, B : Cell;
   begin
      if Param_Stack_Size (V) <= 1 then
         V.Status := Param_Stack_Underflow;
         return;
      end if;
      A := Param_Peek (V, 1);
      B := Param_Peek (V, 0);
      Param_Multipop (V, 2);

      Param_Push (V, (if A > B then -1 else 0));
   end Op_Greater_Than;

   procedure Op_Branch (V : in out VM) is
      Offset : constant Cell := V.Instructions (Positive (V.IP));
   begin
      if Is_Compiling (V) then
         V.Status := Syntax_Error;
         return;
      end if;

      if Offset not in Instruction_Address then
         V.Status := Invalid_Operation;
         return;
      end if;

      if Max_Instructions - Offset >= V.IP and then V.IP - Offset >= 1 then
         V.IP := @ + Instruction_Address (Offset);
      else
         V.Status := Invalid_Operation;
      end if;
   end Op_Branch;

   procedure Op_Branch_If_False (V : in out VM) is
      Condition : Cell := 0;
      Offset    : constant Cell := V.Instructions (Positive (V.IP));
   begin
      if Is_Compiling (V) then
         V.Status := Syntax_Error;
         return;
      end if;

      if Param_Stack_Size (V) = 0 then
         Stop (V, Param_Stack_Underflow, "No condition to pop from stack.");
         return;
      end if;
      Param_Pop (V, Condition);

      if Max_Instructions - Offset >= V.IP and then V.IP + Offset >= 1 then
         if Condition = Cell_False then
            V.IP := Instruction_Address (Cell (@) + Offset);
         else
            --  No branch, but still need to skip the distance cell.
            Step_IP (V);
         end if;
      else
         Stop
           (V,
            Invalid_Operation,
            "0BRANCH trying to jump to outside instruction block: "
            & V.IP'Image
            & " delta "
            & Offset'Image);
      end if;
   end Op_Branch_If_False;

end VMS.Builtins;
