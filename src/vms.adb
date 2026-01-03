with Ada.Text_IO;

package body VMS
  with SPARK_Mode => On
is
   procedure Stop (V : in out VM; Status : VM_Status; Message : String) is
   begin
      V.Status := Status;
      V.Error (1 .. Message'Length) := Message (Message'First .. Message'Last);
      V.Error_Length := Message'Length;
   end Stop;

   procedure Append_Instruction (V : in out VM; Inst : Cell) is
   begin
      if V.Num_Instructions = Max_Instructions then
         Stop
           (V,
            Instruction_Space_Exceeded,
            "Cannot append instruction: " & Inst'Image);
      else
         V.Num_Instructions := V.Num_Instructions + 1;
         V.Instructions (Positive (V.Num_Instructions)) := Inst;
      end if;
   end Append_Instruction;

   procedure Set_Instruction
     (V : in out VM; Index : Instruction_Address; Inst : Cell) is
   begin
      V.Instructions (Integer (Index)) := Inst;
   end Set_Instruction;

   procedure Step_IP (V : in out VM) is
   begin
      if V.IP = Max_Instructions then
         --  TODO: Catch for now, be able to verify this conclusively at
         --  some point.
         Stop (V, Invalid_Operation, "IP ran off end of instruction range.");
      else
         V.IP := V.IP + 1;
      end if;
   end Step_IP;

   procedure Dump_Param_Stack (V : VM) is
   begin
      for I in 1 .. V.Param_Top loop
         Ada.Text_IO.Put (V.Params (I)'Image & " ");
      end loop;
   end Dump_Param_Stack;

   procedure Dump_VM (V : VM) is
      procedure Safe_Col (C : Ada.Text_IO.Count) is
         use all type Ada.Text_IO.Count;
      begin
         if C > 0
           and then (C <= Ada.Text_IO.Line_Length
                     or else Ada.Text_IO.Line_Length = 0)
         then
            Ada.Text_IO.Set_Col (C);
         end if;
      exception
         when others =>
            null;
      end Safe_Col;
   begin
      Ada.Text_IO.Put_Line ("------ VM DUMP -------");

      Ada.Text_IO.Put_Line ("Status: " & V.Status'Image);
      if V.Status /= Ok then
         if V.Error_Length > 0
           and then V.Error_Length <= Max_Error_Message_Length
         then
            Ada.Text_IO.Put_Line (V.Error (1 .. V.Error_Length));
         end if;
      end if;

      Ada.Text_IO.Put ("Params: ");
      Dump_Param_Stack (V);
      Ada.Text_IO.New_Line;

      for I in 1 .. V.Words.Words_Used loop
         Ada.Text_IO.Put_Line
           ("Word"
            & I'Image
            & ": "
            & V.Words.Names
                (V.Words.Words (I).Name_Start .. V.Words.Words (I).Name_End)
            & " ("
            & V.Words.Words (Word_Index (I)).Code.Form'Image
            & ")");
      end loop;

      for I in 1 .. V.Num_Instructions loop
         Ada.Text_IO.Put (I'Image & ": ");
         if I = V.IP then
            Ada.Text_IO.Put ("-->");
         end if;
         Safe_Col (8);

         declare
            Next_Cell : constant Cell := V.Instructions (Integer (I));
            Next_Word : Word_Id;
         begin
            if Cell (Word_Id'First) <= Next_Cell
              and then Next_Cell <= Cell (Word_Id'Last)
            then
               Ada.Text_IO.Put (Next_Cell'Image);
               Safe_Col (20);
               Next_Word := Word_Id (Next_Cell);
               if Next_Word <= V.Words.Words_Used then
                  Ada.Text_IO.Put
                    (V.Words.Names
                       (V.Words.Words (Next_Word).Name_Start
                        .. V.Words.Words (Next_Word).Name_End));
                  Ada.Text_IO.New_Line;
               end if;
            else
               Ada.Text_IO.Put_Line ("<<INVALID>>");
            end if;
         end;
      end loop;

      Ada.Text_IO.Put ("Return: ");
      for I in 1 .. V.Returns_Top loop
         Ada.Text_IO.Put_Line (V.Returns (I)'Image & " ");
      end loop;
      Ada.Text_IO.New_Line;
   end Dump_VM;

   procedure Param_Push (V : in out VM; C : Cell) is
   begin
      if V.Param_Top >= Max_Param_Stack_Size then
         Stop (V, Param_Stack_Overflow, "Unable to push " & C'Image);
      else
         V.Param_Top := @ + 1;
         V.Params (V.Param_Top) := C;
      end if;
   end Param_Push;

   procedure Param_Pop (V : in out VM; C : in out Cell) is
   begin
      if V.Param_Top = 0 then
         Stop (V, Param_Stack_Underflow, "Unable to pop " & C'Image);
      else
         C := V.Params (V.Param_Top);
         V.Param_Top := V.Param_Top - 1;
      end if;
   end Param_Pop;

   procedure Param_Multipop (V : in out VM; Depth : Positive) is
   begin
      if V.Param_Top = 0 then
         Stop
           (V,
            Param_Stack_Underflow,
            "Could not pop " & Depth'Image & " elements");
      else
         V.Param_Top := V.Param_Top - Depth;
      end if;
   end Param_Multipop;

   procedure Param_Top_Print (V : in out VM) is
      C : Cell := 0;
   begin
      if V.Param_Top = 0 then
         Stop
           (V,
            Param_Stack_Underflow,
            "Cannot print top data stack element, stack is empty.");
      else
         Param_Pop (V, C);

         declare
            Image : constant String := C'Image;
         begin
            Ada.Text_IO.Put
              ((if C >= 0
                then Image (Image'First + 1 .. Image'Last)
                else Image)
               & ' ');
         end;
      end if;
   end Param_Top_Print;

   procedure Exec (V : in out VM; Code : String) is
      Tk : Terminal_Input_Buffers.Token;
      use type Terminal_Input_Buffers.Token_Kind;
   begin
      Terminal_Input_Buffers.Load_Input (V.TIB, Code);
      while Terminal_Input_Buffers.Has_Next (V.TIB) and Is_Running (V) loop
         Terminal_Input_Buffers.Next_Token (V.TIB, Tk);

         declare
            Token_Image : constant String :=
              Terminal_Input_Buffers.Image (Tk, V.TIB);
            Token_Value : Interfaces.Integer_64;
            use type Interfaces.Integer_64;
            Maybe_Word  : Word_Id;
         begin
            if Token_Image'Length > 0
              and then Terminal_Input_Buffers.Is_Number (Token_Image)
            then
               --  Push a number if there is one.
               Token_Value := Terminal_Input_Buffers.To_Number (Token_Image);
               if Token_Value >= Interfaces.Integer_64 (Cell'First)
                 and Token_Value <= Interfaces.Integer_64 (Cell'Last)
               then
                  if Is_Compiling (V) then
                     if not Can_Append_Instructions (V, 2) then
                        Stop
                          (V,
                           Instruction_Space_Exceeded,
                           "Cannot append literal instruction, out of instruction space.");
                        return;
                     end if;

                     Maybe_Word := Lookup (V.Words, "LITERAL");
                     if not Is_Word (V, Maybe_Word) then
                        Stop (V, Invalid_Operation, "No LITERAL word exists");
                        return;
                     end if;
                     Append_Instruction (V, Cell (Maybe_Word));
                     Append_Instruction (V, Cell (Token_Value));
                  else
                     Param_Push (V, Cell (Token_Value));
                  end if;
               else
                  Ada.Text_IO.Put_Line
                    ("Input value out of range: " & Token_Image);
               end if;
            else
               --  Try to run the given word.
               if Tk.Kind = Terminal_Input_Buffers.End_Of_Input then
                  null;
               else
                  if Token_Image'Length in Word_Length then
                     Maybe_Word := Lookup (V.Words, Token_Image);
                     if Is_Word (V, Maybe_Word) then
                        if not Is_Compiling (V)
                          or else V.Words.Words (Maybe_Word).Immediate
                        then
                           Execute (V, Maybe_Word);
                        else
                           if Can_Append_Instructions (V, 1) then
                              Append_Instruction (V, Cell (Maybe_Word));
                           else
                              Stop
                                (V,
                                 Instruction_Space_Exceeded,
                                 "Cannot append instruction "
                                 & Token_Image
                                 & ", out of instruction space.");
                              return;
                           end if;
                        end if;
                     else
                        Stop (V, Unknown_Word, "Unknown word:" & Token_Image);
                        Ada.Text_IO.Put_Line ("Unknown Word: " & Token_Image);
                     end if;
                  else
                     Ada.Text_IO.Put_Line
                       ("Word is not the correct length:" & Token_Image);
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Exec;

   procedure Register
     (V         : in out VM;
      Name      : String;
      Intrinsic : Op_Intrinsic;
      Immediate : Boolean := False) is
   begin
      if Can_Allocate_Word (V.Words)
        and then Name'Length in Word_Length
        and then Can_Allocate_Name (V.Words, Word_Length (Name'Length))
      then
         Allocate_Word
           (V.Words,
            Name,
            (Form => Form_Intrinsic, Intrinsic => Intrinsic),
            Immediate);
      else
         Stop
           (V,
            Word_Space_Exceeded,
            "Cannot register intrinsic word " & Name & " out of space.");
      end if;
   end Register;

   procedure Register
     (V         : in out VM;
      Name      : String;
      Proc      : Op_Procedure;
      Immediate : Boolean := False) is
   begin
      if Can_Allocate_Word (V.Words)
        and then Name'Length in Word_Length
        and then Can_Allocate_Name (V.Words, Word_Length (Name'Length))
      then
         Allocate_Word
           (V.Words,
            Name,
            (Form => Form_Procedure_Access, Builtin => Proc),
            Immediate);
      else
         Stop
           (V,
            Word_Space_Exceeded,
            "Cannot register procedure word " & Name & " out of space.");
      end if;
   end Register;

   procedure Allocate_Word
     (Table     : in out Word_Table;
      Name      : String;
      Code      : Word_Code;
      Immediate : Boolean) is
   begin
      declare
         New_Word    : constant Word_Index :=
           Word_Index (Table.Words_Used + 1);
         Name_Length : constant Word_Length := Word_Length (Name'Length);
      begin
         Table.Words (New_Word).Name_Start := Table.Name_Space_Used + 1;
         Table.Words (New_Word).Name_End :=
           Name_Index (Table.Name_Space_Used + Name_Length);
         Table.Words (New_Word).Code := Code;
         Table.Words (New_Word).Immediate := Immediate;
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

   procedure Print_Words (V : VM) is
   begin
      for Id in 1 .. V.Words.Words_Used loop
         Ada.Text_IO.Put
           (V.Words.Names
              (V.Words.Words (Id).Name_Start .. V.Words.Words (Id).Name_End)
            & " ");
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Words;

   procedure Execute (V : in out VM; Op : Word_Id) is
   begin
      if Is_Stopped (V) then
         if Op = Reset then
            V.Status := Ok;
         end if;
         return;
      end if;

      if not Is_Word (V, Op) then
         Stop (V, Unknown_Word, "No word with id: " & Op'Image);
         return;
      end if;

      case V.Words.Words (Op).Code.Form is
         when Form_Intrinsic        =>
            case V.Words.Words (Op).Code.Intrinsic is
               when Nop         =>
                  null;

               when Words       =>
                  Print_Words (V);

               when Print       =>
                  Param_Top_Print (V);

               when CR          =>
                  Ada.Text_IO.New_Line;

               when Print_Stack =>
                  for Index in 1 .. V.Param_Top loop
                     declare
                        Image : constant String := V.Params (Index)'Image;
                     begin
                        Ada.Text_IO.Put
                          ((if V.Params (Index) < 0
                            then Image
                            else Image (Image'First + 1 .. Image'Last))
                           & ' ');
                     end;
                  end loop;

               when Clear_Error =>
                  --  Handled above in Op = Reset
                  null;

               when Dump_VM     =>
                  Dump_VM (V);
            end case;

         when Form_Procedure_Access =>
            V.Words.Words (Op).Code.Builtin (V);

         when Form_Instructions     =>
            Run_Address_Interpreter (V, Op);
      end case;

   end Execute;

   procedure Run_Address_Interpreter (V : in out VM; Id : Word_Id) is
      Start     : constant Word_Header := V.Words.Words (Id);
      Next_Word : Word_Id;
   begin
      --  Ensure that the word is an interpreted (compiled) word.
      if Start.Code.Form /= Form_Instructions then
         Stop
           (V,
            Invalid_Operation,
            "Cannot use address interpreter on a built-in word.");
         return;
      end if;

      -- Set the IP to the start of the word's instructions.

      if V.Returns_Top >= Max_Return_Stack_Size then
         Stop (V, Return_Stack_Overflow, "Return stack overflow.");
         return;
      end if;

      V.Returns_Top := V.Returns_Top + 1;
      V.Returns (V.Returns_Top) := V.IP;
      V.IP := Start.Code.Start;

      loop
         declare
            Next_Inst : constant Cell := V.Instructions (Positive (V.IP));
         begin
            pragma Loop_Invariant (Is_Running (V));

            if Next_Inst < 1 or else Next_Inst > Cell (Word_Id'Last) then
               Stop
                 (V,
                  Invalid_Operation,
                  "Encountered invalid instruction: "
                  & Next_Inst'Image
                  & " at "
                  & V.IP'Image);
               return;
            end if;

            Next_Word := Word_Id (Next_Inst);
            if Is_Word (V, Next_Word) then
               --  Left here for debugging, especially proofs which need to
               --  verify jumps.
               --  if Next_Word <= V.Words.Words_Used then
               --     Dump_Param_Stack (V);
               --     Ada.Text_IO.Put
               --       ("Executing: "
               --        & V.IP'Image
               --        & "  "
               --        & V.Words.Names
               --            (V.Words.Words (Next_Word).Name_Start
               --             .. V.Words.Words (Next_Word).Name_End));
               --     Ada.Text_IO.New_Line;
               --  end if;

               --  IP could run off the end of the instruction array.  If the
               --  last instruction is a branch, then this would be ok since
               --  the IP would jump.
               Step_IP (V);
               if not Is_Running (V) then
                  return;
               end if;

               if V.Words.Words (Next_Word).Code.Form /= Form_Instructions then
                  Execute (V, Next_Word);
               else
                  if V.Returns_Top = Max_Return_Stack_Size then
                     Stop
                       (V,
                        Return_Stack_Overflow,
                        "Reached maximum return stack size.");
                  else
                     V.Returns_Top := @ + 1;
                     V.Returns (V.Returns_Top) := V.IP;
                     V.IP := V.Words.Words (Next_Word).Code.Start;
                  end if;
               end if;
            else
               Stop
                 (V,
                  Invalid_Operation,
                  "Encountered invalid word: "
                  & Next_Word'Image
                  & " at "
                  & V.IP'Image);
               return;
            end if;
            exit when V.Returns_Top = 0 or else not Is_Running (V);
         end;
      end loop;
   end Run_Address_Interpreter;

end VMS;
