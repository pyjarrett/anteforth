package VMS.Builtins
  with SPARK_Mode => On, Elaborate_Body
is
   procedure Register_Builtins (V : in out VM)
   with
     Post =>
       --  Everything except status and words should remain the same.
       Same_State (V, V'Old)
       --   and then Same_Status (V, V'Old) -- status might change
       and then Same_TIB (V, V'Old)
       and then Same_Params (V, V'Old)
       and then Same_Returns (V, V'Old)
       --   and then Same_Words (V, V'Old) -- words could change
       and then Same_Instructions (V, V'Old)
       and then Same_Address_Interpreter (V, V'Old);

   procedure Op_Add (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (V.Param_Top < 2 =>
          V.Status = Param_Stack_Underflow
          and then Only_Status_Changed (V, V'Old),
        others          =>
          (Only_Param_Stack_Changed (V, V'Old)
           and then Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
           and then V.Params (V.Param_Top)
                    = V'Old.Params (V'Old.Param_Top)
                      + V'Old.Params (V'Old.Param_Top - 1))
          or else (V.Status = Value_Out_Of_Bounds
                   and then Only_Status_Changed (V, V'Old)));

   procedure Op_Subtract (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) < 2 =>
          V.Status = Param_Stack_Underflow
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old),
        others                   =>
          ((Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
            and then Param_Peek (V)
                     = Param_Peek (V'Old, 1) - Param_Peek (V'Old, 0))
           or else (V.Status = Value_Out_Of_Bounds)));

   procedure Op_Multiply (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) < 2 =>
          V.Status = Param_Stack_Underflow
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old),
        others                   =>
          ((Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
            and then Param_Peek (V)
                     = Param_Peek (V'Old, 1) * Param_Peek (V'Old, 0))
           or else (V.Status = Value_Out_Of_Bounds)));

   procedure Op_Divide (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) < 2 =>
          V.Status = Param_Stack_Underflow
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old),
        others                   =>
          ((Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
            and then Param_Peek (V)
                     = Param_Peek (V'Old, 1) / Param_Peek (V'Old, 0))
           or else (V.Status = Value_Out_Of_Bounds)));

   procedure Op_Negate (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) = 0 =>
          V.Status = Param_Stack_Underflow
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old),
        others                   =>
          (Param_Stack_Size (V) = Param_Stack_Size (V'Old)
           and then Param_Peek (V) = -Param_Peek (V'Old)));

   procedure Op_Swap (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) < 2 => V.Status = Param_Stack_Underflow,
        others                   =>
          Param_Peek (V, 0) = Param_Peek (V'Old, 1)
          and then Param_Peek (V, 1) = Param_Peek (V'Old, 0));

   procedure Op_Over (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) < 2                    =>
          V.Status = Param_Stack_Underflow
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old),
        Param_Stack_Size (V) = Max_Param_Stack_Size =>
          V.Status = Param_Stack_Overflow
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old),
        others                                      =>
          (Param_Stack_Size (V) = Param_Stack_Size (V'Old) + 1
           and then Param_Peek (V, 0) = Param_Peek (V'Old, 1)
           and then Param_Peek (V, 1) = Param_Peek (V'Old, 0)
           and then Param_Peek (V, 2) = Param_Peek (V'Old, 1)));

   procedure Op_Rotate (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) < 3 =>
          V.Status = Param_Stack_Underflow
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old),
        others                   =>
          (Param_Stack_Size (V) = Param_Stack_Size (V'Old)
           and then Param_Peek (V, 0) = Param_Peek (V'Old, 2)
           and then Param_Peek (V, 1) = Param_Peek (V'Old, 0)
           and then Param_Peek (V, 2) = Param_Peek (V'Old, 1)));

   procedure Op_Dupe (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) = Max_Param_Stack_Size =>
          V.Status = Param_Stack_Overflow
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old),
        Param_Stack_Size (V) = 0                    =>
          V.Status = Param_Stack_Underflow
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old),
        others                                      =>
          (Param_Stack_Size (V) = Param_Stack_Size (V'Old) + 1
           and then Param_Peek (V, 0) = Param_Peek (V'Old, 0)
           and then Param_Peek (V, 1) = Param_Peek (V'Old, 0)));

   procedure Op_Drop (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) = 0 => V.Status = Param_Stack_Underflow,
        others                   =>
          V.Status = V'Old.Status
          and then (for all X in 0 .. Param_Stack_Size (V) - 1 =>
                      Param_Peek (V, X) = Param_Peek (V'Old, X + 1)));

   procedure Op_Push_To_Return_Stack (V : in out VM)
   with
     Global => null,
     Pre    => Is_Running (V),
     Post   =>
       (Return_Stack_Size (V'Old) = Max_Return_Stack_Size
        and then not Is_Running (V))
       or else (Param_Stack_Size (V'Old) = 0 and then not Is_Running (V))
       or else (Is_Running (V)
                and then V.Param_Top = V.Param_Top'Old - 1
                and then V.Returns_Top = V.Returns_Top'Old + 1
                and then V.Returns (V.Returns_Top)
                         = V'Old.Params (V.Param_Top'Old)
                and then Param_Stack_Equal_From_Bottom_Until
                           (V, V'Old, V.Param_Top)
                and then Return_Stack_Equal_From_Bottom_Until
                           (V, V'Old, V.Returns_Top'Old));

   procedure Op_Push_From_Return_Stack (V : in out VM)
   with
     Global => null,
     Pre    => Is_Running (V),
     Post   =>
       (Param_Stack_Size (V'Old) = Max_Param_Stack_Size
        and then not Is_Running (V))
       or else (Return_Stack_Size (V'Old) = 0 and then not Is_Running (V))
       or else (Is_Running (V)
                and then V.Param_Top = V.Param_Top'Old + 1
                and then V.Returns_Top = V.Returns_Top'Old - 1
                and then V'Old.Returns (V.Returns_Top'Old)
                         = V.Params (V.Param_Top)
                and then Param_Stack_Equal_From_Bottom_Until
                           (V, V'Old, V.Param_Top'Old)
                and then Return_Stack_Equal_From_Bottom_Until
                           (V, V'Old, V.Returns_Top));

   procedure Op_Literal (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Is_Compiling (V)
        and then Param_Stack_Size (V) > 0
        and then V.Num_Instructions + 2 <= Max_Instructions =>
          Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1,
        not Is_Compiling (V)
        and then Param_Stack_Size (V) < Max_Param_Stack_Size
        and then V.IP /= Max_Instructions                   =>
          Param_Stack_Size (V) = Param_Stack_Size (V'Old) + 1
          and then V.IP = V.IP'Old + 1,
        others                                              =>
          not Is_Running (V));

   procedure Comment (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (Is_Running (V) and then Only_TIB_Changed (V, V'Old))
       or else (not Is_Running (V));

   procedure Op_Left_Bracket (V : in out VM)
   with Global => null, Pre => Is_Running (V), Post => not Is_Compiling (V);

   procedure Op_Right_Bracket (V : in out VM)
   with Global => null, Pre => Is_Running (V), Post => Is_Compiling (V);

   --  Creates a new word definition.
   --
   --  Reads the new word name from the TIB.
   --  Puts the VM into compiling mode.
   procedure Colon (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (not Is_Running (V))
       or else (Is_Running (V)
                and then Same_Params (V, V'Old)
                and then Is_Compiling (V));

   --  Terminates the current word definition
   procedure Semicolon (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (not Is_Running (V))
       or else (Is_Running (V)
                and then V.Words.Words_Used = V'Old.Words.Words_Used);

   procedure Recurse (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (Is_Compiling (V)
        and then ((V.Num_Instructions'Old = Max_Instructions
                   and then not Is_Running (V))
                  or else (V.Num_Instructions = V.Num_Instructions'Old + 1
                           and then Is_Running (V)
                           and then V.Instructions
                                      (Positive (V.Num_Instructions))
                                    = Cell (V.Words.Words_Used))))
       or else (not Is_Compiling (V) and then not Is_Running (V));

   procedure Op_Exit (V : in out VM)
   with
     Pre            => Is_Running (V),
     Contract_Cases =>
       (V.Returns_Top > 0
        and then V.Returns_Top <= Max_Return_Stack_Size
        and then (V.Returns (V.Returns_Top) in Instruction_Address)
        and then V.Returns (V.Returns_Top) <= V.Num_Instructions =>
          V.IP = V'Old.Returns (V'Old.Returns_Top)
          and then Return_Stack_Equal_From_Bottom_Until
                     (V, V'Old, V.Returns_Top)
          and then V.Returns_Top = V.Returns_Top'Old - 1,
        others                                                   =>
          not Is_Running (V));

   procedure Op_Conditional_Exit (V : in out VM)
   with
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) > 0
        and then V.Returns_Top > 0
        and then V.Returns_Top <= Max_Return_Stack_Size
        and then (V.Returns (V.Returns_Top) in Instruction_Address)
        and then V.Returns (V.Returns_Top) <= V.Num_Instructions =>
          (if Param_Peek (V'Old) = 0
           then
             (V.IP = V'Old.Returns (V'Old.Returns_Top)
              and then Return_Stack_Equal_From_Bottom_Until
                         (V, V'Old, V.Returns_Top)
              and then V.Returns_Top = V.Returns_Top'Old - 1)
           else (V.IP = V.IP'Old)),
        others                                                   =>
          not Is_Running (V));

   --  "IF" adds a branch and an offset instruction, pushing the location of the
   --  offset onto the parameter stack.
   procedure Op_If (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (not Is_Running (V))
       or else (Is_Running (V)
                and then Same_Words (V, V'Old)
                and then Same_Address_Interpreter (V, V'Old)
                and then V.Param_Top = V'Old.Param_Top + 1
                and then V.Params (V.Param_Top) = V.Num_Instructions
                and then V.Num_Instructions = V.Num_Instructions'Old + 2);

   procedure Op_Then (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (not Is_Running (V))
       or else (V.Param_Top = V'Old.Param_Top - 1
                and then Param_Peek (V'Old) in Instruction_Address
                and then Max_Instructions - Param_Peek (V'Old)
                         >= V.Instructions
                              (Positive (Param_Peek (V'Old))
                               --  The jump written should be a valid IP.
                              )
                and then Is_Valid_Jump
                           (V
                            --  Origin
                            ,
                            Param_Peek
                              (V'Old
                               --  Offset
                              )
                            + V.Instructions (Positive (Param_Peek (V'Old)))));

   procedure Op_Else (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (not Is_Running (V))
       or else (V.Param_Top = V'Old.Param_Top
                and then Param_Peek (V'Old) in Instruction_Address
                and then V.Num_Instructions = V.Num_Instructions'Old + 2
                and then Max_Instructions - Param_Peek (V'Old)
                         >= V.Instructions
                              (Positive (Param_Peek (V'Old))
                               --  The jump written should be a valid IP.
                              )
                and then Is_Valid_Jump
                           (V
                            --  Origin
                            ,
                            Param_Peek
                              (V'Old
                               --  Offset
                              )
                            + V.Instructions (Positive (Param_Peek (V'Old)))));

   procedure Op_Begin (V : in out VM)
   with
     Pre            => Is_Running (V),
     Contract_Cases =>
       (not Is_Compiling (V) => not Is_Running (V),
        Is_Compiling (V)     =>
          (Param_Stack_Size (V) = Max_Param_Stack_Size
           and then not Is_Running (V))
          or else (Is_Running (V)
                   and then (Param_Stack_Equal_From_Bottom_Until
                               (V, V'Old, V.Param_Top'Old)
                             and then V.Param_Top = V.Param_Top'Old + 1)));

   procedure Op_Until (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (not Is_Running (V)
        or else (Is_Compiling (V)
                 and then Param_Stack_Size (V'Old) > 0
                 and then Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
                 and then Max_Instructions - 2 >= V.Num_Instructions'Old
                 and then V.Num_Instructions = V.Num_Instructions'Old + 2));

   procedure Op_Zero_Equal (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (Param_Stack_Size (V'Old) = 0
        and then V.Status = Param_Stack_Underflow
        and then Only_Status_Changed (V, V'Old))
       or else ((if V'Old.Params (V'Old.Param_Top) = 0
                 then V.Params (V.Param_Top) = -1
                 else V.Params (V.Param_Top) = 0)
                and then V.Param_Top = V'Old.Param_Top
                and then Param_Stack_Equal_From_Bottom_Until
                           (V, V'Old, V.Param_Top - 1)
                and then Only_Param_Stack_Changed (V, V'Old));

   procedure Op_Zero_Not_Equal (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (Param_Stack_Size (V'Old) = 0
        and then V.Status = Param_Stack_Underflow
        and then Only_Status_Changed (V, V'Old))
       or else ((if V'Old.Params (V'Old.Param_Top) /= 0
                 then V.Params (V.Param_Top) = -1
                 else V.Params (V.Param_Top) = 0)
                and then V.Param_Top = V'Old.Param_Top
                and then Param_Stack_Equal_From_Bottom_Until
                           (V, V'Old, V.Param_Top - 1)
                and then Only_Param_Stack_Changed (V, V'Old));

   procedure Op_Zero_Less_than (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (Param_Stack_Size (V'Old) = 0
        and then V.Status = Param_Stack_Underflow
        and then Only_Status_Changed (V, V'Old))
       or else ((if V'Old.Params (V'Old.Param_Top) < 0
                 then V.Params (V.Param_Top) = -1
                 else V.Params (V.Param_Top) = 0)
                and then V.Param_Top = V'Old.Param_Top
                and then Param_Stack_Equal_From_Bottom_Until
                           (V, V'Old, V.Param_Top - 1)
                and then Only_Param_Stack_Changed (V, V'Old));

   procedure Op_Zero_Greater_than (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (Param_Stack_Size (V'Old) = 0
        and then V.Status = Param_Stack_Underflow
        and then Only_Status_Changed (V, V'Old))
       or else ((if V'Old.Params (V'Old.Param_Top) > 0
                 then V.Params (V.Param_Top) = -1
                 else V.Params (V.Param_Top) = 0)
                and then V.Param_Top = V'Old.Param_Top
                and then Param_Stack_Equal_From_Bottom_Until
                           (V, V'Old, V.Param_Top - 1)
                and then Only_Param_Stack_Changed (V, V'Old));

   procedure Op_Equal (V : in out VM)
   with
     Pre            => Is_Running (V),
     Contract_Cases =>
       (V.Param_Top < 2 =>
          V.Status = Param_Stack_Underflow
          and then Only_Status_Changed (V, V'Old),
        others          =>
          (Only_Param_Stack_Changed (V, V'Old)
           and then Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
           and then (if V'Old.Params (V'Old.Param_Top - 1)
                       = V'Old.Params (V'Old.Param_Top)
                     then V.Params (V.Param_Top) = -1
                     else V.Params (V.Param_Top) = 0)));

   procedure Op_Not_Equal (V : in out VM)
   with
     Pre            => Is_Running (V),
     Contract_Cases =>
       (V.Param_Top < 2 =>
          V.Status = Param_Stack_Underflow
          and then Only_Status_Changed (V, V'Old),
        others          =>
          (Only_Param_Stack_Changed (V, V'Old)
           and then Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
           and then (if V'Old.Params (V'Old.Param_Top - 1)
                       /= V'Old.Params (V'Old.Param_Top)
                     then V.Params (V.Param_Top) = -1
                     else V.Params (V.Param_Top) = 0)));

   procedure Op_Less_than (V : in out VM)
   with
     Pre            => Is_Running (V),
     Contract_Cases =>
       (V.Param_Top < 2 =>
          V.Status = Param_Stack_Underflow
          and then Only_Status_Changed (V, V'Old),
        others          =>
          (Only_Param_Stack_Changed (V, V'Old)
           and then Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
           and then (if V'Old.Params (V'Old.Param_Top - 1)
                       < V'Old.Params (V'Old.Param_Top)
                     then V.Params (V.Param_Top) = -1
                     else V.Params (V.Param_Top) = 0)));

   procedure Op_Greater_than (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       (V'Old.Param_Top < 2
        and then V.Status = Param_Stack_Underflow
        and then Only_Status_Changed (V, V'Old))
       or else (Only_Param_Stack_Changed (V, V'Old)
                and then Param_Stack_Equal_From_Bottom_Until
                           (V, V'Old, V.Param_Top - 1)
                and then Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
                and then (if V'Old.Params (V'Old.Param_Top - 1)
                            > V'Old.Params (V'Old.Param_Top)
                          then V.Params (V.Param_Top) = -1
                          else V.Params (V.Param_Top) = 0));

   procedure Op_Branch (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       not Is_Running (V)
       or else V.IP = V'Old.IP + V.Instructions (Positive (V'Old.IP));

   procedure Op_Branch_If_False (V : in out VM)
   with
     Pre  => Is_Running (V),
     Post =>
       not Is_Running (V)
       or else (Param_Stack_Size (V) = Param_Stack_Size (V'Old) - 1
                and then (V.IP = V.IP'Old + 1
                          or else V.IP
                                  = V.IP'Old
                                    + V.Instructions (Positive (V.IP'Old))));

end VMS.Builtins;
