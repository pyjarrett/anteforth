with Interfaces;
with Terminal_Input_Buffers;

package VMS
  with SPARK_Mode => On, Elaborate_Body
is
   use type Terminal_Input_Buffers.Terminal_Input_Buffer;

   Max_Param_Stack_Size  : constant := 1_000;
   Max_Return_Stack_Size : constant := 128;

   --  Instructions, as stored as execution tokens.
   Max_Instructions : constant := 1_024 * 5;

   Max_Error_Message_Length : constant := 256;

   type VM_Status is
     (Ok,
      Param_Stack_Overflow,
      Param_Stack_Underflow,
      Return_Stack_Overflow,
      Return_Stack_Underflow,
      Word_Space_Exceeded,
      Name_Space_Exceeded,
      Instruction_Space_Exceeded,
      Unknown_Word,
      Value_Out_Of_Bounds,
      Invalid_Operation,
      Syntax_Error);

   type Unbounded_Value is new Interfaces.Integer_64;
   subtype Cell is Unbounded_Value range -2**31 .. 2**31;
   subtype Bounded_Value is Cell;
   type Cell_Array is array (Positive range <>) of Cell;

   subtype Param_Count is Natural range 0 .. Max_Param_Stack_Size;
   subtype Return_Count is Natural range 0 .. Max_Return_Stack_Size;

   --  Store cells here since we're going to need to refer to jump offsets for
   --  branching instructions.
   --
   --  Add one sentinel value here to ensure IP + 1 on the last instruction
   --  remains valid.
   subtype Instruction_Address is Cell range 1 .. Max_Instructions;
   subtype Instruction_Count is Cell range 0 .. Max_Instructions;
   type Instruction_Array is array (Positive range <>) of Instruction_Address;

   type VM;

   ---------------------------------------------------------------------------
   --  Word tables.
   ---------------------------------------------------------------------------
   --  Maximum length of a word's identifier.
   Max_Word_Length : constant := 33;

   --  Size in characters of the block of memory associated with storing word
   --  names.
   Word_Name_Storage_Size : constant := 1024;

   --  Total number of word headers which can be stored
   Max_Words : constant := 256;

   --  These cannot be stored as access procedures in SPARK since they have
   --  side effects, and are not representable by other words.
   type Op_Intrinsic is
     (Nop, Clear_Error, Words, Print, CR, Print_Stack, Dump_VM);

   --  Other procedures without side effects can be represented as a access to
   --  procedure.  This provides a capability for rich built-ins without
   --  needing to write a massive dispatch table.
   type Op_Procedure is not null access procedure (V : in out VMS.VM)
   with Pre => Is_Running (V);

   --  Rather than storing a separate data and code block, this is an id to key
   --  into the appropriate storage which describes the effects of the word.
   type Word_Form is
     (Form_Intrinsic, Form_Procedure_Access, Form_Instructions);
   type Word_Code (Form : Word_Form := Form_Intrinsic) is record
      case Form is
         when Form_Intrinsic =>
            Intrinsic : Op_Intrinsic := Nop;

         when Form_Procedure_Access =>
            Builtin : Op_Procedure;

         when Form_Instructions =>
            Start : Instruction_Address := 1;
      end case;
   end record;

   overriding
   function "=" (A, B : Word_Code) return Boolean
   is (A.Form = B.Form
       and then (case A.Form is
                   when Form_Intrinsic        => A.Intrinsic = B.Intrinsic,
                   when Form_Procedure_Access => A.Builtin = B.Builtin,
                   when Form_Instructions     => A.Start = B.Start));

   subtype Word_Length is Positive range 1 .. Max_Word_Length;
   subtype Word_Id is Positive;

   subtype Name_Space_Count is Natural range 0 .. Word_Name_Storage_Size;
   subtype Name_Index is Name_Space_Count range 1 .. Name_Space_Count'Last;

   -----------------------------------------------------------------------------
   --  Reserved word ids.
   -----------------------------------------------------------------------------

   --  Hard resets the virtual machine to an empty state.
   Reset : constant := 1;

   --  Indicates an error.
   Error : constant := Positive'Last;

   Cell_True  : constant := -1;
   Cell_False : constant := 0;

   --  A consistently sized description of a word.
   type Word_Header is record
      -- Range of the user-usable name for this word.
      Name_Start : Name_Index := 1;
      Name_End   : Name_Index := 1;
      Code       : Word_Code := (Form => Form_Intrinsic, Intrinsic => Nop);
      Immediate  : Boolean := False;
      --  Some words use the next value in the instruction list.  This is used
      --  when dumping the VM.
      Has_Value  : Boolean := False;
   end record;

   subtype Word_Count is Natural range 0 .. Max_Words;
   subtype Word_Index is Word_Id range 1 .. Max_Words;
   type Word_Array is array (Word_Index) of Word_Header;

   --  A description fo a set of words, and their related storage.
   type Word_Table is record
      -- Character storage for user inputs for all words.
      Names           : String (1 .. Word_Name_Storage_Size) :=
        [others => ' '];
      Name_Space_Used : Name_Space_Count := 0;
      Words           : Word_Array;
      Words_Used      : Word_Count := 0;
   end record;

   ---------------------------------------------------------------------------
   --  A Forth virtual machine.
   --
   --  Using a plain record here to allow usage of 'Old within contracts.
   ---------------------------------------------------------------------------
   type VM is record
      --  Status used for reporting errors.
      Status : VM_Status := Ok;

      Error        : String (1 .. Max_Error_Message_Length) := [others => ' '];
      Error_Length : Natural := 0;

      --  Index of the top of the parameter stack.  If 0, then the stack is empty.
      Param_Top : Param_Count := 0;

      -- The param (data) stack
      Params : Cell_Array (1 .. Max_Param_Stack_Size) := [others => 0];

      --  Index of jump-back positions in the instruction list.
      Returns     : Cell_Array (1 .. Max_Return_Stack_Size) :=
        [others => 1];  -- TODO: Maybe use an "INVALID instruction"
      Returns_Top : Return_Count := 0;

      --  Terminal input buffer providing a method of communicating text to the VM.
      TIB : Terminal_Input_Buffers.Terminal_Input_Buffer;

      --  All of the words which this VM understands.  This will include both
      --  builtins, as well as user-defined words.
      Words : Word_Table;

      --  Compiling state only runs immediate words, otherwise it is used to
      --  build words.
      Compiling : Boolean := False;

      --  Instruction memory of execution tokens used for compiled words.
      Instructions : Cell_Array (1 .. Max_Instructions) := [others => 0];

      --  Instruction pointer.  Points to the next instruction to execute when
      --  executing the current instruction.
      IP : Instruction_Address := 1;

      --  The total number of instructions written into the instruction array.
      --  Only this number of instructions are valid and should be run.
      --  This is only updated when exiting compiling mode.
      Num_Instructions : Instruction_Count := 0;
   end record;

   --  A running virtual machine can perform operations.  A non-running VM can
   --  only perform "maintenance", just as resetting or retrieving error details.
   function Is_Running (V : VM) return Boolean
   is (V.Status = Ok)
   with Global => null;

   function Is_Stopped (V : VM) return Boolean
   is (V.Status /= Ok)
   with Global => null;

   procedure Stop (V : in out VM; Status : VM_Status; Message : String)
   with
     Global => null,
     Pre    =>
       Message'Length <= Max_Error_Message_Length and then Status /= Ok,
     Post   =>
       V.Status = Status
       and then V.Error (1 .. Message'Length) = Message
       and then V.Error_Length = Message'Length
       and then Only_Status_Changed (V, V'Old);

   function Error_Message (V : VM) return String
   is (V.Error (V.Error'First .. V.Error'First + V.Error_Length));

   ---------------------------------------------------------------------------
   --  Compilation
   ---------------------------------------------------------------------------

   function Is_Compiling (V : VM) return Boolean
   is (V.Compiling)
   with Global => null;

   function Is_Interpreting (V : VM) return Boolean
   is (not Is_Compiling (V))
   with Global => null;

   function Is_Valid_IP (V : VM; IP : Cell) return Boolean
   is (IP >= 1 and then IP <= V.Num_Instructions)
   with Global => null;

   function Is_Valid_Jump (V : VM; IP : Cell) return Boolean
   is (IP >= 1 and then IP <= V.Num_Instructions + 1)
   with Global => null;

   function Can_Append_Instructions
     (V : VM; Count : Instruction_Count) return Boolean
   is (Max_Instructions - V.Num_Instructions >= Count)
   with Global => null;

   procedure Append_Instruction (V : in out VM; Inst : Cell)
   with
     Global => null,
     Pre    =>
       Is_Running (V)
       and then Is_Compiling (V)
       and then V.Num_Instructions < Max_Instructions,
     Post   =>
       Is_Running (V)
       and then Only_Instructions_Changed (V, V'Old)
       and then V.Num_Instructions = V'Old.Num_Instructions + 1
       and then Cell (V.Instructions (Positive (V.Num_Instructions))) = Inst;

   procedure Set_Instruction
     (V : in out VM; Index : Instruction_Address; Inst : Cell)
   with
     Global => null,
     Pre    => Is_Running (V) and then Index <= V.Num_Instructions,
     Post   =>
       ((for all I in 1 .. Index - 1 =>
           V.Instructions (Integer (I)) = V'Old.Instructions (Integer (I)))
        and then (for all I in Index + 1 .. Max_Instructions =>
                    V.Instructions (Integer (I))
                    = V'Old.Instructions (Integer (I)))
        and then V.Instructions (Integer (Index)) = Inst
        and then V.Num_Instructions = V'Old.Num_Instructions
        and then Only_Instructions_Changed (V, V'Old));

   procedure Step_IP (V : in out VM)
   with
     Global         => null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (V.IP = Max_Instructions =>
          not Is_Running (V) and then Only_Status_Changed (V, V'Old),
        others                  =>
          V.IP = V'Old.IP + 1
          and then Same_Params (V, V'Old)
          and then Same_Returns (V, V'Old)
          and then Same_Instructions (V, V'Old)
          and then Same_Words (V, V'Old));

   ---------------------------------------------------------------------------
   --  "Maintenance" facilities - theseDebugging and analysis functions
   ---------------------------------------------------------------------------
   procedure Dump_Param_Stack (V : VM);
   procedure Dump_VM (V : VM);

   ---------------------------------------------------------------------------
   --  Frame conditions helpers
   ---------------------------------------------------------------------------

   function Param_Stack_Equal_From_Bottom_Until
     (A, B : VM; Depth : Natural) return Boolean
   is (for all X in 1 .. Depth => A.Params (X) = B.Params (X))
   with Ghost, Global => null, Pre => Depth <= Max_Param_Stack_Size;

   function Return_Stack_Equal_From_Bottom_Until
     (A, B : VM; Depth : Natural) return Boolean
   is ((for all X in 1 .. Depth => A.Returns (X) = B.Returns (X)))
   with Ghost, Global => null, Pre => Depth <= Max_Return_Stack_Size;

   ------------------------------
   --  By system
   --
   --  Some operations only want to operate on a specific part of the VM.  These
   --  checks ensure that other parts remain unchanged.  Frame conditions assume
   --  unchecked parts have unknown values, so make these checks convenient.
   ------------------------------

   --  The core system-wide checks have the "Same" prefix.

   function Same_State (A, B : VM) return Boolean
   is (A.Compiling = B.Compiling);

   function Same_Status (A, B : VM) return Boolean
   is (A.Status = B.Status);

   function Same_TIB (A, B : VM) return Boolean
   is (A.TIB = B.TIB);

   function Same_Params (A, B : VM) return Boolean
   is ((for all X in A.Params'Range => A.Params (X) = B.Params (X))
       and then (A.Param_Top = B.Param_Top))
   with Ghost, Global => null;

   function Same_Returns (A, B : VM) return Boolean
   is ((for all X in A.Returns'Range => A.Returns (X) = B.Returns (X))
       and then (A.Returns_Top = B.Returns_Top))
   with Ghost, Global => null;

   function Same_Words (A, B : VM) return Boolean
   is (A.Words = B.Words);

   function Same_Instructions (A, B : VM) return Boolean
   is (A.Instructions = B.Instructions
       and then A.Num_Instructions = B.Num_Instructions);

   function Same_Address_Interpreter (A, B : VM) return Boolean
   is (A.Returns = B.Returns
       and then A.Returns_Top = B.Returns_Top
       and then A.IP = B.IP);

   ------------------------------
   --  All-except-this helpers
   ------------------------------

   function Same (A, B : VM) return Boolean
   is ( --
       Same_State (A, B)
       and then Same_Status (A, B)
       and then Same_TIB (A, B)
       and then Same_Params (A, B)
       and then Same_Returns (A, B)
       and then Same_Words (A, B)
       and then Same_Instructions (A, B)
       and then Same_Address_Interpreter (A, B)
       --
       )
   with Ghost;

   function Only_Status_Changed (A, B : VM) return Boolean
   is ( --
       Same_State (A, B)
       --   and then Same_Status (A, B)
       and then Same_TIB (A, B)
       and then Same_Params (A, B)
       and then Same_Returns (A, B)
       and then Same_Words (A, B)
       and then Same_Instructions (A, B)
       and then Same_Address_Interpreter (A, B)
       --
       )
   with Ghost;

   function Only_Param_Stack_Changed (A, B : VM) return Boolean
   is ( --
       Same_State (A, B)
       and then Same_Status (A, B)
       and then Same_TIB (A, B)
       --   and then Same_Params (A, B)
       and then Same_Returns (A, B)
       and then Same_Words (A, B)
       and then Same_Instructions (A, B)
       and then Same_Address_Interpreter (A, B)
       ---
       )
   with Ghost;

   function Only_Words_Changed (A, B : VM) return Boolean
   is ( --
       Same_State (A, B)
       and then Same_Status (A, B)
       and then Same_TIB (A, B)
       and then Same_Params (A, B)
       and then Same_Returns (A, B)
       --   and then Same_Words (A, B)
       and then Same_Instructions (A, B)
       and then Same_Address_Interpreter (A, B)
       --
       )
   with Ghost;

   function Only_Instructions_Changed (A, B : VM) return Boolean
   is ( --
       Same_State (A, B)
       and then Same_Status (A, B)
       and then Same_TIB (A, B)
       and then Same_Params (A, B)
       and then Same_Returns (A, B)
       and then Same_Words (A, B)
       --   and then Same_Instructions (A, B)
       and then Same_Address_Interpreter (A, B)
       --
       )
   with Ghost;

   function Only_TIB_Changed (A, B : VM) return Boolean
   is ( --
       Same_State (A, B)
       and then Same_Status (A, B)
       --   and then Same_TIB (A, B)
       and then Same_Params (A, B)
       and then Same_Returns (A, B)
       and then Same_Words (A, B)
       and then Same_Instructions (A, B)
       and then Same_Address_Interpreter (A, B)
       --
       )
   with Ghost;

   ---------------------------------------------------------------------------
   -- Operations
   ---------------------------------------------------------------------------
   --  These might be moved into subpackages at a later time.
   --  Operations are only valid on a running virtual machine.

   function Param_Stack_Size (V : VM) return Param_Count
   is (V.Param_Top);

   function Param_Peek (V : VM) return Cell
   is (V.Params (V.Param_Top))
   with Pre => Param_Stack_Size (V) > 0;

   function Param_Peek (V : VM; Depth : Param_Count) return Cell
   is (V.Params (V.Param_Top - Depth))
   with Pre => Param_Stack_Size (V) > Depth;

   procedure Param_Push (V : in out VM; C : Cell)
   with
     Global         => Null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (Param_Stack_Size (V) = Max_Param_Stack_Size =>
          not Is_Running (V) and then Only_Status_Changed (V'Old, V),
        others                                      =>
          Is_Running (V)
          and then Param_Stack_Equal_From_Bottom_Until
                     (V'Old, V, V'Old.Param_Top)
          and then V.Param_Top <= Max_Param_Stack_Size
          and then V.Param_Top = V'Old.Param_Top + 1
          and then V.Params (V.Param_Top) = C
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old) + 1
          and then Only_Param_Stack_Changed (V'Old, V));

   procedure Param_Pop (V : in out VM; C : in out Cell)
   with
     Global         => Null,
     Pre            => Is_Running (V),
     Contract_Cases =>
       (not Is_Running (V)                       =>
          not Is_Running (V) and then Same_Params (V'Old, V),
        Is_Running (V) and then V.Param_Top = 0  =>
          not Is_Running (V) and then Only_Status_Changed (V, V'Old),
        Is_Running (V) and then V.Param_Top /= 0 =>
          Is_Running (V)
          and then Only_Param_Stack_Changed (V'Old, V)
          and then Param_Stack_Equal_From_Bottom_Until (V'Old, V, V.Param_Top)
          and then V.Param_Top = V'Old.Param_Top - 1
          and then C = V.Params (V'Old.Param_Top));

   procedure Param_Multipop (V : in out VM; Depth : Positive)
   with
     Global         => Null,
     Pre            =>
       Is_Running (V)
       and then Depth > 0
       and then Depth <= Param_Stack_Size (V),
     Contract_Cases =>
       (not Is_Running (V)                       =>
          not Is_Running (V) and then Same_Params (V'Old, V),
        Is_Running (V) and then V.Param_Top = 0  =>
          not Is_Running (V) and then Only_Status_Changed (V, V'Old),
        Is_Running (V) and then V.Param_Top /= 0 =>
          Is_Running (V)
          and then Only_Param_Stack_Changed (V'Old, V)
          and then Param_Stack_Equal_From_Bottom_Until (V'Old, V, V.Param_Top)
          and then V.Param_Top = V'Old.Param_Top - Depth
          and then Param_Stack_Size (V) = Param_Stack_Size (V'Old) - Depth);

   procedure Param_Top_Print (V : in out VM)
   with
     Pre            => Is_Running (V),
     Contract_Cases =>
       (V.Param_Top = 0  =>
          not Is_Running (V) and then Only_Status_Changed (V, V'Old),
        V.Param_Top /= 0 =>
          Is_Running (V)
          and then Only_Param_Stack_Changed (V, V'Old)
          and then Param_Stack_Equal_From_Bottom_Until (V, V'Old, V.Param_Top)
          and then V.Param_Top = V'Old.Param_Top - 1);

   function Return_Stack_Size (V : VM) return Param_Count
   is (V.Returns_Top);

   ---------------------------------------------------------------------------
   --  User visible operations
   ---------------------------------------------------------------------------

   --  Tokenize, interpret and execute a string containing Forth code.
   procedure Exec (V : in out VM; Code : String)
   with
     --  Global => null, -- skip for now to allow easier output.
     Pre =>
       Is_Running (V)
       and then Code'Length <= Terminal_Input_Buffers.Max_Input_Length;

   ---------------------------------------------------------------------------
   --  Word control
   ---------------------------------------------------------------------------

   procedure Register
     (V         : in out VM;
      Name      : String;
      Intrinsic : Op_Intrinsic;
      Immediate : Boolean := False;
      Has_Value : Boolean := False)
   with
     Pre  => Name'Length in Word_Length,
     Post =>
       (not Is_Running (V) and then Only_Status_Changed (V, V'Old))
       or else (Only_Words_Changed (V, V'Old));

   procedure Register
     (V         : in out VM;
      Name      : String;
      Proc      : Op_Procedure;
      Immediate : Boolean := False;
      Has_Value : Boolean := False)
   with
     Pre  => Name'Length in Word_Length,
     Post =>
       (not Is_Running (V) and then Only_Status_Changed (V, V'Old))
       or else (Only_Words_Changed (V, V'Old));

   function Can_Allocate_Word (Table : Word_Table) return Boolean
   is (Table.Words_Used < Max_Words);

   function Can_Allocate_Name
     (Table : Word_Table; Length : Word_Length) return Boolean
   is (Table.Name_Space_Used + Length <= Word_Name_Storage_Size);

   procedure Allocate_Word
     (Table     : in out Word_Table;
      Name      : String;
      Code      : Word_Code;
      Immediate : Boolean;
      Has_Value : Boolean)
   with
     Pre  =>
       Can_Allocate_Word (Table)
       and then Name'Length in Word_Length
       and then Can_Allocate_Name (Table, Name'Length),
     Post =>
       Table.Words (Word_Index (Table.Words_Used)).Name_Start
       = Table.Name_Space_Used'Old + 1
       and then Table.Words (Word_Index (Table.Words_Used)).Name_End
                = Table.Words (Word_Index (Table.Words_Used)).Name_Start
                  - 1
                  + Name'Length
       and then Table.Words_Used = Table.Words_Used'Old + 1
       and then Table.Name_Space_Used
                = Table.Name_Space_Used'Old + Name_Space_Count (Name'Length);

   function Lookup (Table : Word_Table; Name : String) return Word_Id
   with Pre => Name'Length in Word_Length;

   function Is_Word (V : VM; Op : Word_Id) return Boolean
   is (Op <= V.Words.Words_Used);

   procedure Execute (V : in out VM; Op : Word_Id)
   with
     Contract_Cases =>
       (Is_Stopped (V) and then Op /= Reset             =>
          Is_Stopped (V) and then V.Param_Top = V.Param_Top'Old,
        Is_Stopped (V) and then Op = Reset              =>
          Is_Running (V) and then V.Param_Top = V.Param_Top'Old,
        not Is_Stopped (V) and then not Is_Word (V, Op) =>
          V.Status = Unknown_Word,
        others                                          => true);

   procedure Print_Words (V : VM);

   ---------------------------------------------------------------------------
   --  Interpreters
   ---------------------------------------------------------------------------
   procedure Run_Address_Interpreter (V : in out VM; Id : Word_Id)
   with
     Pre =>
       Is_Running (V)
       and then Is_Word (V, Id)
       and then V.Words.Words (Id).Code.Form = Form_Instructions;

end VMS;
