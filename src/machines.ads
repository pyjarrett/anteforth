with Interfaces;

package Machines
  with SPARK_Mode => On
is
   type Machine_Status is
     (Ok,
      Stack_Overflow,
      Stack_Underflow,
      Value_Out_Of_Bounds,
      Word_Space_Exceeded,
      Unknown_Word);

   type Value is new Interfaces.Integer_64;
   subtype Bounded_Value is Value range -2**31 .. 2**31;

   --  Stack configuration
   type Element_Count is new Integer range 0 .. 1024;
   Max_Stack_Size : constant := Element_Count'Last;

   --  Word configuration
   Max_Word_Length        : constant := 16;
   Word_Name_Storage_Size : constant := 256;
   Max_Words              : constant := 1024;

   subtype Word_Length is Positive range 1 .. Max_Word_Length;

   type Word_Id is new Interfaces.Unsigned_64;
   Add        : constant Word_Id := 0;
   Subtract   : constant Word_Id := 1;
   Multiply   : constant Word_Id := 2;
   Divide     : constant Word_Id := 3;
   Negate     : constant Word_Id := 4;
   Swap       : constant Word_Id := 5;
   Over       : constant Word_Id := 6;
   Rotate     : constant Word_Id := 7;
   Dupe       : constant Word_Id := 8;
   Drop       : constant Word_Id := 9;
   subtype Builtin_Op is Word_Id range Add .. Drop;
   Print      : constant Word_Id := 10;
   Dump_Stack : constant Word_Id := 11;
   subtype Side_Effect_Machine_Op is Word_Id range Print .. Dump_Stack;
   Error      : constant Word_Id := 12;
   Reset      : constant Word_Id := 13;
   subtype Error_State is Word_Id range Error .. Reset;

   type Machine is private;

   procedure Initialize (Self : in out Machine);

   function Status (Self : Machine) return Machine_Status
   with Global => null;

   function Is_Running (Self : Machine) return Boolean
   is (Status (Self) = Ok)
   with Global => null;

   function Is_Stopped (Self : Machine) return Boolean
   is (Status (Self) /= Ok)
   with Global => null;

   function Stack_Size (Self : Machine) return Element_Count
   with Global => null;

   function Is_Stack_Empty (Self : Machine) return Boolean
   is (Stack_Size (Self) = 0)
   with Global => null;

   function Is_Stack_Full (Self : Machine) return Boolean
   is (Stack_Size (Self) = Max_Stack_Size)
   with Global => null;

   function Peek (Self : Machine) return Bounded_Value
   with Global => null, Pre => not Is_Stack_Empty (Self);

   function Peek (Self : Machine; Depth : Element_Count) return Bounded_Value
   with
     Global => null,
     Pre    => Stack_Size (Self) > Depth and then not Is_Stack_Empty (Self);

   procedure Push (Self : in out Machine; Element : Bounded_Value)
   with
     Global         => null,
     Depends        => (Self => +Element),
     Contract_Cases =>
       (Is_Stack_Full (Self) => Status (Self) = Stack_Overflow,
        others               =>
          not Is_Stack_Empty (Self)
          and then Stack_Size (Self) = Stack_Size (Self'Old) + 1
          and then Peek (Self) = Element
          and then (for all X in 0 .. Stack_Size (Self'Old) - 1
                    => Peek (Self, X + 1) = Peek (Self'Old, X)));

   procedure Pop (Self : in out Machine; Count : Element_Count)
   with
     Global         => null,
     Contract_Cases =>
       (Stack_Size (Self) >= Count =>
          Stack_Size (Self) = Stack_Size (Self'Old) - Count
          and then Status (Self) = Status (Self'Old),
        Stack_Size (Self) < Count  =>
          Stack_Size (Self) = Stack_Size (Self'Old)
          and then Status (Self) = Stack_Underflow);

   procedure Execute (Self : in out Machine; Op : Word_Id)
   with
     Contract_Cases =>
       (Is_Stopped (Self) and then Op /= Reset =>
          Is_Stopped (Self) and then Stack_Size (Self) = Stack_Size (Self'Old),
        Is_Stopped (Self) and then Op = Reset  =>
          Is_Running (Self) and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                                 => true);

   function To_Machine_Op (Input : String) return Word_Id
   with Global => null;

   type Op_Procedure is access procedure (Self : in out Machine);

   type Word is private;

   --  Creates a new built-in.
   procedure Register
     (Self : in out Machine; Name : String; Proc : not null Op_Procedure)
   with Pre => Name'Length > 0 and then Name'Length <= Max_Word_Length;

private

   subtype Addend is Bounded_Value;
   subtype Minuend is Addend;
   subtype Subtrahend is Minuend;
   subtype Multiplier is Addend;
   subtype Multiplicand is Addend;
   subtype Dividend is Addend;
   subtype Prohibited_Divisor is Value range 0 .. 0;
   subtype Stack_Index is Element_Count range 1 .. Max_Stack_Size;

   type Machine_Stack is array (Stack_Index) of Bounded_Value;

   type Word is record
      -- Range of the user-usable name for this word.
      Name_Start  : Integer := 0;
      Name_Length : Integer := 0;

      -- Either builtin or Data_Position is defined.
      Builtin : Op_Procedure := null;
   end record;

   type Word_Index is new Positive range 1 .. Max_Words;
   type Word_Array is array (Word_Index) of Word;

   subtype Name_Index is Natural range 0 .. Word_Name_Storage_Size;

   type Word_Table is record
      -- Character storage for user inputs for all words.
      Name_Storage         : String (1 .. Word_Name_Storage_Size);
      Last_Name_Index      : Name_Index := 0;
      Next_Free_Word_Index : Word_Index := 1;
      Words                : Word_Array;
   end record;

   function Can_Allocate_Word (Table : Word_Table) return Boolean
   is (Table.Last_Name_Index < Word_Name_Storage_Size);

   function Can_Allocate_Name
     (Table : Word_Table; Length : Word_Length) return Boolean
   is (Table.Last_Name_Index + Length < Word_Name_Storage_Size);

   type Machine is record
      Status : Machine_Status := Ok;
      Stack  : Machine_Stack;
      Top    : Element_Count := 0;
      Words  : Word_Table;
   end record;

   function Status (Self : Machine) return Machine_Status
   is (Self.Status);

   function Stack_Size (Self : Machine) return Element_Count
   is (Self.Top);

   function Peek (Self : Machine) return Bounded_Value
   is (Self.Stack (Stack_Index (Stack_Size (Self))));

   function Peek (Self : Machine; Depth : Element_Count) return Bounded_Value
   is (Self.Stack (Stack_Index (Stack_Size (Self) - Depth)));

   procedure Op_Add (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1
            and then Peek (Self) = Peek (Self'Old, 0) + Peek (Self'Old, 1))
           or else (Self.Status = Value_Out_Of_Bounds)));

   procedure Op_Subtract (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1
            and then Peek (Self) = Peek (Self'Old, 1) - Peek (Self'Old, 0))
           or else (Self.Status = Value_Out_Of_Bounds)));

   procedure Op_Multiply (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1
            and then Peek (Self) = Peek (Self'Old, 1) * Peek (Self'Old, 0))
           or else (Self.Status = Value_Out_Of_Bounds)));

   procedure Op_Divide (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1
            and then Peek (Self) = Peek (Self'Old, 1) / Peek (Self'Old, 0))
           or else (Self.Status = Value_Out_Of_Bounds)));

   procedure Op_Negate (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Is_Stack_Empty (Self) =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          (Stack_Size (Self) = Stack_Size (Self'Old)
           and then Peek (Self) = -Peek (Self'Old)));

   procedure Op_Swap (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Stack_Size (Self) < 2 => Status (Self) = Stack_Underflow,
        others                =>
          Peek (Self, 0) = Peek (Self'Old, 1)
          and then Peek (Self, 1) = Peek (Self'Old, 0));

   procedure Op_Over (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        Is_Stack_Full (Self)  =>
          Status (Self) = Stack_Overflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          (Stack_Size (Self) = Stack_Size (Self'Old) + 1
           and then Peek (Self, 0) = Peek (Self'Old, 1)
           and then Peek (Self, 1) = Peek (Self'Old, 0)
           and then Peek (Self, 2) = Peek (Self'Old, 1)));

   procedure Op_Rotate (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Stack_Size (Self) < 3 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          (Stack_Size (Self) = Stack_Size (Self'Old)
           and then Peek (Self, 0) = Peek (Self'Old, 2)
           and then Peek (Self, 1) = Peek (Self'Old, 0)
           and then Peek (Self, 2) = Peek (Self'Old, 1)));

   procedure Op_Dupe (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Is_Stack_Full (Self)  =>
          Status (Self) = Stack_Overflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        Is_Stack_Empty (Self) =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          (Stack_Size (Self) = Stack_Size (Self'Old) + 1
           and then Peek (Self, 0) = Peek (Self'Old, 0)
           and then Peek (Self, 1) = Peek (Self'Old, 0)));

   procedure Op_Drop (Self : in out Machine)
   with
     Global         => null,
     Contract_Cases =>
       (Is_Stack_Empty (Self) => Status (Self) = Stack_Underflow,
        others                =>
          Status (Self) = Status (Self'Old)
          and then (for all X in 0 .. Stack_Size (Self) - 1
                    => Peek (Self, X) = Peek (Self'Old, X + 1)));

   procedure Op_Print (Self : in out Machine)
   with
     Contract_Cases =>
       (Is_Stack_Empty (Self) =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          (Stack_Size (Self) = Stack_Size (Self'Old) - 1));

end Machines;
