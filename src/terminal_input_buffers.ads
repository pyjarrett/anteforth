with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Interfaces;

package Terminal_Input_Buffers
  with SPARK_Mode => On, Pure, Always_Terminates
is
   package ACH renames Ada.Characters.Handling;

   Max_Input_Length : constant := 1024;
   subtype Small_Int is Integer range 0 .. Max_Input_Length + 1;
   subtype Range_Size is Small_Int range 0 .. Max_Input_Length;

   type Cursor_Index is new Positive range 1 .. Max_Input_Length + 1;
   subtype Range_Index is Cursor_Index range 1 .. Max_Input_Length;

   --  A range within a terminal input buffer at a specific time.
   type Lexeme_Range is private;
   function Width (Self : Lexeme_Range) return Range_Size;

   type Token_Kind is (Word, End_Of_Input);

   --  Tokens are associated with a specific terminal input buffer.
   type Token is record
      Kind   : Token_Kind := End_Of_Input;
      Lexeme : Lexeme_Range;
   end record;

   --  A terminal input buffer:
   --  * a block of text input
   --  * a currently parsed lexeme (word)
   --  * cursor position
   type Terminal_Input_Buffer is private;

   ---------------------------------------------------------------------------
   --  Frame condition helpers
   ---------------------------------------------------------------------------

   ------------------------------
   --  Input validity
   ------------------------------

   function Has_Input (Self : Terminal_Input_Buffer) return Boolean;

   function Input_Size (Self : Terminal_Input_Buffer) return Range_Size;

   function Input_Unchanged (A, B : Terminal_Input_Buffer) return Boolean
   with Ghost;

   ------------------------------
   --  Parsing state validity
   ------------------------------

   --  Whether or not there are more characters to examine.
   function Has_More_Characters (Self : Terminal_Input_Buffer) return Boolean
   with
     Post =>
       (if Has_More_Characters'Result then Remaining_Characters (Self) > 0);

   --  The number of remaining unparsed characters.
   function Remaining_Characters
     (Self : Terminal_Input_Buffer) return Range_Size;

   ------------------------------
   --  Lexeme validity
   ------------------------------

   --  A valid lexeme is a non-empty range within the buffer.
   function Has_Lexeme (Self : Terminal_Input_Buffer) return Boolean
   with Ghost;

   function Lexeme_Size (Self : Terminal_Input_Buffer) return Range_Size
   with Post => Lexeme_Size'Result <= Input_Size (Self);

   ---------------------------------------------------------------------------
   --  Token utilities
   ---------------------------------------------------------------------------
   function Is_Word (Self : Terminal_Input_Buffer; Tk : Token) return Boolean;

   function Is_Number (Input : String) return Boolean
   with Pre => Input'Length < Natural'Last - 1;

   --  Parse an integer.
   function To_Number (Input : String) return Interfaces.Integer_64
   with
     Pre =>
       Input'Length > 0
       and then Input'Length < Natural'Last - 1
       and then Input'First <= Positive'Last - 1
       and then Is_Number (Input);

   function Image (Tk : Token; S : Terminal_Input_Buffer) return String
   with
     Post =>
       Image'Result'Length <= Max_Input_Length
       and then Image'Result'First <= Max_Input_Length
       and then Image'Result'Last <= Max_Input_Length;

   function Contains (Self : Terminal_Input_Buffer; Tk : Token) return Boolean;
   function Contains
     (Self : Terminal_Input_Buffer; Lexeme : Lexeme_Range) return Boolean;

   ---------------------------------------------------------------------------
   -- Main public interface
   ---------------------------------------------------------------------------

   procedure Load_Input (Self : in out Terminal_Input_Buffer; Input : String)
   with
     Depends        => (Self => +Input),
     Pre            => Input'Length <= Max_Input_Length,
     Contract_Cases =>
       (Input'Length = 0 =>
          Remaining_Characters (Self) = 0
          and then not Has_More_Characters (Self)
          and then Input_Size (Self) = 0,
        others           =>
          Remaining_Characters (Self) = Input'Length
          and then Has_More_Characters (Self)
          and then Input_Size (Self) = Input'Length);

   function Has_Next (Self : Terminal_Input_Buffer) return Boolean
   with
     Post =>
       ((Has_Next'Result and then Remaining_Characters (Self) > 0)
        or else (not Has_Next'Result and then Remaining_Characters (Self) = 0)
        or else (not Has_Next'Result and then not Has_Input (Self)));

   procedure Next_Token (Self : in out Terminal_Input_Buffer; Tk : out Token)
   with
     Pre  => Has_Next (Self) and then Has_More_Characters (Self),
     Post =>
       Input_Unchanged (Self, Self'Old)
       and then (Remaining_Characters (Self) < Remaining_Characters (Self'Old))
       and then (Tk.Kind = End_Of_Input
                 or else (Tk.Kind = Word
                          and then Width (Tk.Lexeme) > 0
                          and then Is_Word (Self, Tk)));

   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   procedure Ignore_Lexeme (Self : in out Terminal_Input_Buffer)
   with
     Post =>
       Input_Unchanged (Self, Self'Old)
       and then Lexeme_Size (Self) = 0
       and then Remaining_Characters (Self) = Remaining_Characters (Self'Old)
       and then Has_Next (Self) = Has_Next (Self'Old)
       and then Peek (Self) = Peek (Self'Old);

   function Peek (Self : Terminal_Input_Buffer) return Character;

   procedure Next (Self : in out Terminal_Input_Buffer)
   with
     Pre  => Has_Next (Self),
     Post =>
       Input_Unchanged (Self, Self'Old)
       and then Lexeme_Size (Self) = Lexeme_Size (Self'Old) + 1
       and then Remaining_Characters (Self) < Remaining_Characters (Self'Old);

   --  Used for ensuring that TIBs don't change between unrelated operations.
   --  Helpful to testing frame conditions of objects which might contain a TIB.
   overriding
   function "=" (A, B : Terminal_Input_Buffer) return Boolean;

private
   ---------------------------------------------------------------------------
   --  Private elements
   ---------------------------------------------------------------------------

   No_More_Characters : constant Character := Ada.Characters.Latin_1.NUL;

   -- Lexeme --

   type Lexeme_Range is record
      Lower : Range_Index := 1;
      Upper : Cursor_Index := 1;
   end record
   with
     Type_Invariant =>
       Cursor_Index (Lower) <= Upper
       and then Integer (Upper - Lower) <= Integer (Range_Size'Last);

   function Width (Self : Lexeme_Range) return Range_Size
   is (Range_Size (Self.Upper - Self.Lower));

   -- Terminal_Input_Buffer --

   type Terminal_Input_Buffer_State is (Empty, Ready, Complete);

   type Terminal_Input_Buffer is record
      Input  : String (1 .. Max_Input_Length) :=
        [others => No_More_Characters];
      Start  : Cursor_Index := 1;
      Cursor : Cursor_Index := 1;
      Length : Range_Size := 0;
   end record
   with
     Dynamic_Predicate =>
       Terminal_Input_Buffer.Start <= Terminal_Input_Buffer.Cursor
       and then Terminal_Input_Buffer.Cursor
                <= Cursor_Index (Terminal_Input_Buffer.Length + 1);

   overriding
   function "=" (A, B : Terminal_Input_Buffer) return Boolean
   is (A.Input = B.Input
       and then A.Start = B.Start
       and then A.Cursor = B.Cursor
       and then A.Length = B.Length);

   function End_Cursor_Index (Self : Terminal_Input_Buffer) return Cursor_Index
   is (Cursor_Index (Self.Length + 1));

   function Has_Valid_Cursor (Self : Terminal_Input_Buffer) return Boolean
   is (Self.Start <= Self.Cursor
       and then Self.Cursor <= End_Cursor_Index (Self));

   function Has_Input (Self : Terminal_Input_Buffer) return Boolean
   is (Self.Length > 0);

   function Has_More_Characters (Self : Terminal_Input_Buffer) return Boolean
   is (Self.Cursor < End_Cursor_Index (Self));

   function Has_Lexeme (Self : Terminal_Input_Buffer) return Boolean
   is (Self.Start < End_Cursor_Index (Self) and then Self.Start < Self.Cursor);

   function Lexeme_Size (Self : Terminal_Input_Buffer) return Range_Size
   is (Range_Size (Self.Cursor - Self.Start));

   function Remaining_Characters
     (Self : Terminal_Input_Buffer) return Range_Size
   is (Self.Length - Range_Size (Self.Cursor - 1));

   function Input_Size (Self : Terminal_Input_Buffer) return Range_Size
   is (Self.Length);

   function Input_Unchanged (A, B : Terminal_Input_Buffer) return Boolean
   is (A.Input = B.Input and then A.Length = B.Length);

   function Has_Next (Self : Terminal_Input_Buffer) return Boolean
   is (Self.Cursor in Range_Index and then Has_More_Characters (Self));

   function Peek (Self : Terminal_Input_Buffer) return Character
   is (if Has_Next (Self)
       then Self.Input (Integer (Self.Cursor))
       else No_More_Characters);

   procedure Skip_Whitespace
     (Self : in out Terminal_Input_Buffer; Skipped_Whitespace : out Boolean)
   with
     Pre  =>
       Has_Valid_Cursor (Self)
       and then Has_Input (Self)
       and then Has_Next (Self)
       and then Has_More_Characters (Self),
     Post =>
       Input_Unchanged (Self, Self'Old)
       and then (if Has_Next (Self) then not ACH.Is_Space (Peek (Self)))
       and then (if not Has_Next (Self)
                 then
                   Skipped_Whitespace and then not ACH.Is_Space (Peek (Self)))
       and then (if Skipped_Whitespace
                 then
                   Remaining_Characters (Self)
                   < Remaining_Characters (Self'Old))
       and then (if not Skipped_Whitespace
                 then
                   Remaining_Characters (Self)
                   = Remaining_Characters (Self'Old))
       and then (Lexeme_Size (Self) = 0);

   function Contains (Self : Terminal_Input_Buffer; Tk : Token) return Boolean
   is (Contains (Self, Tk.Lexeme));

   function Contains
     (Self : Terminal_Input_Buffer; Lexeme : Lexeme_Range) return Boolean
   is (Lexeme.Upper <= End_Cursor_Index (Self));

   procedure Take_Lexeme
     (Self : in out Terminal_Input_Buffer; Output : out Lexeme_Range)
   with
     Pre  => Has_Valid_Cursor (Self) and then Has_Lexeme (Self),
     Post =>
       Has_Valid_Cursor (Self)
       and then Input_Unchanged (Self, Self'Old)
       and then Lexeme_Size (Self) = 0
       and then Width (Output) = Lexeme_Size (Self'Old)
       and then Remaining_Characters (Self) = Remaining_Characters (Self'Old)
       and then Has_Next (Self) = Has_Next (Self'Old)
       and then Peek (Self) = Peek (Self'Old)
       and then Contains (Self, Output)
       and then (if (for all X in Self.Start'Old .. Self.Cursor'Old - 1 =>
                       not ACH.Is_Space (Self.Input'Old (Integer (X))))
                 then
                   (for all X in Output.Lower .. Output.Upper - 1 =>
                      not ACH.Is_Space (Self.Input (Integer (X)))));

   function Is_Word (Self : Terminal_Input_Buffer; Tk : Token) return Boolean
   is ((for all X in Tk.Lexeme.Lower .. Tk.Lexeme.Upper - 1 =>
          not ACH.Is_Space (Self.Input (Integer (X)))));

end Terminal_Input_Buffers;
