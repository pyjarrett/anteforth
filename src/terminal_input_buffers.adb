package body Terminal_Input_Buffers
  with SPARK_Mode => On
is
   function Char
     (Self : Terminal_Input_Buffer; Index : Integer) return Character
   is (Self.Input (Index))
   with Pre => Index in 1 .. Max_Input_Length;

   function Char
     (Self : Terminal_Input_Buffer; Index : Cursor_Index) return Character
   is (Self.Input (Integer (Index)))
   with Pre => Integer (Index) in 1 .. Max_Input_Length;

   procedure Load_Input (Self : in out Terminal_Input_Buffer; Input : String)
   is
   begin
      Self.Start := 1;
      Self.Cursor := 1;
      Self.Length := Input'Length;
      Self.Input (1 .. Input'Length) := Input;
   end Load_Input;

   procedure Ignore_Lexeme (Self : in out Terminal_Input_Buffer) is
   begin
      Self.Start := Self.Cursor;
   end Ignore_Lexeme;

   procedure Take_Lexeme
     (Self : in out Terminal_Input_Buffer; Output : out Lexeme_Range) is
   begin
      Output.Lower := Self.Start;
      Output.Upper := Self.Cursor;

      pragma Assert (Width (Output) = Range_Size (Self.Cursor - Self.Start));
      pragma Assert (Contains (Self, Output));

      Self.Start := Self.Cursor;
   end Take_Lexeme;

   procedure Next (Self : in out Terminal_Input_Buffer)
   with
     Refined_Post =>
       Input_Unchanged (Self, Self'Old)
       and then Lexeme_Size (Self) = Lexeme_Size (Self'Old) + 1
       and then Remaining_Characters (Self) < Remaining_Characters (Self'Old)
       and then Self.Start = Self.Start'Old
   is
   begin
      if Has_Next (Self) then
         Self.Cursor := Self.Cursor + 1;
      end if;
   end Next;

   procedure Next_Token (Self : in out Terminal_Input_Buffer; Tk : out Token)
   is
   begin
      Tk := (Kind => End_Of_Input, Lexeme => (Lower => 1, Upper => 1));

      declare
         Skipped_Whitespace : Boolean;
      begin
         Skip_Whitespace (Self, Skipped_Whitespace);
         if not Has_Next (Self) then
            pragma Assert (Skipped_Whitespace);
            return;
         end if;
      end;

      pragma Assert (not ACH.Is_Space (Peek (Self)));
      pragma Assert (Has_Next (Self));
      pragma Assert (Self.Start = Self.Cursor);
      pragma
        Assert
          (for all X in Self.Start .. Self.Cursor =>
             not ACH.Is_Space (Char (Self, X)));
      while Has_Next (Self) and then not ACH.Is_Space (Peek (Self)) loop
         pragma
           Loop_Invariant
             (Input_Unchanged (Self, Self'Loop_Entry)
                and then Remaining_Characters (Self)
                         <= Remaining_Characters (Self'Loop_Entry)
                and then (not ACH.Is_Space (Char (Self, Self.Cursor)))
                and then (for all X in Self.Input'Range =>
                            Char (Self, X) = Self.Input'Loop_Entry (X))
                and then Self.Start = Self.Start'Loop_Entry
                and then Integer (Self.Cursor) in Self.Input'Range
                and then (for all X in Self.Start'Loop_Entry .. Self.Cursor =>
                            not ACH.Is_Space (Char (Self, X))));
         pragma Loop_Variant (Decreases => Remaining_Characters (Self));
         Next (Self);
      end loop;

      pragma Assert (Integer (Self.Start) in Self.Input'Range);
      pragma Assert (Self.Cursor <= Cursor_Index (Self.Input'Last + 1));
      pragma
        Assert
          ((for all X in Self.Start .. Self.Cursor - 1 =>
              not ACH.Is_Space (Char (Self, X))));
      Tk.Kind := Word;
      Take_Lexeme (Self, Tk.Lexeme);

      pragma Assert (Contains (Self, Tk));
      pragma
        Assert
          ((for all X in Tk.Lexeme.Lower .. Tk.Lexeme.Upper - 1 =>
              not ACH.Is_Space (Char (Self, X))));
   end Next_Token;

   procedure Skip_Whitespace
     (Self : in out Terminal_Input_Buffer; Skipped_Whitespace : out Boolean) is
   begin
      Skipped_Whitespace := False;
      while Has_Next (Self) and then ACH.Is_Space (Peek (Self)) loop
         pragma
           Loop_Invariant
             (Has_Next (Self)
                and then Input_Unchanged (Self, Self'Loop_Entry)
                and then Remaining_Characters (Self) > 0
                and then Has_Valid_Cursor (Self)
                and then ((not Skipped_Whitespace
                           and then Remaining_Characters (Self)
                                    = Remaining_Characters (Self'Loop_Entry))
                          or else (Skipped_Whitespace
                                   and then Remaining_Characters (Self)
                                            < Remaining_Characters
                                                (Self'Loop_Entry))));
         pragma Loop_Variant (Decreases => Remaining_Characters (Self));
         Next (Self);
         Skipped_Whitespace := True;
      end loop;

      if Has_Next (Self) then
         pragma Assert (not ACH.Is_Space (Peek (Self)));
      end if;

      if Skipped_Whitespace then
         Self.Start := Self.Cursor;
      end if;

      Ignore_Lexeme (Self);
   end Skip_Whitespace;

   function Image (Tk : Token; S : Terminal_Input_Buffer) return String is
   begin
      return
        (if Tk.Lexeme.Lower = Tk.Lexeme.Upper
         then ""
         else
           S.Input
             (Integer (Tk.Lexeme.Lower) .. Integer (Tk.Lexeme.Upper - 1)));
   end Image;

   function Is_Number (Input : String) return Boolean is
      Start : Positive;
   begin
      if Input'Length >= 8 or else Input'Length = 0 then
         return False;
      end if;

      if Input = "0" then
         return True;
      end if;

      Start := Input'First;

      if Input (Start) in '+' | '-' | '0' then
         if Start = Positive'Last then
            return False;
         else
            Start := @ + 1;
         end if;
      end if;

      if Start not in Input'Range or else Input (Start) not in '1' .. '9' then
         return False;
      end if;

      if Start = Positive'Last then
         return False;
      else
         return
           (for all I in Start + 1 .. Input'Last => Input (I) in '0' .. '9');
      end if;
   end Is_Number;

   --  Can't get the precondition for Integer_64'Value to pass, so just get this
   --  working.
   function To_Number (Input : String) return Interfaces.Integer_64 is
      Place    : Interfaces.Integer_64 := 1;
      Value    : Interfaces.Integer_64 := 0;
      Largest  : constant := Integer'Last;
      Smallest : constant := Integer'First;
      Digit    : Interfaces.Integer_64;
      use type Interfaces.Integer_64;
   begin
      for I in reverse Input'First .. Input'Last loop
         pragma
           Loop_Invariant
             (Smallest <= Value
                and then Value <= Largest
                and then Place <= 1_000_000
                and then Place >= 1);
         if I = Input'First and then Input (Input'First) = '+' then
            null;  --  nothing to do
         elsif I = Input'First and then Input (Input'First) = '-' then
            Value := -Value;
         else
            Digit := Character'Pos (Input (I)) - Character'Pos ('0');
            if Digit in 0 .. 9
              and then Interfaces.Integer_64 (Interfaces.Integer_32'Last)
                       > Value - Place * Digit
            then
               Value := Value + Place * Digit;
            else
               exit;
            end if;
         end if;
         Place := Place * 10;
         exit when
           Value < Smallest or else Value > Largest or else Place > 1_000_000;
      end loop;
      return Value;
   end To_Number;
end Terminal_Input_Buffers;
