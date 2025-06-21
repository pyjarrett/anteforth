with Ada.Text_IO;

with Machines;
with Scanners;

procedure Anteforth with SPARK_Mode => Off is
   M : Machines.Machine;
   S : Scanners.Scanner;
begin
   Machines.Initialize (M);

   Ada.Text_IO.Put_Line ("Anteforth");
   Ada.Text_IO.Put_Line ("Operations: + - * / . negate dup dump reset");
   Ada.Text_IO.New_Line;

   loop
      <<REPL_START>>
      Ada.Text_IO.Put
        (Machines.Status (M)'Image
         & ": Stack:"
         & Machines.Stack_Size (M)'Image
         & " /"
         & Scanners.Max_Input_Length'Image
         & " > ");
      declare
         Input : constant String := Ada.Text_IO.Get_Line;
      begin
         exit when Input = "bye";

         if Input'Length > Scanners.Max_Input_Length then
            Ada.Text_IO.Put_Line
              ("Input line is too long:"
               & Input'Length'Image
               & ", max is"
               & Scanners.Max_Input_Length'Image);
            goto REPL_START;
         end if;

         Scanners.Load_Input (S, Input);
         while Scanners.Has_More_Characters (S) loop
            declare
               Tokens : constant Scanners.Token_Array := Scanners.Tokenize (S);
               use type Scanners.Token_Kind;
            begin
               for Tk of Tokens loop
                  if Tk.Kind = Scanners.Word then
                     declare
                        Lexeme    : constant String := Scanners.Image (Tk, S);
                        New_Value : Machines.Bounded_Value;
                        Op        : Machines.Word_Id;
                     begin
                        if Scanners.Is_Number (Lexeme) then
                           begin
                              New_Value :=
                                Machines.Bounded_Value'Value (Lexeme);
                              Machines.Push (M, New_Value);
                           exception
                              when Constraint_Error =>
                                 Ada.Text_IO.Put_Line
                                   ("Error in value:" & Lexeme);
                                 null;
                           end;
                        else
                           Op := Machines.To_Machine_Op (M, Lexeme);
                           Machines.Execute (M, Op);
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end loop;
      end;
   end loop;
end Anteforth;
