with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;
with Terminal_Input_Buffers;
with VMS.Builtins;
with GNAT.Traceback.Symbolic;

procedure Anteforth with SPARK_Mode => Off is
   procedure Run_REPL is
      V : VMS.VM;
   begin

      VMS.Builtins.Register_Builtins (V);

      Ada.Text_IO.Put_Line ("----------------------------------------------");
      Ada.Text_IO.Put_Line ("Available words: ");
      Ada.Text_IO.New_Line;
      VMS.Print_Words (V);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("bye to exit");
      Ada.Text_IO.Put_Line ("----------------------------------------------");

      loop
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put (" > ");
         declare
            Line : constant String := Ada.Text_IO.Get_Line;
         begin
            if Line'Length <= Terminal_Input_Buffers.Max_Input_Length then
               exit when Ada.Strings.Equal_Case_Insensitive (Line, "BYE");

               if Ada.Strings.Equal_Case_Insensitive (Line, "RESET") then
                  VMS.Execute (V, VMS.Reset);
               elsif VMS.Is_Running (V) then
                  VMS.Exec (V, Line);
               else
                  Ada.Text_IO.Put_Line ("Invalid word, VM is not running.");
               end if;

               if not VMS.Is_Running (V) then
                  Ada.Text_IO.New_Line;
                  VMS.Dump_VM (V);
                  Ada.Text_IO.New_Line;
               end if;
            else
               Ada.Text_IO.Put_Line ("Input line too long!");
            end if;
         end;
      end loop;
   end Run_REPL;

   procedure Run_File (File_Name : String) is
      V    : VMS.VM;
      File : Ada.Text_IO.File_Type;
   begin
      VMS.Builtins.Register_Builtins (V);

      Ada.Text_IO.Open
        (File => File, Mode => Ada.Text_IO.In_File, Name => File_Name);

      while not Ada.Text_IO.End_Of_File (File) and then VMS.Is_Running (V) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            if Line'Length <= Terminal_Input_Buffers.Max_Input_Length then
               exit when Line = "bye" or else Line = "BYE";
               VMS.Exec (V, Line);
               if not VMS.Is_Running (V) then
                  VMS.Dump_VM (V);
               end if;
            else
               Ada.Text_IO.Put_Line ("Input line too long!");
            end if;
         end;
      end loop;
      Ada.Text_IO.Close (File);
   exception
      when Err : others =>
         Ada.Text_IO.Put_Line ("Unable to open file: """ & File_Name & """");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Err));
         Ada.Text_IO.Put_Line
           ("Exception traceback: "
            & GNAT.Traceback.Symbolic.Symbolic_Traceback (Err));
   end Run_File;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Run_REPL;
   elsif Ada.Command_Line.Argument_Count = 1 then
      Run_File (Ada.Command_Line.Argument (1));
   else
      Ada.Text_IO.Put_Line ("Usage: ");
      Ada.Text_IO.Put_Line ("REPL:       " & Ada.Command_Line.Command_Name);
      Ada.Text_IO.Put_Line
        ("Run a file: " & Ada.Command_Line.Command_Name & " [script_file]");
   end if;
end Anteforth;
