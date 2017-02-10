with Ada.Text_IO;
with Ada.Exceptions;

package body GL.Math is

   procedure Get (F : Ada.Text_IO.File_Type; Item : out Real_Float_Vector) is
      use Ada.Text_IO;
      use GL.Math.GLfloat_IO;
   begin
      for E of Item loop
         Get (F, E);
      end loop;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Message (E));
   end;

end;
