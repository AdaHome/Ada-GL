with Ada.Exceptions;

package body GL.Colors is

   package body RGBA is
      procedure Get (F : Ada.Text_IO.File_Type; Item : out Vector) is
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

end;
