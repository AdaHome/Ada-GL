with GL.C.Complete;
with Interfaces.C;
with Ada.Text_IO;


package body GL.Uniforms is

   function Identity (Item : Location) return GLint is
   begin
      return GLint (Item);
   end;

   procedure Modify (Item : Location; Data : Address) is
      use GL.C.Complete;
      use GL.C;
   begin
      glUniformMatrix4fv (GLint (Item), 1, GL_FALSE, Data);
   end;

   function Get (From : GLuint; Name : String) return Location is
      use GL.C.Complete;
      use Interfaces.C;
   begin
      return Location (glGetUniformLocation (From, To_C (Name)));
   end;



   procedure Put_Line_Fancy (Item : Location) is
      use Ada.Text_IO;
   begin
      Put_Line ("Uniform location: " & Item'Img);
   end;

end;
