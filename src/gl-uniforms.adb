with GL.C.Complete;
with Interfaces.C;
with Ada.Text_IO;


package body GL.Uniforms is

   function Identity (Item : Location) return GLint is
   begin
      return GLint (Item);
   end;

   procedure Modify_Matrix_4f (Item : Location; Data : Address) is
      use GL.C.Complete;
      use GL.C;
   begin
      glUniformMatrix4fv (GLint (Item), 1, GL_FALSE, Data);
   end;

   procedure Modify_1f (Item : Location; Data : GLFloat) is
      use GL.C.Complete;
      use GL.C;
   begin
      glUniform1f (GLint (Item), Data);
   end;

   procedure Modify_1i (Item : Location; Data : GLint) is
      use GL.C.Complete;
      use GL.C;
   begin
      glUniform1i (GLint (Item), Data);
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
