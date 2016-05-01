with GL.C.Complete;
with Interfaces.C;


package body GL.Programs.Uniforms is

   procedure Modify (L : Location; M : Matrix_4) is
      use GL.C.Complete;
      use GL.C;
   begin
      glUniformMatrix4fv (GLint (L), 1, GL_FALSE, M'Address);
   end;

   function Get (P : Program; Name : String) return Location is
      use GL.C;
      use GL.C.Complete;
      use Interfaces.C;
   begin
      return Location (glGetUniformLocation (GLuint (P), To_C (Name)));
   end;

end;
