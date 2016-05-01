with GL.C.Complete;
with Interfaces.C;


package body GL.Uniforms is

   function Identity (L : Location) return GLint is
   begin
      return GLint (L);
   end;

   procedure Modify (L : Location; M : Address) is
      use GL.C.Complete;
      use GL.C;
   begin
      glUniformMatrix4fv (GLint (L), 1, GL_FALSE, M);
   end;

   function Get (P : GLuint; Name : String) return Location is
      use GL.C;
      use GL.C.Complete;
      use Interfaces.C;
   begin
      return Location (glGetUniformLocation (P, To_C (Name)));
   end;

end;
