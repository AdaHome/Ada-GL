with GL.C.Complete;
with GL.C.Initializations;
with Interfaces.C;

package body GL.Programs is

   use GL.C.Complete;
   use GL.C.Initializations;

   function Create_Empty return Program is
      P : Program;
   begin
      P := Program (glCreateProgram.all);
      return P;
   end;

   procedure Link (Item : Program) is
      use GL.C;
   begin
      glLinkProgram (GLuint (Item));
   end;

   procedure Set_Current (Item : Program) is
      use GL.C;
   begin
      glUseProgram (GLuint (Item));
   end;

   function Validate (Item : Program) return Boolean is
      use GL.C;
      use type GLboolean;
      B : GLboolean;
   begin
      B := glIsProgram (GLuint (Item));
      return B = GL_TRUE;
   end;



end;
