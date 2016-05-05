package body GL.Drawings is

   procedure Draw (Item : Mode; From : Natural; Count : Natural) is
   begin
      glDrawArrays (Item'Enum_Rep, GLint (From), GLsizei (Count));
   end;


   procedure Viewport (X, Y, Width, Height : Natural) is
   begin
      glViewport (GLint (X), GLint (Y), GLsizei (Width), GLsizei (Height));
   end;

end;
