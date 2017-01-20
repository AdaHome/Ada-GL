with GL.C;
with GL.C.Complete;

package GL.Drawings is

   use GL.C;
   use GL.C.Complete;

   type Mode is (Lines_Mode, Line_Strip_Mode, Triangles_Mode);

   procedure Draw (Item : Mode; From : Natural; Count : Natural);


   procedure Viewport (X, Y, Width, Height : Natural);

private


   for Mode'Size use GLenum'Size;
   for Mode use
     (
      Lines_Mode => GL_LINES,
      Line_Strip_Mode => GL_LINE_STRIP,
      Triangles_Mode => GL_TRIANGLES
     );

end;
