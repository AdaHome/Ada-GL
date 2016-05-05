with GL.C;
with GL.C.Complete;

package GL.Drawings is

   use GL.C;
   use GL.C.Complete;

   type Mode is (Triangle_Mode);

   procedure Draw (Item : Mode; From : Natural; Count : Natural);


   procedure Viewport (X, Y, Width, Height : Natural);

private


   for Mode'Size use GLenum'Size;
   for Mode use
     (
      Triangle_Mode => GL_TRIANGLES
     );

end;
