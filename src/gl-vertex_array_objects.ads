with GL.C;
with GL.C.Complete;
with System;

package GL.Vertex_Array_Objects is

   type Name is private;

   function Generate return Name;
   procedure Bind (Item : Name);
   procedure Put_Line_Fancy (Item : Name);

private

   type Name is new GL.C.GLuint;

end;
