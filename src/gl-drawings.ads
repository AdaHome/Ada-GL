with GL.C;
with GL.C.Complete;
with GL.Errors;

package GL.Drawings is

   use GL.C;
   use GL.C.Complete;
   use GL.Errors;

   type Mode is (Lines_Mode, Line_Strip_Mode, Triangles_Mode);

   --  Depth_Plane : GL_COLOR_BUFFER_BIT
   --  Indicates the buffers currently enabled for color writing.
   --  Color_Plane : GL_DEPTH_BUFFER_BIT
   --  Indicates the depth buffer.
   --  GL_ACCUM_BUFFER_BIT
   --  Indicates the accumulation buffer.
   --  GL_STENCIL_BUFFER_BIT
   --  Indicates the stencil buffer.
   type Bitplane is (Depth_Plane, Color_Plane);


   -- glDrawArrays
   procedure Draw (Item : Mode; From : Natural; Count : Natural) with
     Post => Check_No_Error;


   procedure Viewport (X, Y, Width, Height : Natural) with
     Post => Check_No_Error;


   -- glClear sets the bitplane area of the window to values previously selected by
   -- glClearColor, glClearIndex, glClearDepth, glClearStencil, and glClearAccum.
   -- Multiple color buffers can be cleared simultaneously by selecting more than one buffer at a time using glDrawBuffer.
   procedure Clear (Item : Bitplane);


private


   for Bitplane'Size use GLbitfield'Size;
   for Bitplane use
     (
      Depth_Plane => GL_DEPTH_BUFFER_BIT,
      Color_Plane => GL_COLOR_BUFFER_BIT
     );


   for Mode'Size use GLenum'Size;
   for Mode use
     (
      Lines_Mode => GL_LINES,
      Line_Strip_Mode => GL_LINE_STRIP,
      Triangles_Mode => GL_TRIANGLES
     );

end;
