with GL.C;
with GL.C.Complete;
with System;

package GL.Buffers is

   use GL.C;
   use GL.C.Complete;
   use System;

   type Buffer is private;
   type Buffer_Array is array (Integer range <>) of aliased Buffer;
   type Buffer_Slot is (Array_Slot, Element_Array_Slot);
   type Buffer_Usage is (Static_Usage, Dynamic_Usage);

   --  Depth_Plane : GL_COLOR_BUFFER_BIT
   --  Indicates the buffers currently enabled for color writing.
   --
   --  Color_Plane : GL_DEPTH_BUFFER_BIT
   --  Indicates the depth buffer.
   --
   --  GL_ACCUM_BUFFER_BIT
   --  Indicates the accumulation buffer.
   --
   --  GL_STENCIL_BUFFER_BIT
   --  Indicates the stencil buffer.
   type Bitplane is (Depth_Plane, Color_Plane);--, Accumulation_Plane, Stencil_Plane);


   procedure Generate (Item : out Buffer_Array);
   function Generate return Buffer;
   procedure Bind (To : Buffer_Slot; Item : Buffer);

   procedure Allocate (Target : Buffer_Slot; Size : Byte_Unit; Data : Address; Usage : Buffer_Usage);
   procedure Allocate (Target : Buffer_Slot; Size : Byte_Unit; Usage : Buffer_Usage);
   procedure Allocate (Target : Buffer_Slot; Size : Bit_Unit; Data : Address; Usage : Buffer_Usage);
   procedure Allocate (Target : Buffer_Slot; Size : Bit_Unit; Usage : Buffer_Usage);
   procedure Redefine (Target : Buffer_Slot; Offset : Byte_Unit; Size : Byte_Unit; Data : Address);
   procedure Redefine (Target : Buffer_Slot; Offset : Bit_Unit; Size : Bit_Unit; Data : Address);

   procedure Put_Line_Fancy (Item : Buffer);

   -- glClear sets the bitplane area of the window to values previously selected by
   -- glClearColor, glClearIndex, glClearDepth, glClearStencil, and glClearAccum.
   -- Multiple color buffers can be cleared simultaneously by selecting more than one buffer at a time using glDrawBuffer.
   procedure Clear (Item : Bitplane);


   function Identity (Item : Buffer) return GLuint;

private

   type Buffer is new GLuint range 0 .. GLuint'Last;


   for Buffer_Slot'Size use GLuint'Size;
   for Buffer_Slot use
     (
      Array_Slot => GL_ARRAY_BUFFER,
      Element_Array_Slot => GL_ELEMENT_ARRAY_BUFFER
     );


   for Buffer_Usage'Size use GLenum'Size;
   for Buffer_Usage use
     (
      Static_Usage => GL_STATIC_DRAW,
      Dynamic_Usage => GL_DYNAMIC_DRAW
     );

   for Bitplane'Size use GLbitfield'Size;
   for Bitplane use
     (
      Depth_Plane => GL_DEPTH_BUFFER_BIT,
      Color_Plane => GL_COLOR_BUFFER_BIT
     );

end;
