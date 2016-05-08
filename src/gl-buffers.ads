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
   type Bitplane is (Depth_Plane, Color_Plane);--, Accumulation_Plane, Stencil_Plane);
   type Byte is new Natural;
   type Bit is new Natural;


   procedure Generate (Item : out Buffer_Array);
   function Generate return Buffer;
   procedure Bind (To : Buffer_Slot; Item : Buffer);

   procedure Allocate (Target : Buffer_Slot; Size : Byte; Data : Address; Usage : Buffer_Usage);
   procedure Allocate (Target : Buffer_Slot; Size : Byte; Usage : Buffer_Usage);
   procedure Allocate (Target : Buffer_Slot; Size : Bit; Data : Address; Usage : Buffer_Usage);
   procedure Allocate (Target : Buffer_Slot; Size : Bit; Usage : Buffer_Usage);
   procedure Redefine (Target : Buffer_Slot; Offset : Byte; Size : Byte; Data : Address);
   procedure Redefine (Target : Buffer_Slot; Offset : Bit; Size : Bit; Data : Address);

   procedure Put_Line_Fancy (Item : Buffer);

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
