with GL.C;
with GL.C.Complete;
with System;

package GL.Buffers is

   use GL.C;
   use GL.C.Complete;

   type Buffer is private;
   type Buffer_Array is array (Integer range <>) of aliased Buffer;
   type Buffer_Slot is (Array_Slot, Element_Array_Slot);
   type Buffer_Usage is (Static_Usage, Dynamic_Usage);

   procedure Generate (Item : out Buffer_Array);
   function Generate return Buffer;
   procedure Bind (Item : Buffer; Slot : Buffer_Slot);
   procedure Allocate (Target : Buffer_Slot; Size : Natural; Data : System.Address; Usage : Buffer_Usage);
   procedure Allocate (Target : Buffer_Slot; Size : Natural; Usage : Buffer_Usage);
   procedure Redefine (Target : Buffer_Slot; Offset : Natural; Size : Natural; Data : System.Address);



private

   type Buffer is new GLuint;


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

end;
