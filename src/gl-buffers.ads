with GL.C;
with GL.C.Complete;
with System;

package GL.Buffers is

   use GL.C;
   use GL.C.Complete;
   use System;

   type Buffer is private;
   type Buffer_Array is array (Integer range <>) of aliased Buffer;

   --  Array_Slot	        Vertex attributes
   --  GL_ATOMIC_COUNTER_BUFFER	Atomic counter storage
   --  GL_COPY_READ_BUFFER	Buffer copy source
   --  GL_COPY_WRITE_BUFFER	Buffer copy destination
   --  GL_DISPATCH_INDIRECT_BUFFER	Indirect compute dispatch commands
   --  GL_DRAW_INDIRECT_BUFFER	Indirect command arguments
   --  Element_Array_Slot	Vertex array indices
   --  GL_PIXEL_PACK_BUFFER	Pixel read target
   --  GL_PIXEL_UNPACK_BUFFER	Texture data source
   --  GL_QUERY_BUFFER	Query result buffer
   --  GL_SHADER_STORAGE_BUFFER	Read-write storage for shaders
   --  GL_TEXTURE_BUFFER	Texture data buffer
   --  GL_TRANSFORM_FEEDBACK_BUFFER	Transform feedback buffer
   --  GL_UNIFORM_BUFFER	Uniform block storage
   type Buffer_Slot is (Array_Slot, Element_Array_Slot);


   type Buffer_Usage is (Static_Usage, Dynamic_Usage);

   --  Depth_Plane : GL_COLOR_BUFFER_BIT
   --  Indicates the buffers currently enabled for color writing.
   --  Color_Plane : GL_DEPTH_BUFFER_BIT
   --  Indicates the depth buffer.
   --  GL_ACCUM_BUFFER_BIT
   --  Indicates the accumulation buffer.
   --  GL_STENCIL_BUFFER_BIT
   --  Indicates the stencil buffer.
   type Bitplane is (Depth_Plane, Color_Plane);--, Accumulation_Plane, Stencil_Plane);


   procedure Generate (Item : out Buffer_Array);
   function Generate return Buffer;
   procedure Bind (To : Buffer_Slot; Item : Buffer);


   -- glBufferData
   -- Create a new data store for a buffer object.
   -- Data that will be copied into the data store for initialization.
   procedure Allocate_Initialized_Bytes (Target : Buffer_Slot; Size_Bytes : Natural; Data : Address; Usage : Buffer_Usage);

   -- glBufferData
   -- Create a new data store for a buffer object.
   -- Data that will be copied into the data store for initialization.
   procedure Allocate_Initialized_Bits (Target : Buffer_Slot; Size_Bits : Natural; Data : Address; Usage : Buffer_Usage);

   -- glBufferData
   -- Create a new data store for a buffer object.
   procedure Allocate_Uninitialized_Bytes (Target : Buffer_Slot; Size_Bytes : Natural; Usage : Buffer_Usage);

   -- glBufferData
   -- Create a new data store for a buffer object.
   procedure Allocate_Uninitialized_Bits (Target : Buffer_Slot; Size_Bytes : Natural; Usage : Buffer_Usage);

   -- glGetBufferSubData
   -- glGetBufferSubData and glGetNamedBufferSubData return some or all of the data contents of the data store of the specified buffer object.
   -- Data starting at byte offset offset and extending for size bytes is copied from the buffer object's data store
   -- to the memory pointed to by data. An error is thrown if the buffer object is currently mapped,
   -- or if offset and size together define a range beyond the bounds of the buffer object's data store.
   procedure Redefine_Bytes (Target : Buffer_Slot; Offset_Byte : Natural; Size_Byte : Natural; Data : System.Address);

   -- glGetBufferSubData
   -- glGetBufferSubData and glGetNamedBufferSubData return some or all of the data contents of the data store of the specified buffer object.
   -- Data starting at byte offset offset and extending for size bytes is copied from the buffer object's data store
   -- to the memory pointed to by data. An error is thrown if the buffer object is currently mapped,
   -- or if offset and size together define a range beyond the bounds of the buffer object's data store.
   procedure Redefine_Bits (Target : Buffer_Slot; Offset_Bits : Natural; Size_Bits : Natural; Data : System.Address);


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
