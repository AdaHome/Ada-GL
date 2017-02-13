with GL.C;
with GL.C.Complete;
with System;
with GL.Errors;

package GL.Buffers is

   pragma Assertion_Policy (Check);
   --pragma Assertion_Policy (Ignore);

   use GL.C;
   use GL.C.Complete;
   use GL.Errors;
   use System;


   -- Buffer object name
   type Buffer is new GLuint range 0 .. GLuint'Last;


   type Buffer_Array is array (Integer range <>) of aliased Buffer;


   --  Legacy
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


   -- Specifies the target to which the buffer object is bound.
   type Buffer_Usage is (Static_Usage, Dynamic_Usage);


   function Is_Buffer (Item : Buffer) return Boolean;

   -- glGenBuffers returns Item'Length buffer object names in buffers.
   -- There is no guarantee that the names form a contiguous set of integers;
   -- however, it is guaranteed that none of the returned names was in use
   -- immediately before the call to glGenBuffers.
   procedure Generate_Buffer (Item : out Buffer_Array);



   -- Generate one buffer object name.
   function Generate_Buffer return Buffer;


   -- glCreateBuffers returns n previously unused buffer names in buffers,
   -- each representing a new buffer object initialized as if it had been bound to an unspecified target.
   function Create_Buffer return Buffer with
     Post => Is_Buffer (Create_Buffer'Result) and Check_No_Error;


   -- glBindBuffer binds a buffer object to the specified buffer binding point.
   -- Calling glBindBuffer with target set to one of the accepted symbolic
   -- constants and buffer set to the name of a buffer object binds that buffer object name to the target.
   -- If no buffer object with name buffer exists, one is created with that name.
   -- When a buffer object is bound to a target, the previous binding for that target is automatically broken.
   procedure Bind (To : Buffer_Slot; Item : Buffer) with
     Post => Is_Buffer (Item) and Check_No_Error;


   procedure Create_New_Storage (B : Buffer; Size_Bytes : Natural; Data : Address; Usage : Buffer_Usage) with
     Post => Is_Buffer (B) and Check_No_Error;

   --procedure Create_New_Storage (B : Buffer; Size_Bytes : Natural; Usage : Buffer_Usage);
   procedure Redefine_Storage (B : Buffer; Offset_Byte : Natural; Size_Bytes : Natural; Data : Address) with
     Post => Is_Buffer (B) and Check_No_Error;

   function Identity (Item : Buffer) return GLuint;


   procedure Put_Line_Fancy (Item : Buffer);


private




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
