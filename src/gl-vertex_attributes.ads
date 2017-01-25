with GL.C;
with GL.C.Complete;
with System;

package GL.Vertex_Attributes is

   use GL.C;
   use GL.C.Complete;

   -- Generic vertex attribute to be used.
   type Attribute is new GLuint;


   -- Specifies the data type of each component in the array.
   type Component_Kind is (Byte_Type, Unsigned_Byte_Type, Short_Type, Unsigned_Short_Type, Float_Type, Fixed_Type);

   --glEnableVertexAttribArray enables the generic vertex attribute array specified by index.
   --glDisableVertexAttribArray disables the generic vertex attribute array specified by index.
   --By default, all client-side capabilities are disabled, including all generic vertex attribute arrays.
   --If enabled, the values in the generic vertex attribute array will be accessed and used for rendering when calls are made to vertex array commands such as glDrawArrays or glDrawElements.
   procedure Enable (Item : Attribute);


   -- glVertexAttribPointer
   -- glVertexAttribPointer specifies the location and data format of the array
   -- of generic vertex attributes at index index to use when rendering.
   -- size specifies the number of components per attribute and must be 1, 2, 3, or 4.
   -- type specifies the data type of each component,
   -- and stride specifies the byte stride from one attribute to the next,
   -- allowing vertices and attributes to be packed into a single array or stored in separate arrays.
   -- If set to GL_TRUE, normalized indicates that values stored in an integer format
   -- are to be mapped to the range [-1,1] (for signed values) or [0,1] (for unsigned values)
   -- when they are accessed and converted to floating point. Otherwise, values will be converted to
   -- floats directly without normalization.
   procedure Set_Memory_Layout (Item : Attribute; Component_Count : Natural; Kind : Component_Kind; Normalized : Boolean; Stride_Bytes : Natural; Offset_Bytes : Natural);


   -- glGetAttribLocation
   -- glGetAttribLocation queries the previously linked program object specified
   -- by program for the attribute variable specified by name and returns the index
   -- of the generic vertex attribute that is bound to that attribute variable.
   -- If name is a matrix attribute variable, the index of the first column of the matrix is returned.
   -- If the named attribute variable is not an active attribute in the specified
   -- program object or if name starts with the reserved prefix "gl_", a value of -1 is returned.
   function Get_Index_By_Name (From_Program : GLuint; Name : String) return Attribute;

private


   for Component_Kind'Size use GLenum'Size;
   for Component_Kind use
     (
      Byte_Type => GL_BYTE,
      Unsigned_Byte_Type => GL_UNSIGNED_BYTE,
      Short_Type => GL_SHORT,
      Unsigned_Short_Type => GL_UNSIGNED_SHORT,
      Float_Type => GL_FLOAT,
      Fixed_Type => GL_FIXED
     );


end;
