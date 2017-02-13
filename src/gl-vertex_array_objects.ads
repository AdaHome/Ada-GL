with GL.C;
with GL.C.Complete;
with System;
with GL.Errors;

package GL.Vertex_Array_Objects is

   use GL.Errors;

   type Vertex_Array_Object is new GL.C.GLuint;


   -- Generic vertex attribute to be used.
   type Component_Attribute is new GL.C.GLuint;


   -- Specifies the data type of each component in the array.
   type Component_Kind is (Byte_Type, Unsigned_Byte_Type, Short_Type, Unsigned_Short_Type, Float_Type, Fixed_Type);

   type Component_Count is new GL.C.GLint;

   function Is_Vertex_Array_Object (Item : Vertex_Array_Object) return Boolean;


   --  glGenVertexArrays returns n vertex array object names in arrays.
   --  There is no guarantee that the names form a contiguous set of integers;
   --  however, it is guaranteed that none of the returned names was in use immediately before the call to glGenVertexArrays.
   --  Vertex array object names returned by a call to glGenVertexArrays are not returned by subsequent calls,
   --  unless they are first deleted with glDeleteVertexArrays.
   --  The names returned in arrays are marked as used, for the purposes of glGenVertexArrays only,
   --  but they acquire state and type only when they are first bound.
   function Generate_Attribute return Vertex_Array_Object;



   -- glCreateVertexArrays
   -- glCreateVertexArrays returns n previously unused vertex array object names in arrays,
   -- each representing a new vertex array object initialized to the default state.
   function Create_Attribute return Vertex_Array_Object with
     Post => Is_Vertex_Array_Object (Create_Attribute'Result) and Check_No_Error;



   -- glBindVertexArray binds the vertex array object with name array. array is the
   -- name of a vertex array object previously returned from a call to glGenVertexArrays,
   -- or zero to break the existing vertex array object binding.
   -- If no vertex array object with name array exists, one is created when array is first bound.
   -- If the bind is successful no change is made to the state of the vertex array object,
   -- and any previous vertex array object binding is broken.
   procedure Bind (Item : Vertex_Array_Object) with
     Pre => Is_Vertex_Array_Object (Item),
     Post => Check_No_Error;



   --glEnableVertexAttribArray enables the generic vertex attribute array specified by index.
   --glDisableVertexAttribArray disables the generic vertex attribute array specified by index.
   --By default, all client-side capabilities are disabled, including all generic vertex attribute arrays.
   --If enabled, the values in the generic vertex attribute array will be accessed and used for rendering when calls are made to vertex array commands such as glDrawArrays or glDrawElements.
   procedure Set_Attribute_Enable (Attribute : Component_Attribute);



   -- glEnableVertexAttribArray and glEnableVertexArrayAttrib enable the generic vertex attribute array specified by index.
   -- glEnableVertexAttribArray uses currently bound vertex array object for the operation,
   -- whereas glEnableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
   -- glDisableVertexAttribArray and glDisableVertexArrayAttrib disable the generic vertex attribute array specified by index.
   -- glDisableVertexAttribArray uses currently bound vertex array object for the operation,
   -- whereas glDisableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
   procedure Set_Attribute_Enable (Item : Vertex_Array_Object; Attribute : Component_Attribute) with
     Pre => Is_Vertex_Array_Object (Item);



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
   procedure Set_Attribute_Memory_Layout (Attribute : Component_Attribute; Count : Component_Count; Kind : Component_Kind; Normalized : Boolean; Stride_Bytes : Natural; Offset_Bytes : Natural);



   -- glVertexArrayAttribFormat
   -- glVertexAttribFormat, glVertexAttribIFormat and glVertexAttribLFormat,
   -- as well as glVertexArrayAttribFormat, glVertexArrayAttribIFormat and
   -- glVertexArrayAttribLFormat specify the organization of data in vertex arrays.
   -- The first three calls operate on the bound vertex array object, whereas the
   -- last three ones modify the state of a vertex array object with ID vaobj.
   -- attribindex specifies the index of the generic vertex attribute array whose data layout is being described,
   -- and must be less than the value of GL_MAX_VERTEX_ATTRIBS.
   procedure Set_Attribute_Memory_Layout (VAO : Vertex_Array_Object; Attribute : Component_Attribute; Count : Component_Count; Kind : Component_Kind; Normalized : Boolean; Offset_Bytes : Natural) with
     Pre => Is_Vertex_Array_Object (VAO);



   -- glGetAttribLocation
   -- glGetAttribLocation queries the previously linked program object specified
   -- by program for the attribute variable specified by name and returns the index
   -- of the generic vertex attribute that is bound to that attribute variable.
   -- If name is a matrix attribute variable, the index of the first column of the matrix is returned.
   -- If the named attribute variable is not an active attribute in the specified
   -- program object or if name starts with the reserved prefix "gl_", a value of -1 is returned.
   function Get_Attribute_By_Name (From_Program : GL.C.GLuint; Name : String) return Component_Attribute;



   procedure Put_Line_Fancy (Item : Vertex_Array_Object);



private


   for Component_Kind'Size use GL.C.GLenum'Size;
   for Component_Kind use
     (
      Byte_Type           => GL.C.Complete.GL_BYTE,
      Unsigned_Byte_Type  => GL.C.Complete.GL_UNSIGNED_BYTE,
      Short_Type          => GL.C.Complete.GL_SHORT,
      Unsigned_Short_Type => GL.C.Complete.GL_UNSIGNED_SHORT,
      Float_Type          => GL.C.Complete.GL_FLOAT,
      Fixed_Type          => GL.C.Complete.GL_FIXED
     );

end;
