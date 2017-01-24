with GL.C;
with GL.C.Complete;
with System;

package GL.Vertex_Attributes is

   use GL.C;
   use GL.C.Complete;


   type Index is private;
   type Component_Kind is (Byte_Type, Unsigned_Byte_Type, Short_Type, Unsigned_Short_Type, Float_Type, Fixed_Type);

   --glEnableVertexAttribArray enables the generic vertex attribute array specified by index.
   --glDisableVertexAttribArray disables the generic vertex attribute array specified by index.
   --By default, all client-side capabilities are disabled, including all generic vertex attribute arrays.
   --If enabled, the values in the generic vertex attribute array will be accessed and used for rendering when calls are made to vertex array commands such as glDrawArrays or glDrawElements.
   procedure Enable (Index_Item : Index);

   -- glVertexAttribPointer
   procedure Set_Memory_Layout (Index_Item : Index; Component_Count : Natural; Kind : Component_Kind; Normalized : Boolean; Stride_Bytes : Natural; Offset_Bytes : Natural);

   -- glGetAttribLocation
   function Get_Index_By_Name (From_Program : GLuint; Name : String) return Index;

   function Create_Index (Index_Item : Natural) return Index;


private

   type Index is new GLuint;


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
