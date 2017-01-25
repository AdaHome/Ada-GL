with GL.Vertex_Attributes;

package GL.Programs.Vertex_Attributes is

   -- glGetAttribLocation
   -- glGetAttribLocation queries the previously linked program object specified
   -- by program for the attribute variable specified by name and returns the index
   -- of the generic vertex attribute that is bound to that attribute variable.
   -- If name is a matrix attribute variable, the index of the first column of the matrix is returned.
   -- If the named attribute variable is not an active attribute in the specified
   -- program object or if name starts with the reserved prefix "gl_", a value of -1 is returned.
   function Get_Index_By_Name (From : Program; Name : String) return GL.Vertex_Attributes.Attribute;

end;
