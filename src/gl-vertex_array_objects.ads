with GL.C;
with GL.C.Complete;
with System;

package GL.Vertex_Array_Objects is

   type Vertex_Array_Object is new GL.C.GLuint;



   --  glGenVertexArrays returns n vertex array object names in arrays.
   --  There is no guarantee that the names form a contiguous set of integers;
   --  however, it is guaranteed that none of the returned names was in use immediately before the call to glGenVertexArrays.
   --  Vertex array object names returned by a call to glGenVertexArrays are not returned by subsequent calls,
   --  unless they are first deleted with glDeleteVertexArrays.
   --  The names returned in arrays are marked as used, for the purposes of glGenVertexArrays only,
   --  but they acquire state and type only when they are first bound.
   function Generate return Vertex_Array_Object;



   -- glCreateVertexArrays
   -- glCreateVertexArrays returns n previously unused vertex array object names in arrays,
   -- each representing a new vertex array object initialized to the default state.
   function Create return Vertex_Array_Object;


   procedure Bind (Item : Vertex_Array_Object);



   procedure Put_Line_Fancy (Item : Vertex_Array_Object);

end;
