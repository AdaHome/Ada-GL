with Ada.Containers.Vectors;
with GL.C;

package GL.Errors is

   use GL.C;
   use type GL.C.GLenum;

   package GLenum_Vectors is new Ada.Containers.Vectors (Positive, GLenum);
   subtype GLenum_Vector is GLenum_Vectors.Vector;

   Error_Vector : GLenum_Vector;

   function Check_No_Error return Boolean;

end GL.Errors;
