with GL.C.Complete;

package body GL.Errors is


   function Check_No_Error return Boolean is
      use GL.C.Complete;
      use type GL.C.GLenum;
      E : GLenum;
   begin
      E := glGetError;
      if E = GL_NO_ERROR then
         return True;
      else
         Error_Vector.Append (E);
         return False;
      end if;
   end;

end GL.Errors;
