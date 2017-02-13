with GL.Uniforms;
with GL.Errors;

package GL.Programs.Uniforms is
   use GL.Errors;

   subtype Location is GL.Uniforms.Location;

   function Get (From : Program; Name : String) return Location with
     Post => Check_No_Error;

end;
