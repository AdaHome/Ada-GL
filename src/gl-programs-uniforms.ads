with GL.Uniforms;

package GL.Programs.Uniforms is

   subtype Location is GL.Uniforms.Location;

   function Get (From : Program; Name : String) return Location;

end;
