with GL.C;
with System;

package GL.Uniforms is

   use GL.C;

   subtype Address is System.Address;

   type Location is private;
   type Uniform_Name is new String;

   procedure Modify (L : Location; M : Address);
   function Get (P : GLuint; Name : String) return Location;
   function Identity (L : Location) return GLint;

private

   type Location is new GLint range 0 .. GLint'Last;

end;
