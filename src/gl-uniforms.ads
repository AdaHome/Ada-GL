with GL.C;
with System;

package GL.Uniforms is

   use GL.C;

   subtype Address is System.Address;

   type Location is private;
   type Uniform_Name is new String;

   procedure Modify (Item : Location; Data : Address);
   function Get (From : GLuint; Name : String) return Location;
   function Identity (Item : Location) return GLint;
   procedure Put_Line_Fancy (Item : Location);

private

   type Location is new GLint range 0 .. GLint'Last;

end;
