with GL.C;
with System;

package GL.Uniforms is

   use GL.C;

   subtype Address is System.Address;

   type Location is private;
   type Uniform_Name is new String;

   -- glUniform modifies the value of a uniform variable or a uniform variable array.
   -- The location of the uniform variable to be modified is specified by location,
   -- which should be a value returned by glGetUniformLocation.
   -- glUniform operates on the program object that was made part of current state by calling glUseProgram.
   -- glUniformMatrix4fv
   procedure Modify (Item : Location; Data : Address);

   -- glGetUniformLocation
   function Get (From : GLuint; Name : String) return Location;


   function Identity (Item : Location) return GLint;


   procedure Put_Line_Fancy (Item : Location);

private

   type Location is new GLint range 0 .. GLint'Last;

end;
