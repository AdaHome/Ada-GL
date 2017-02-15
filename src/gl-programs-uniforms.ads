with GL.Errors;
with System;

package GL.Programs.Uniforms is

   pragma Assertion_Policy (Check);
   --pragma Assertion_Policy (Ignore);

   use GL.Errors;
   use GL.C;
   use GL.Errors;

   subtype Address is System.Address;

   type Location is private;
   type Uniform_Name is new String;



-- glGetUniformLocation
   function Get (From : Program; Name : String) return Location with
     Post => Check_No_Error;

   -- glGetUniformLocation
   function Get_Checked (From : Program; Name : String) return Location with
     Post => Check_No_Error;



   -- glUniform modifies the value of a uniform variable or a uniform variable array.
   -- The location of the uniform variable to be modified is specified by location,
   -- which should be a value returned by glGetUniformLocation.
   -- glUniform operates on the program object that was made part of current state by calling glUseProgram.
   -- glUniformMatrix4fv
   procedure Modify_Matrix_4f (Item : Location; Data : Address) with
     Post => Check_No_Error;

   procedure Modify_1f (Item : Location; Data : GLFloat) with
     Post => Check_No_Error;

   procedure Modify_1i (Item : Location; Data : GLint) with
     Post => Check_No_Error;


   function Identity (Item : Location) return GLint;


   procedure Put_Line_Fancy (Item : Location);

private

   type Location is new GLint range 0 .. GLint'Last;
end;
