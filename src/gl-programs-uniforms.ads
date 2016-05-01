with GL.C;
with Ada.Numerics.Real_Arrays;

package GL.Programs.Uniforms is

   type Location is private;

   type Matrix_4 is new Ada.Numerics.Real_Arrays.Real_Matrix;

   procedure Modify (L : Location; M : Matrix_4) with Pre => M'Length (1) = 4 and M'Length (2) = 4;
   function Get (P : Program; Name : String) return Location;

private

   type Location is new GL.C.GLint;

end;
