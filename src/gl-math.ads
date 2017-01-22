with Ada.Numerics.Generic_Elementary_Functions;
with GL.C;

package GL.Math is

   use GL.C;

   package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (GLfloat);

   subtype Dimension is Integer;
   subtype Dimension_1 is Integer range 1 .. 1;
   subtype Dimension_2 is Integer range 1 .. 2;
   subtype Dimension_3 is Integer range 1 .. 3;
   subtype Dimension_4 is Integer range 1 .. 4;

   type Vector is array (Dimension range <>) of GLfloat;
   subtype Vector_1 is Vector (Dimension_1);
   subtype Vector_2 is Vector (Dimension_2);
   subtype Vector_3 is Vector (Dimension_3);
   subtype Vector_4 is Vector (Dimension_4);

   type Matrix is array (Dimension range <>, Dimension range <>) of GLfloat;
   subtype Matrix_1 is Matrix (Dimension_1, Dimension_1);
   subtype Matrix_2 is Matrix (Dimension_2, Dimension_2);
   subtype Matrix_3 is Matrix (Dimension_3, Dimension_3);
   subtype Matrix_4 is Matrix (Dimension_4, Dimension_4);

end;
