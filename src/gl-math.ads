with GL.C;

package GL.Math is

   use GL.C;

   subtype Dimension_1 is Integer range 1 .. 1;
   subtype Dimension_2 is Integer range 1 .. 2;
   subtype Dimension_3 is Integer range 1 .. 3;
   subtype Dimension_4 is Integer range 1 .. 4;

   type Vector_1 is array (Dimension_1) of GLfloat;
   type Vector_2 is array (Dimension_2) of GLfloat;
   type Vector_3 is array (Dimension_3) of GLfloat;
   type Vector_4 is array (Dimension_4) of GLfloat;

   type Matrix_1 is array (Dimension_1, Dimension_1) of GLfloat;
   type Matrix_2 is array (Dimension_2, Dimension_2) of GLfloat;
   type Matrix_3 is array (Dimension_3, Dimension_3) of GLfloat;
   type Matrix_4 is array (Dimension_4, Dimension_4) of GLfloat;

end;
