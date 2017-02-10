with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with GL.C;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package GL.Math is

   subtype Real_Float is GL.C.GLfloat;
   subtype Real_Double is GL.C.GLdouble;

   use type Real_Float;


   package GLfloat_IO is new Ada.Text_IO.Float_IO (Real_Float);
   package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Real_Float);

   subtype Dimension is Integer;
   subtype Dimension1 is Integer range 1 .. 1;
   subtype Dimension2 is Integer range 1 .. 2;
   subtype Dimension3 is Integer range 1 .. 3;
   subtype Dimension4 is Integer range 1 .. 4;

   type Real_Float_Vector is array (Dimension range <>) of Real_Float;
   type Real_Float_Vector1 is new Real_Float_Vector (Dimension1);
   type Real_Float_Vector2 is new Real_Float_Vector (Dimension2);
   type Real_Float_Vector3 is new Real_Float_Vector (Dimension3);
   type Real_Float_Vector4 is new Real_Float_Vector (Dimension4);

   type Real_Float_Matrix is array (Dimension range <>, Dimension range <>) of Real_Float;
   type Real_Float_Matrix1 is new Real_Float_Matrix (Dimension1, Dimension1);
   type Real_Float_Matrix2 is new Real_Float_Matrix (Dimension2, Dimension2);
   type Real_Float_Matrix3 is new Real_Float_Matrix (Dimension3, Dimension3);
   type Real_Float_Matrix4 is new Real_Float_Matrix (Dimension4, Dimension4);



   type Data_Kind is (Real_Float_Matrix4_Kind, Real_Float_Vector3_Kind, Real_Float_Vector4_Kind);


   procedure Get (F : Ada.Text_IO.File_Type; Item : out Real_Float_Vector);


end;
