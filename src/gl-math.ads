with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with GL.C;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package GL.Math is

   subtype GLfloat is GL.C.GLfloat;
   use type GLfloat;


   package GLfloat_IO is new Ada.Text_IO.Float_IO (GLfloat);
   package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (GLfloat);

   subtype Dimension is Integer;
   subtype Dimension1 is Integer range 1 .. 1;
   subtype Dimension2 is Integer range 1 .. 2;
   subtype Dimension3 is Integer range 1 .. 3;
   subtype Dimension4 is Integer range 1 .. 4;

   type Float_Vector is array (Dimension range <>) of GLfloat;
   subtype Float_Vector1 is Float_Vector (Dimension1);
   subtype Float_Vector2 is Float_Vector (Dimension2);
   subtype Float_Vector3 is Float_Vector (Dimension3);
   subtype Float_Vector4 is Float_Vector (Dimension4);

   type Float_Matrix is array (Dimension range <>, Dimension range <>) of GLfloat;
   subtype Float_Matrix1 is Float_Matrix (Dimension1, Dimension1);
   subtype Float_Matrix2 is Float_Matrix (Dimension2, Dimension2);
   subtype Float_Matrix3 is Float_Matrix (Dimension3, Dimension3);
   subtype Float_Matrix4 is Float_Matrix (Dimension4, Dimension4);



   type Data_Kind is (Float_Matrix4_Kind, Float_Vector3_Kind, Float_Vector4_Kind);


   procedure Get (F : Ada.Text_IO.File_Type; Item : out Float_Vector);


end;
