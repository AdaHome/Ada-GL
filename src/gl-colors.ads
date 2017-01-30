with GL.Math;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;

package GL.Colors is

   use GL.Math;
   subtype Color_Amount is GLfloat range 0.0 .. 1.0;

   function Random (G : Ada.Numerics.Float_Random.Generator) return Color_Amount is (Color_Amount (Ada.Numerics.Float_Random.Random (G)));

   package Colors_RGBA is
      type Color_Index is (Red_Index, Green_Index, Blue_Index, Alpha_Index);
      type Color_Amount_Vector is array (Color_Index) of Color_Amount;
      Black_Color_Amount_Vector : constant Color_Amount_Vector := (0.0, 0.0, 0.0, 1.0);
      White_Color_Amount_Vector : constant Color_Amount_Vector := (1.0, 1.0, 1.0, 1.0);
      Red_Color_Amount_Vector : constant Color_Amount_Vector := (1.0, 0.0, 0.0, 1.0);
      Green_Color_Amount_Vector : constant Color_Amount_Vector := (0.0, 1.0, 0.0, 1.0);
      Blue_Color_Amount_Vector : constant Color_Amount_Vector := (0.0, 0.0, 1.0, 1.0);
      procedure Get (F : Ada.Text_IO.File_Type; Item : out Color_Amount_Vector);
   end;

end;
