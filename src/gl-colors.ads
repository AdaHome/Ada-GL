with GL.Math;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;

package GL.Colors is

   use GL.Math;
   subtype Color_Amount is Real_Float range 0.0 .. 1.0;

   function Random (G : Ada.Numerics.Float_Random.Generator) return Color_Amount is (Color_Amount (Ada.Numerics.Float_Random.Random (G)));




   package Names is
      type Name is (Red, Green, Blue, Yellow, Magenta, Cyan, Violet);
   end Names;

   package RGBA is
      type Index is (Red_Index, Green_Index, Blue_Index, Alpha_Index);
      type Vector is array (Index) of Color_Amount;
      Black_Vector : constant Vector := (0.0, 0.0, 0.0, 1.0);
      White_Vector : constant Vector := (1.0, 1.0, 1.0, 1.0);
      Red_Vector : constant Vector := (1.0, 0.0, 0.0, 1.0);
      Green_Vector : constant Vector := (0.0, 1.0, 0.0, 1.0);
      Blue_Vector : constant Vector := (0.0, 0.0, 1.0, 1.0);
      procedure Get (F : Ada.Text_IO.File_Type; Item : out Vector);
   end RGBA;

end;
