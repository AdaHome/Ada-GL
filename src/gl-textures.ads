with GL.C;
with GL.C.Complete;
with System;

package GL.Textures is

   use GL.C;
   use GL.C.Complete;
   use System;

   type Texture is private;
   type Texture_Target is (Texture_1D_Texture_Target, Texture_2D_Texture_Target);
   type Symbolic_Name is (Texture_Mag_Filter, Texture_Min_Filter, Texture_Wrap_S, Texture_Wrap_T);
   type Symbolic_Param is (Nearest_Param, Linear_Param, Clamp_Param, Repeat_Param);

   type Pixel_Format is (Red_Pixel_Format, Green_Pixel_Format, Blue_Pixel_Format);
   type Pixel_Type is (Byte_Pixel_Type, Unsigned_Byte_Pixel_Type);


   function Generate return Texture;
   procedure Bind (Target : Texture_Target; Texture_Obj : Texture);
   procedure Load (Target : Texture_Target; width : GLsizei; height : GLsizei; Format : Pixel_Format; Kind : Pixel_Type; Data : Address);

   procedure Set_Parameter (Target : Texture_Target; Name : Symbolic_Name; Param : Symbolic_Param);
   procedure Set_Pixel_Alignment (Bytes : GLint);

private



   type Texture is new GLuint;

   for Texture_Target'Size use GLenum'Size;
   for Pixel_Format'Size use GLenum'Size;
   for Pixel_Type'Size use GLenum'Size;
   for Symbolic_Name'Size use GLenum'Size;
   for Symbolic_Param'Size use GLenum'Size;

   for Texture_Target use
     (
      Texture_1D_Texture_Target => GL_TEXTURE_1D,
      Texture_2D_Texture_Target => GL_TEXTURE_2D
     );

   for Pixel_Format use
     (
      Red_Pixel_Format => GL_RED,
      Green_Pixel_Format => GL_GREEN,
      Blue_Pixel_Format => GL_BLUE
     );

   for Pixel_Type use
     (
      Byte_Pixel_Type => GL_BYTE,
      Unsigned_Byte_Pixel_Type => GL_UNSIGNED_BYTE
     );

   for Symbolic_Param use
     (
      Nearest_Param => GL_NEAREST,
      Linear_Param => GL_LINEAR,
      Clamp_Param => GL_CLAMP,
      Repeat_Param => GL_REPEAT
     );

   for Symbolic_Name use
     (
      Texture_Mag_Filter => GL_TEXTURE_MAG_FILTER,
      Texture_Min_Filter => GL_TEXTURE_MIN_FILTER,
      Texture_Wrap_S => GL_TEXTURE_WRAP_S,
      Texture_Wrap_T => GL_TEXTURE_WRAP_T
     );
end;
