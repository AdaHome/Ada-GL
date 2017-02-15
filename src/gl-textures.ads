with GL.C;
with GL.C.Complete;
with System;
with GL.Errors;

package GL.Textures is

   pragma Assertion_Policy (Check);
   --pragma Assertion_Policy (Ignore);

   use GL.Errors;
   use GL.C;
   use GL.C.Complete;
   use System;
   use type GL.C.GLenum;

   subtype Texels is GLsizei range 0 .. GLsizei'Last;

   type Texture is private;

   package Targets is
      type Target is (Texture_1D, Texture_3D, Texture_2D_Array);
   private
      for Target'Size use GLenum'Size;
      for Target use
        (
         Texture_1D => GL_TEXTURE_1D,
         Texture_3D => GL_TEXTURE_2D,
         Texture_2D_Array => GL_TEXTURE_2D_ARRAY
        );
   end;





   type Symbolic_Name is (Texture_Mag_Filter, Texture_Min_Filter, Texture_Wrap_S, Texture_Wrap_T);
   type Symbolic_Param is (Nearest_Param, Linear_Param, Clamp_Param, Repeat_Param, Clamp_To_Edge_Param);

   type Pixel_Format is (Red_Pixel_Format, RGB_Pixel_Format, RGBA_Pixel_Format);
   type Internal_Pixel_Format is (RGBA2_Internal_Pixel_Format, R8_Internal_Pixel_Format, R16_Internal_Pixel_Format);
   type Pixel_Type is (Byte_Pixel_Type, Unsigned_Byte_Pixel_Type);


   function Is_Texture (Name : Texture) return Boolean;

   function Generate return Texture;

   function Create (Target : Targets.Target) return Texture with
     Post => Is_Texture (Create'Result) and Check_No_Error;

   procedure Bind (Target : Targets.Target; Name : Texture) with
     Post => Is_Texture (Name) and Check_No_Error;

   --glTextureStorage2D
   procedure Allocate (Name : Texture; Format : Internal_Pixel_Format; Width : Texels; Height : Texels) with
     Pre => Is_Texture (Name),
     Post => Check_No_Error;

   procedure Allocate_3D (Name : Texture; Format : Internal_Pixel_Format; Width : Texels; Height : Texels; Depth : Texels) with
     Pre => Is_Texture (Name),
     Post => Check_No_Error;

   -- glTexStorage2D and glTextureStorage2D specify the storage requirements for
   -- all levels of a two-dimensional texture or one-dimensional texture array simultaneously.
   -- Once a texture is specified with this command,
   -- the format and dimensions of all levels become immutable unless it is a proxy texture.
   -- The contents of the image may still be modified, however, its storage requirements may not change.
   -- Such a texture is referred to as an immutable-format texture.
   -- Texturing maps a portion of a specified texture image onto each graphical primitive for which texturing is enabled.
   procedure Load (Name : Texture; xoffset : GLint; yoffset : GLint; width : GLsizei; height : GLsizei; Format : Pixel_Format; Kind : Pixel_Type; Data : Address) with
     Pre => Is_Texture (Name),
     Post => Check_No_Error;

   procedure Load_3D
     (
      Name : Texture;
      xoffset : GLint; yoffset : GLint; zoffset : GLint;
      width : GLsizei; height : GLsizei; depth : GLsizei; Format : Pixel_Format; Kind : Pixel_Type; Data : Address) with
     Pre => Is_Texture (Name),
     Post => Check_No_Error;

   procedure Load (Target : Targets.Target; width : GLsizei; height : GLsizei; Format : Pixel_Format; Kind : Pixel_Type; Data : Address) with
     Post => Check_No_Error;

   procedure Set_Parameter (Name : Texture; Symbol : Symbolic_Name; Param : Symbolic_Param) with
     Pre => Is_Texture (Name),
     Post => Check_No_Error;

   procedure Set_Parameter (Target : Targets.Target; Symbol : Symbolic_Name; Param : Symbolic_Param) with
     Post => Check_No_Error;

   procedure Set_Pack_Pixel_Alignment (Bytes : GLint) with
     Pre => Bytes in 1 | 2 | 4 | 8,
     Post => Check_No_Error;

   procedure Set_Unpack_Pixel_Alignment (Bytes : GLint) with
     Pre =>  Bytes in 1 | 2 | 4 | 8,
     Post => Check_No_Error;

private



   type Texture is new GLuint;

   for Pixel_Format'Size use GLenum'Size;
   for Pixel_Type'Size use GLenum'Size;
   for Symbolic_Name'Size use GLenum'Size;
   for Symbolic_Param'Size use GLenum'Size;
   for Internal_Pixel_Format'Size use GLenum'Size;



   for Pixel_Format use
     (
      Red_Pixel_Format => GL_RED,
      RGB_Pixel_Format => GL_RGB,
      RGBA_Pixel_Format => GL_RGBA
     );

   for Internal_Pixel_Format use
     (
      RGBA2_Internal_Pixel_Format => GL_RGBA2,
      R8_Internal_Pixel_Format => GL_R8,
      R16_Internal_Pixel_Format => GL_R16
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
      Repeat_Param => GL_REPEAT,
      Clamp_To_Edge_Param => GL_CLAMP_TO_EDGE
     );

   for Symbolic_Name use
     (
      Texture_Mag_Filter => GL_TEXTURE_MAG_FILTER,
      Texture_Min_Filter => GL_TEXTURE_MIN_FILTER,
      Texture_Wrap_S => GL_TEXTURE_WRAP_S,
      Texture_Wrap_T => GL_TEXTURE_WRAP_T
     );


end;
