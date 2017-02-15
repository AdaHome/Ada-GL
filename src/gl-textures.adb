with GL.C.Complete;
with Ada.Assertions;

package body GL.Textures is



   use Ada.Assertions;

   function Is_Texture (Name : Texture) return Boolean is
      use type GL.C.GLboolean;
   begin
      return glIsTexture (GLuint (Name)) = GL_TRUE;
   end;

   function Generate return Texture is
      use GL.C.Complete;
      tex : aliased GLuint;
   begin
      glGenTextures (1, tex'Access);
      return Texture (tex);
   end;

   function Create (Target : Targets.Target) return Texture is
      use GL.C.Complete;
      tex : aliased GLuint;
   begin
      glCreateTextures (Target'Enum_Rep, 1, tex'Access);
      return Texture (tex);
   end;

   procedure Bind (Target : Targets.Target; Name : Texture) is
   begin
      glBindTexture (Target'Enum_Rep, GLuint (Name));
   end;

   procedure Allocate (Name : Texture; Format : Internal_Pixel_Format; Width : Texels; Height : Texels) is
      Levels : constant GLsizei := 1;
   begin
      glTextureStorage2D (GLuint (Name), Levels, Format'Enum_Rep, Width, Height);
   end;

   procedure Allocate_3D (Name : Texture; Format : Internal_Pixel_Format; Width : Texels; Height : Texels; Depth : Texels) is
      Levels : constant GLsizei := 1;
   begin
      glTextureStorage3D (GLuint (Name), Levels, Format'Enum_Rep, Width, Height, Depth);
   end;

   procedure Load_3D (Name : Texture; xoffset : GLint; yoffset : GLint; zoffset : GLint; width : GLsizei; height : GLsizei; depth : GLsizei; Format : Pixel_Format; Kind : Pixel_Type; Data : Address) is
   begin
      glTextureSubImage3D (GLuint (Name), 0, xoffset, yoffset, zoffset, width, height, depth, format'Enum_Rep, kind'Enum_Rep, data);
   end;

   procedure Load (Name : Texture; xoffset : GLint; yoffset : GLint; width : GLsizei; height : GLsizei; Format : Pixel_Format; Kind : Pixel_Type; Data : Address) is
   begin
      glTextureSubImage2D (GLuint (Name), 0, xoffset, yoffset, width, height, format'Enum_Rep, kind'Enum_Rep, data);
   end;

   procedure Load (Target : Targets.Target; width : GLsizei; height : GLsizei; Format : Pixel_Format; Kind : Pixel_Type; Data : Address) is
   begin
      glTexImage2D (Target'Enum_Rep, 0, Format'Enum_Rep, width, height, 0, Format'Enum_Rep, Kind'Enum_Rep, Data);
   end;

   procedure Set_Parameter (Name : Texture; Symbol : Symbolic_Name; Param : Symbolic_Param) is
   begin
      glTextureParameteri (GLuint (Name), Symbol'Enum_Rep, Param'Enum_Rep);
   end;

   procedure Set_Parameter (Target : Targets.Target; Symbol : Symbolic_Name; Param : Symbolic_Param) is
   begin
      glTexParameteri (Target'Enum_Rep, Symbol'Enum_Rep, Param'Enum_Rep);
   end;

   procedure Set_Pack_Pixel_Alignment (Bytes : GLint) is
   begin
      glPixelStorei (GL_PACK_ALIGNMENT, Bytes);
   end;

   procedure Set_Unpack_Pixel_Alignment (Bytes : GLint) is
   begin
      glPixelStorei (GL_UNPACK_ALIGNMENT, Bytes);
   end;

end;
