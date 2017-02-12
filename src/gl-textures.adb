with GL.C.Complete;
with Ada.Assertions;

package body GL.Textures is

   use Ada.Assertions;

   function Is_Texture (Texture_Obj : Texture) return Boolean is
      use type GL.C.GLboolean;
   begin
      return glIsTexture (GLuint (Texture_Obj)) = GL_TRUE;
   end;

   function Generate return Texture is
      use GL.C.Complete;
      tex : aliased GLuint;
   begin
      glGenTextures (1, tex'Access);
      return Texture (tex);
   end;

   function Create (Target : Texture_Target) return Texture is
      use GL.C.Complete;
      tex : aliased GLuint;
   begin
      glCreateTextures (Target'Enum_Rep, 1, tex'Access);

      Assert (Is_Texture (Texture (tex)), "Texture error 44");
      return Texture (tex);
   end;

   procedure Bind (Target : Texture_Target; Texture_Obj : Texture) is
   begin
      glBindTexture (Target'Enum_Rep, GLuint (Texture_Obj));
      Assert (Is_Texture (Texture_Obj), "Texture error 45");
   end;

   procedure Allocate (Texture_Object : Texture; Format : Internal_Pixel_Format; Width : Texels; Height : Texels) is
      Levels : constant GLsizei := 1;
   begin
      glTextureStorage2D (GLuint (Texture_Object), Levels, Format'Enum_Rep, Width, Height);
   end;

   procedure Load (T : Texture; xoffset : GLint; yoffset : GLint; width : GLsizei; height : GLsizei; Format : Pixel_Format; Kind : Pixel_Type; Data : Address) is
   begin
      glTextureSubImage2D (GLuint (T), 0, xoffset, yoffset, width, height, format'Enum_Rep, kind'Enum_Rep, data);
   end;

   procedure Load (Target : Texture_Target; width : GLsizei; height : GLsizei; Format : Pixel_Format; Kind : Pixel_Type; Data : Address) is
   begin
      glTexImage2D (Target'Enum_Rep, 0, Format'Enum_Rep, width, height, 0, Format'Enum_Rep, Kind'Enum_Rep, Data);
   end;

   procedure Set_Parameter (T : Texture; Name : Symbolic_Name; Param : Symbolic_Param) is
   begin
      glTextureParameteri (GLuint (T), Name'Enum_Rep, Param'Enum_Rep);
   end;

   procedure Set_Parameter (Target : Texture_Target; Name : Symbolic_Name; Param : Symbolic_Param) is
   begin
      glTexParameteri (Target'Enum_Rep, Name'Enum_Rep, Param'Enum_Rep);
   end;

   procedure Set_Pixel_Alignment (Bytes : GLint) is
   begin
      glPixelStorei (GL_UNPACK_ALIGNMENT, Bytes);
   end;

end;
