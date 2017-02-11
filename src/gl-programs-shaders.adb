with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;


package body GL.Programs.Shaders is

   function Identity (Item : Shader) return GLuint is
   begin
      return GLuint (Item);
   end;

   function Create_Empty (Kind : Shader_Stage) return Shader is
      S : Shader;
   begin
      S := Shader (glCreateShader (Kind'Enum_Rep));
      return S;
   end;

   function Is_Shader (Item : Shader) return Boolean is
      use type GLboolean;
      B : GLboolean;
   begin
      B := glIsShader (GLuint (Item));
      return B = GL_TRUE;
   end;

   procedure Delete (Item : Shader) is
   begin
      glDeleteShader (GLuint (Item));
   end;


   procedure Attach (To : Program; Attachment : Shader) is
   begin
      glAttachShader (GLuint (To), GLuint (Attachment));
   end;




   procedure Get_Info (Item : Shader; P : Shader_Info; R : access GLint) is
   begin
      glGetShaderiv (GLuint (Item), P'Enum_Rep, R);
   end;

   function Compile_Succeess (Item : Shader) return Boolean is
      use type GLint;
      R : aliased GLint;
   begin
      Get_Info (Item, Compile_Info, R'Access);
      return R = GL_TRUE;
   end;

   function Get_Stage (Item : Shader) return Shader_Stage is
      use type GLint;
      Result : aliased GLint;
   begin
      Get_Info (Item, Stage_Info, Result'Access);
      return Shader_Stage'Enum_Val (Result);
   end;

   function Get_Source_Length (Item : Shader) return Natural is
      use type GLint;
      Result : aliased GLint;
   begin
      Get_Info (Item, Source_Length_Info, Result'Access);
      --Ada.Text_IO.Put_Line ("L: " & Result'Img);
      return Natural (Result);
   end;



   procedure Get_Compile_Log (Item : Shader; Message : out String; Count : out Natural) is
      use Interfaces.C;
      Length : aliased GLsizei := 0;
      -- Note: unnecessary double buffer.
      Text : aliased GLstring (1 .. Message'Length);
   begin
      glGetShaderInfoLog (GLuint (Item), Message'Length, Length, Text'Address);
      To_Ada (Text, String (Message), Count);
   end;

   function Get_Compile_Log (Item : Shader; Count : Natural := 1024) return String is
      Text : String (1 .. Count);
      Length : Natural := 0;
   begin
      Get_Compile_Log (Item, Text, Length);
      return Text (1 .. Length);
   end;

   procedure Set_Source (Item : Shader; Source : String) is
      use Interfaces.C;
      use Interfaces.C.Strings;
      C_Content : aliased char_array := To_C (String (Source));
      C_Content_Array : GLstringv (1 .. 1);
      C_Length_Array : GLintv (1 .. 1);
   begin
      -- Array of pointers to strings containing the source code to be loaded into the shader.
      C_Content_Array (1) := To_Chars_Ptr (C_Content'Unrestricted_Access);
      -- The null character is not counted as part of the string length.
      C_Length_Array (1) := Source'Length;
      glShaderSource (GLuint (Item), 1, C_Content_Array, C_Length_Array);
   end;


   procedure Compile (Item : Shader) is
   begin
      glCompileShader (GLuint (Item));
   end;


   procedure Compile_Checked (Item : Shader) is
      Log_Buffer : String (1 .. 1000);
      Log_Length : Natural;
   begin
      Compile (Item);
      if not Compile_Succeess (Item) then
         Get_Compile_Log (Item, Log_Buffer, Log_Length);
         raise Compile_Error with Log_Buffer (1 .. Log_Length);
      end if;
   end;

end;
