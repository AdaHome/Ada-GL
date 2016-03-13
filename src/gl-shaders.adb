with Ada.Text_IO;
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;

package body GL.Shaders is


   use GL.C.Complete;


   function Create_Empty (Kind : Shader_Type) return Shader_Name is
      S : Shader_Name;
   begin
      S := Shader_Name (glCreateShader (Kind'Enum_Rep));
      return S;
   end;

   function Validate (Item : Shader_Name) return Boolean is
      use type GLboolean;
      B : GLboolean;
   begin
      B := glIsShader (GLuint (Item));
      return B = GL_TRUE;
   end;

   procedure Delete (Item : Shader_Name) is
   begin
      --glDeleteShader (GLuint (S));
      null;
   end;







   procedure Get_Info (Item : Shader_Name; P : Shader_Info; R : access GLint) is
   begin
      glGetShaderiv (GLuint (Item), P'Enum_Rep, R);
   end;

   function Compile_Succeess (Item : Shader_Name) return Boolean is
      use type GLint;
      R : aliased GLint;
   begin
      Get_Info (Item, Compile_Info, R'Access);
      return R = GL_TRUE;
   end;

   function Get_Type (Item : Shader_Name) return Shader_Type is
      use type GLint;
      Result : aliased GLint;
   begin
      Get_Info (Item, Type_Info, Result'Access);
      return Shader_Type'Enum_Val (Result);
   end;

   function Get_Source_Length (Item : Shader_Name) return Natural is
      use type GLint;
      Result : aliased GLint;
   begin
      Get_Info (Item, Source_Length_Info, Result'Access);
      Ada.Text_IO.Put_Line ("L: " & Result'Img);
      return Natural (Result);
   end;


   procedure Get_Compile_Log (Item : Shader_Name; Message : out Compile_Log; Count : out Natural) is
      use Interfaces.C;
      Length : aliased GLsizei := 0;
      Text : aliased GLstring (1 .. Message'Length);
   begin
      glGetShaderInfoLog (GLuint (Item), Message'Length, Length'Access, Text'Address);
      To_Ada (Text, String (Message), Count);
   end;

   function Get_Compile_Log (Item : Shader_Name; Count : Natural := 512) return Compile_Log is
      Text : Compile_Log (1 .. Count);
      Length : Natural := 0;
   begin
      Get_Compile_Log (Item, Text, Length);
      return Text (1 .. Length);
   end;




   procedure Set_Source (Item : Shader_Name; Source : Shading_Language) is
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


   procedure Compile_Unchecked (Item : Shader_Name) is
   begin
      glCompileShader (GLuint (Item));
   end;

   procedure Compile_Checked (Item : Shader_Name) is
   begin
      glCompileShader (GLuint (Item));
      if not Compile_Succeess (Item) then
         raise Compile_Error with String (Get_Compile_Log (Item));
      end if;
   end;


   procedure Compile_Unchecked_Source (Item : Shader_Name; Source : Shading_Language) is
   begin
      Set_Source (Item, Source);
      Compile_Unchecked_Source (Item, Source);
   end;

   procedure Compile_Checked_Source (Item : Shader_Name; Source : Shading_Language) is
   begin
      Set_Source (Item, Source);
      Compile_Checked_Source (Item, Source);
   end;


   function Create_Checked (Kind : Shader_Type; Source : Shading_Language) return Shader_Name is
      S : Shader_Name;
   begin
      S := Create_Empty (Kind);
      Compile_Checked_Source (S, Source);
      return S;
   end;

   function Create_Unchecked (Kind : Shader_Type; Source : Shading_Language) return Shader_Name is
      S : Shader_Name;
   begin
      S := Create_Empty (Kind);
      Compile_Unchecked_Source (S, Source);
      return S;
   end;

end;
