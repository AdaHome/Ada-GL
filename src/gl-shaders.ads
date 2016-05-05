with GL.C;
with GL.C.Complete;

package GL.Shaders is

   use GL.C;
   use GL.C.Complete;

   type Shader_Stage is (Fragment_Stage, Vertex_Stage, Geometry_Stage, Tess_Evaluation_Stage, Tess_Control_Stage);
   type Shader_Info is (Stage_Info, Delete_Info, Compile_Info, Log_Length_Info, Source_Length_Info);

   -- Specifies the handle of the shader object whose source code is to be replaced.
   type Shader (<>) is private;

   function Identity (Item : Shader) return GLuint;
   function Create_Empty (Kind : Shader_Stage) return Shader;
   procedure Delete (Item : Shader);

   -- ShaderSource sets the source code in shader.
   -- Any source code previously stored in the shader object is completely replaced.
   -- OpenGL copies the shader source code strings when glShaderSource is called,
   -- so an application may free its copy of the source code strings immediately after the function returns.
   procedure Set_Source (Item : Shader; Source : String);

   procedure Compile (Item : Shader);
   procedure Compile_Checked (Item : Shader);

   function Is_Shader (Item : Shader) return Boolean;
   function Compile_Succeess (Item : Shader) return Boolean;
   procedure Get_Compile_Log (Item : Shader; Message : out String; Count : out Natural);
   function Get_Source_Length (Item : Shader) return Natural;
   function Get_Compile_Log (Item : Shader; Count : Natural := 1024) return String;
   function Get_Stage (Item : Shader) return Shader_Stage;

   Compile_Error : exception;

private

   type Shader is new GLuint range 1 .. GLuint'Last;

   for Shader_Stage'Size use GLenum'Size;
   for Shader_Info'Size use GLenum'Size;

   for Shader_Stage use
     (
      Fragment_Stage        => 16#8B30#,
      Vertex_Stage          => 16#8B31#,
      Geometry_Stage        => 16#8DD9#,
      Tess_Evaluation_Stage => 16#8E87#,
      Tess_Control_Stage    => 16#8E88#
     );

   for Shader_Info use
     (
      Stage_Info => GL_SHADER_TYPE,
      Delete_Info => GL_DELETE_STATUS,
      Compile_Info => GL_COMPILE_STATUS,
      Log_Length_Info => GL_INFO_LOG_LENGTH,
      Source_Length_Info => GL_SHADER_SOURCE_LENGTH
     );

end;
