with GL.C;
with GL.C.Complete;

package GL.Programs.Shaders is

   use GL.C;
   use GL.C.Complete;

   type Shader_Type is (Fragment_Type, Vertex_Type, Geometry_Type, Tess_Evaluation_Type, Tess_Control_Type);
   type Shader_Info is (Type_Info, Delete_Info, Compile_Info, Log_Length_Info, Source_Length_Info);

   -- Specifies the handle of the shader object whose source code is to be replaced.
   type Shader (<>) is private;

   type Compile_Log is new String;
   type Shading_Language is new String;
   type Fragment_Shading_Language is new Shading_Language;
   type Geometry_Shading_Language is new Shading_Language;
   type Tess_Evaluation_Shading_Language is new Shading_Language;
   type Tess_Control_Shading_Language is new Shading_Language;

   Compile_Error : exception;

   function Create_Empty (Kind : Shader_Type) return Shader;

   procedure Delete (Item : Shader);

   -- ShaderSource sets the source code in shader.
   -- Any source code previously stored in the shader object is completely replaced.
   -- OpenGL copies the shader source code strings when glShaderSource is called,
   -- so an application may free its copy of the source code strings immediately after the function returns.
   procedure Set_Source (Item : Shader; Source : Shading_Language);

   procedure Compile (Item : Shader);
   procedure Attach (Item : Program; S : Shader);

   function Validate (Item : Shader) return Boolean;
   function Compile_Succeess (Item : Shader) return Boolean;
   procedure Get_Compile_Log (Item : Shader; Message : out Compile_Log; Count : out Natural);
   function Get_Source_Length (Item : Shader) return Natural;
   function Get_Compile_Log (Item : Shader; Count : Natural := 512) return Compile_Log;
   function Get_Type (Item : Shader) return Shader_Type;


private

   type Shader is new GLuint range 1 .. GLuint'Last;

   for Shader_Type'Size use GLenum'Size;
   for Shader_Info'Size use GLenum'Size;

   for Shader_Type use
     (
      Fragment_Type        => 16#8B30#,
      Vertex_Type          => 16#8B31#,
      Geometry_Type        => 16#8DD9#,
      Tess_Evaluation_Type => 16#8E87#,
      Tess_Control_Type    => 16#8E88#
     );

   for Shader_Info use
     (
      Type_Info => GL_SHADER_TYPE,
      Delete_Info => GL_DELETE_STATUS,
      Compile_Info => GL_COMPILE_STATUS,
      Log_Length_Info => GL_INFO_LOG_LENGTH,
      Source_Length_Info => GL_SHADER_SOURCE_LENGTH
     );




end;
