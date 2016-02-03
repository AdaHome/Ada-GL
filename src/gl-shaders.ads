with GL.C;
with GL.C.Complete;

package GL.Shaders is

   use GL.C;
   use GL.C.Complete;

   type Shader_Type is (Fragment_Type, Vertex_Type, Geometry_Type, Tess_Evaluation_Type, Tess_Control_Type);
   type Shader_Info is (Type_Info, Delete_Info, Compile_Info, Log_Length_Info, Source_Length_Info);

   -- Specifies the handle of the shader object whose source code is to be replaced.
   type Shader_Name (<>) is private;

   type Compile_Log is new String;
   type Shading_Language is new String;
   type Fragment_Shading_Language is new Shading_Language;
   type Geometry_Shading_Language is new Shading_Language;
   type Tess_Evaluation_Shading_Language is new Shading_Language;
   type Tess_Control_Shading_Language is new Shading_Language;


   function Create_Empty (Kind : Shader_Type) return Shader_Name with
     Post => Validate (Create_Empty'Result);

   function Create (Kind : Shader_Type; Source : Shading_Language) return Shader_Name
     with
       Post => Validate (Create'Result) and Compile_Succeess (Create'Result);


   procedure Delete (Item : Shader_Name);

   -- ShaderSource sets the source code in shader.
   -- Any source code previously stored in the shader object is completely replaced.
   -- OpenGL copies the shader source code strings when glShaderSource is called,
   -- so an application may free its copy of the source code strings immediately after the function returns.
   procedure Set_Source (Item : Shader_Name; Source : Shading_Language) with
     Post => Get_Source_Length (Item) = Source'Length + 1; -- + 1 includes null terminator.

   procedure Compile_Unchecked (Item : Shader_Name);
   procedure Compile (Item : Shader_Name) with Post => Compile_Succeess (Item);

   function Get_Source_Length (Item : Shader_Name) return Natural;
   function Validate (Item : Shader_Name) return Boolean;
   function Compile_Succeess (Item : Shader_Name) return Boolean;
   procedure Get_Compile_Log (Item : Shader_Name; Message : out Compile_Log; Count : out Natural);
   function Get_Compile_Log (Item : Shader_Name; Count : Natural := 512) return Compile_Log;
   function Get_Type (Item : Shader_Name) return Shader_Type;


private

   type Shader_Name is new GLuint range 1 .. GLuint'Last;

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
