with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;

package GL.Shaders.Files is

   type File_Name is new String;

   function Create (Kind : Shader_Type; Source : File_Name) return Shader_Name;

end;
