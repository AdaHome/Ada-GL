with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;

package GL.Programs.Shaders.Files is

   subtype Shader_File_Size is Ada.Directories.File_Size;
   type Shader_File_Name is new String;

   function Size (Name : Shader_File_Name) return Shader_File_Size;
   procedure Read (Name : Shader_File_Name; Content : out Shading_Language);

end;
