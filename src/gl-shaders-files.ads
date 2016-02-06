with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;

package GL.Shaders.Files is

   type File_Name is new String;

   procedure Set_Source (Name : Shader_Name; Source : File_Name);

end;
