with Ada.Text_IO;
with Ada.Streams.Stream_IO;

package body GL.Programs.Shaders.Files is


   function Size (Name : Shader_File_Name) return Shader_File_Size is
   begin
      return Ada.Directories.Size (String (Name));
   end;


   procedure Read (Name : Shader_File_Name; Content : out Shading_Language) is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Stream0 : Stream_Access;
   begin
      Open  (File, In_File, String (Name));
      Stream0 := Stream (File);
      Shading_Language'Read (Stream0, Content);
      Close (File);
   end;

end;
