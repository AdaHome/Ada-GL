with Ada.Text_IO;


package body GL.Shaders.Files is



   procedure Set_Source (Name : Shader_Name; Source : File_Name) is
      use Ada;
      use Ada.Directories;
      subtype Shading_Language_Bounded is Shading_Language (1 .. Natural (Size (String (Source))));
      package Shading_Language_Bounded_IO is new Direct_IO (Shading_Language_Bounded);
      use Shading_Language_Bounded_IO;
      File : File_Type;
      Buffer : Shading_Language_Bounded;
   begin
      Open  (File, In_File, String (Source));
      Read  (File, Buffer);
      Close (File);
      Set_Source (Name, Buffer);
   end;



end;
