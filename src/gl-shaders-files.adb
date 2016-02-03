with Ada.Text_IO;


package body GL.Shaders.Files is


   function Create (Kind : Shader_Type; Source : File_Name) return Shader_Name is
      use Ada;
      use Ada.Directories;
      subtype Shading_Language_Bounded is Shading_Language (1 .. Natural (Size (String (Source))));
      package Shading_Language_Bounded_IO is new Direct_IO (Shading_Language_Bounded);
      use Shading_Language_Bounded_IO;
      File : File_Type;
      Buffer : Shading_Language_Bounded;
      Item : Shader_Name;
   begin
      Open  (File, In_File, String (Source));
      Read  (File, Buffer);
      Close (File);
      Item := Create (Kind, Buffer);
      return Item;
   end;



end;
