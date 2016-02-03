with Ada.Text_IO;


package body GL.Shaders.Programs.Files is


   procedure Attach (Item : Program; Kind : Shader_Type; Source : File_Name) is
      Accessory : Shader_Name;
   begin
      Accessory := Create (Kind, Source);
      Attach (Item, Accessory);
   end;



end;
