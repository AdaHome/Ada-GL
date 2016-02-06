with Ada.Text_IO;


package body GL.Shaders.Programs.Files is


   procedure Attach_Source (Item : Program; Kind : Shader_Type; Source : File_Name) is
      Accessory : Shader_Name;
   begin
      Accessory := Create_Empty (Kind);
      Set_Source (Accessory, Source);
      Attach (Item, Accessory);
   end;



end;
