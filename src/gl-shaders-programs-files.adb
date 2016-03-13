with Ada.Text_IO;


package body GL.Shaders.Programs.Files is


   procedure Attach_Checked_Source (Item : Program; Kind : Shader_Type; Source : File_Name) is
      Accessory : Shader_Name;
   begin
      Accessory := Create_Empty (Kind);
      Set_Source (Accessory, Source);
      Compile_Checked (Accessory);
      Attach (Item, Accessory);
   end;

   procedure Attach_Unchecked_Source (Item : Program; Kind : Shader_Type; Source : File_Name) is
      Accessory : Shader_Name;
   begin
      Accessory := Create_Empty (Kind);
      Set_Source (Accessory, Source);
      Compile_Unchecked (Accessory);
      Attach (Item, Accessory);
   end;


end;
