with GL.Shaders.Files;

package GL.Shaders.Programs.Files is

   use GL.Shaders.Files;

   procedure Attach_Checked_Source (Item : Program; Kind : Shader_Type; Source : File_Name);
   procedure Attach_Unchecked_Source (Item : Program; Kind : Shader_Type; Source : File_Name);

end;
