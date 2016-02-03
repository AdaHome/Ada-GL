package GL.Shaders.Programs is

   type Program (<>) is private;

   function Create return Program;
   procedure Link (Item : Program);
   procedure Attach (Item : Program; Accessory : Shader_Name);

private

   type Program is new GLuint range 1 .. GLuint'Last;

end;
