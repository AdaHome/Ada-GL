with GL.Shaders;

package GL.Programs.Shaders is

   subtype Shader is GL.Shaders.Shader;

   procedure Attach (To : Program; Attachment : Shader);

end;
