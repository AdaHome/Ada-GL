package body GL.Programs.Shaders is

   procedure Attach (To : Program; Attachment : Shader) is
   begin
      Attach (To, GL.Shaders.Identity (Attachment));
   end;

end;
