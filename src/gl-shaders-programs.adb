with GL.C.Complete;

package body GL.Shaders.Programs is

   use GL.C.Complete;

   function Create return Program is
      P : Program;
   begin
      P := Program (glCreateProgram.all);
      return P;
   end;

   procedure Link (Item : Program) is
   begin
      glLinkProgram (GLuint (Item));
   end;

   procedure Attach (Item : Program; Accessory : Shader_Name) is
   begin
      glAttachShader (GLuint (Item), GLuint (Accessory));
   end;

end;
