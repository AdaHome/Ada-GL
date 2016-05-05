package body GL.Programs.Uniforms is

   function Get (From : Program; Name : String) return Location is
   begin
      return GL.Uniforms.Get (Identity (From), Name);
   end;

end;
