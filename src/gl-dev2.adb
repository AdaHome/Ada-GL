package body GL.Dev2 is

   function Create_Program return Program is
      P : GL.Dev1.Create_Program;
   begin
      return P.all;
   end;

end;
