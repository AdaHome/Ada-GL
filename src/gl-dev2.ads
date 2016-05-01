with System;
with GL.Dev1;

package GL.Dev2 is


   subtype Program is GL.Dev1.Program;

   function Create_Program return Program;

end;
