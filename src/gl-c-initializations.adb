with Ada.Unchecked_Conversion;
with GL.C;
with GL.C.Accessories;
with GL.C.Complete;

package body GL.C.Initializations is

   use Ada;

   function Convert is new Unchecked_Conversion (Address, Accessories.glCreateProgram);
   function Convert is new Unchecked_Conversion (Address, Accessories.glCreateShader);
   function Convert is new Unchecked_Conversion (Address, Accessories.glAttachShader);
   function Convert is new Unchecked_Conversion (Address, Accessories.glLinkProgram);
   function Convert is new Unchecked_Conversion (Address, Accessories.glCompileShader);
   function Convert is new Unchecked_Conversion (Address, Accessories.glShaderSource);
   function Convert is new Unchecked_Conversion (Address, Accessories.glGetShaderInfoLog);
   function Convert is new Unchecked_Conversion (Address, Accessories.glGetShaderiv);
   function Convert is new Unchecked_Conversion (Address, Accessories.glGetProgramInfoLog);
   function Convert is new Unchecked_Conversion (Address, Accessories.glGetProgramiv);
   function Convert is new Unchecked_Conversion (Address, Accessories.glUniformMatrix4fv);
   function Convert is new Unchecked_Conversion (Address, Accessories.glGetUniformLocation);
   function Convert is new Unchecked_Conversion (Address, Accessories.glGenVertexArrays);
   function Convert is new Unchecked_Conversion (Address, Accessories.glGenBuffers);
   function Convert is new Unchecked_Conversion (Address, Accessories.glBindVertexArray);
   function Convert is new Unchecked_Conversion (Address, Accessories.glBindBuffer);
   function Convert is new Unchecked_Conversion (Address, Accessories.glBufferData);
   function Convert is new Unchecked_Conversion (Address, Accessories.glVertexAttribPointer);
   function Convert is new Unchecked_Conversion (Address, Accessories.glEnableVertexAttribArray);
   function Convert is new Unchecked_Conversion (Address, Accessories.glUseProgram);
   function Convert is new Unchecked_Conversion (Address, Accessories.glIsShader);
   function Convert is new Unchecked_Conversion (Address, Accessories.glIsProgram);
   function Convert is new Unchecked_Conversion (Address, Accessories.glIsBuffer);
   function Convert is new Unchecked_Conversion (Address, Accessories.glIsVertexArray);
   function Convert is new Unchecked_Conversion (Address, Accessories.glGetAttribLocation);
   function Convert is new Unchecked_Conversion (Address, Accessories.glVertexArrayAttribFormat);
   function Convert is new Unchecked_Conversion (Address, Accessories.glBufferSubData);


   procedure Initialize (Load : Loader) is
      use GL.C.Complete;
   begin
      glCreateProgram           := Convert (Load ("glCreateProgram"));
      glCreateShader            := Convert (Load ("glCreateShader"));
      glAttachShader            := Convert (Load ("glAttachShader"));
      glLinkProgram             := Convert (Load ("glLinkProgram"));
      glCompileShader           := Convert (Load ("glCompileShader"));
      glShaderSource            := Convert (Load ("glShaderSource"));
      glGetShaderInfoLog        := Convert (Load ("glGetShaderInfoLog"));
      glGetShaderiv             := Convert (Load ("glGetShaderiv"));
      glGetProgramInfoLog       := Convert (Load ("glGetProgramInfoLog"));
      glGetProgramiv            := Convert (Load ("glGetProgramiv"));
      glUniformMatrix4fv        := Convert (Load ("glUniformMatrix4fv"));
      glGetUniformLocation      := Convert (Load ("glGetUniformLocation"));
      glGenVertexArrays         := Convert (Load ("glGenVertexArrays"));
      glGenBuffers              := Convert (Load ("glGenBuffers"));
      glBindVertexArray         := Convert (Load ("glBindVertexArray"));
      glBindBuffer              := Convert (Load ("glBindBuffer"));
      glBufferData              := Convert (Load ("glBufferData"));
      glBufferSubData           := Convert (Load ("glBufferSubData"));
      glVertexAttribPointer     := Convert (Load ("glVertexAttribPointer"));
      glEnableVertexAttribArray := Convert (Load ("glEnableVertexAttribArray"));
      glUseProgram              := Convert (Load ("glUseProgram"));
      glIsShader                := Convert (Load ("glIsShader"));
      glIsProgram               := Convert (Load ("glIsProgram"));
      glIsBuffer                := Convert (Load ("glIsBuffer"));
      glIsVertexArray           := Convert (Load ("glIsVertexArray"));
      glGetAttribLocation       := Convert (Load ("glGetAttribLocation"));
   end;

end;
