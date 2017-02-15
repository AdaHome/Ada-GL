with Ada.Unchecked_Conversion;
with GL.C;
with GL.C.Accessories;
with GL.C.Complete;

package body GL.C.Initializations is

   use Ada;

   function Convert is new Unchecked_Conversion (Address, Accessories.glCreateProgram);
   function Convert is new Unchecked_Conversion (Address, Accessories.glDeleteProgram);
   function Convert is new Unchecked_Conversion (Address, Accessories.glDeleteShader);
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
   function Convert is new Unchecked_Conversion (Address, Accessories.glUniform1f);
   function Convert is new Unchecked_Conversion (Address, Accessories.glUniform1i);
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
   function Convert is new Unchecked_Conversion (Address, Accessories.glBufferSubData);
   function Convert is new Unchecked_Conversion (Address, Accessories.glDrawArrays);
   function Convert is new Unchecked_Conversion (Address, Accessories.glClear);

   function Convert is new Unchecked_Conversion (Address, Accessories.glPixelStorei);
   function Convert is new Unchecked_Conversion (Address, Accessories.glGenTextures);
   function Convert is new Unchecked_Conversion (Address, Accessories.glBindTexture);
   function Convert is new Unchecked_Conversion (Address, Accessories.glTexImage2D);
   function Convert is new Unchecked_Conversion (Address, Accessories.glTexParameteri);
   function Convert is new Unchecked_Conversion (Address, Accessories.glIsTexture);
   function Convert is new Unchecked_Conversion (Address, Accessories.glActiveTexture);


   -- OpenGL Version 4.5
   function Convert is new Unchecked_Conversion (Address, Accessories.glVertexArrayAttribBinding);
   function Convert is new Unchecked_Conversion (Address, Accessories.glVertexArrayAttribFormat);
   function Convert is new Unchecked_Conversion (Address, Accessories.glVertexArrayVertexBuffer);
   function Convert is new Unchecked_Conversion (Address, Accessories.glEnableVertexArrayAttrib);
   function Convert is new Unchecked_Conversion (Address, Accessories.glCreateVertexArrays);
   function Convert is new Unchecked_Conversion (Address, Accessories.glCreateBuffers);
   function Convert is new Unchecked_Conversion (Address, Accessories.glNamedBufferData);
   function Convert is new Unchecked_Conversion (Address, Accessories.glNamedBufferSubData);
   function Convert is new Unchecked_Conversion (Address, Accessories.glTextureParameteri);
   function Convert is new Unchecked_Conversion (Address, Accessories.glTextureSubImage2D);
   function Convert is new Unchecked_Conversion (Address, Accessories.glCreateTextures);
   function Convert is new Unchecked_Conversion (Address, Accessories.glTextureStorage2D);
   function Convert is new Unchecked_Conversion (Address, Accessories.glTextureSubImage3D);
   function Convert is new Unchecked_Conversion (Address, Accessories.glTextureStorage3D);



   procedure Initialize (Load : Loader) is
      use GL.C.Complete;
   begin
      glCreateProgram           := Convert (Load ("glCreateProgram"));
      glDeleteProgram           := Convert (Load ("glDeleteProgram"));
      glDeleteShader            := Convert (Load ("glDeleteShader"));
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
      glUniform1f               := Convert (Load ("glUniform1f"));
      glUniform1i := Convert (Load ("glUniform1i"));
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

      glPixelStorei   := Convert (Load ("glPixelStorei"));
      glGenTextures   := Convert (Load ("glGenTextures"));
      glBindTexture   := Convert (Load ("glBindTexture"));
      glTexImage2D    := Convert (Load ("glTexImage2D"));
      glTexParameteri := Convert (Load ("glTexParameteri"));
      glIsTexture     := Convert (Load ("glIsTexture"));
      glActiveTexture := Convert (Load ("glActiveTexture"));
      -- OpenGL Version 4.5
      glVertexArrayAttribBinding := Convert (Load ("glVertexArrayAttribBinding"));
      glVertexArrayAttribFormat  := Convert (Load ("glVertexArrayAttribFormat"));
      glVertexArrayVertexBuffer  := Convert (Load ("glVertexArrayVertexBuffer"));
      glEnableVertexArrayAttrib  := Convert (Load ("glEnableVertexArrayAttrib"));
      glCreateVertexArrays       := Convert (Load ("glCreateVertexArrays"));
      glCreateBuffers            := Convert (Load ("glCreateBuffers"));
      glNamedBufferData          := Convert (Load ("glNamedBufferData"));
      glNamedBufferSubData       := Convert (Load ("glNamedBufferSubData"));

      glTextureParameteri       := Convert (Load ("glTextureParameteri"));
      glTextureSubImage2D       := Convert (Load ("glTextureSubImage2D"));
      glTextureStorage2D        := Convert (Load ("glTextureStorage2D"));
      glCreateTextures          := Convert (Load ("glCreateTextures"));

      glTextureSubImage3D    := Convert (Load ("glTextureSubImage3D"));
      glTextureStorage3D    := Convert (Load ("glTextureStorage3D"));

   end;


end;
