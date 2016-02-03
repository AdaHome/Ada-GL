with GL.C.Accessories;


package GL.C.Complete is

   GL_TRUE                 : constant := 1;
   GL_FALSE                : constant := 0;
   GL_FRAGMENT_SHADER      : constant := 16#8B30#;
   GL_VERTEX_SHADER        : constant := 16#8B31#;
   GL_GEOMETRY_SHADER      : constant := 16#8DD9#;


   GL_SHADER_TYPE          : constant := 16#8B4F#;
   GL_DELETE_STATUS        : constant := 16#8B80#;
   GL_COMPILE_STATUS       : constant := 16#8B81#;
   GL_INFO_LOG_LENGTH      : constant := 16#8B84#;
   GL_SHADER_SOURCE_LENGTH : constant := 16#8B88#;



   GL_LINK_STATUS          : constant := 16#8B82#;
   GL_ARRAY_BUFFER         : constant := 16#8892#;
   GL_ELEMENT_ARRAY_BUFFER : constant := 16#8893#;
   GL_STATIC_DRAW          : constant := 16#88E4#;
   GL_FLOAT                : constant := 16#1406#;
   GL_TRIANGLES            : constant := 16#0004#;
   GL_ACTIVE_ATTRIBUTES    : constant := 16#8B89#;
   GL_MAJOR_VERSION        : constant := 16#821B#;
   GL_COLOR_BUFFER_BIT     : constant := 16#00004000#;
   GL_DEPTH_BUFFER_BIT     : constant := 16#00000100#;
   GL_DEPTH_TEST           : constant := 16#0B71#;




   procedure glDrawArrays (mode : GLenum; first : GLint; count : GLsizei) with
     Import,
     Convention => StdCall,
     External_Name => "glDrawArrays";

   function glGetError return GLenum with
     Import,
     Convention => StdCall,
     External_Name => "glGetError";

   procedure glGetIntegerv (pname : GLenum; data : access GLint) with
     Import,
     Convention => StdCall,
     External_Name => "glGetIntegerv";

   procedure glClear (Bits : GLbitfield) with
     Import,
     Convention => StdCall,
     External_Name => "glClear";

   procedure glEnable (cap : GLenum) with
     Import,
     Convention => StdCall,
     External_Name => "glEnable";

   procedure glDisable (cap : GLenum) with
     Import,
     Convention => StdCall,
     External_Name => "glDisable";


   glCreateProgram           : Accessories.glCreateProgram;
   glCreateShader            : Accessories.glCreateShader;
   glAttachShader            : Accessories.glAttachShader;
   glLinkProgram             : Accessories.glLinkProgram;
   glCompileShader           : Accessories.glCompileShader;
   glShaderSource            : Accessories.glShaderSource;
   glGetShaderInfoLog        : Accessories.glGetShaderInfoLog;
   glGetShaderiv             : Accessories.glGetShaderiv;
   glGetProgramiv            : Accessories.glGetProgramiv;
   glGetProgramInfoLog       : Accessories.glGetProgramInfoLog;
   glUniformMatrix4fv        : Accessories.glUniformMatrix4fv;
   glGetUniformLocation      : Accessories.glGetUniformLocation;
   glGenVertexArrays         : Accessories.glGenVertexArrays;
   glGenBuffers              : Accessories.glGenBuffers;
   glBindVertexArray         : Accessories.glBindVertexArray;
   glBindBuffer              : Accessories.glBindBuffer;
   glBufferData              : Accessories.glBufferData;
   glBufferSubData           : Accessories.glBufferSubData;
   glVertexAttribPointer     : Accessories.glVertexAttribPointer;
   glEnableVertexAttribArray : Accessories.glEnableVertexAttribArray;
   glUseProgram              : Accessories.glUseProgram;
   glIsShader                : Accessories.glIsShader;
   glIsProgram               : Accessories.glIsProgram;
   glIsBuffer                : Accessories.glIsBuffer;
   glIsVertexArray           : Accessories.glIsVertexArray;
   glGetAttribLocation       : Accessories.glGetAttribLocation;
   glVertexArrayAttribFormat : Accessories.glVertexArrayAttribFormat;

end;
