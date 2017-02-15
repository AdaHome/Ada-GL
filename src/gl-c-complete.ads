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

   GL_BYTE                 : constant := 16#1400#;
   GL_UNSIGNED_BYTE        : constant := 16#1401#;
   GL_SHORT                : constant := 16#1402#;
   GL_UNSIGNED_SHORT       : constant := 16#1403#;
   GL_FLOAT                : constant := 16#1406#;
   GL_FIXED                : constant := 16#140C#;

   GL_LINK_STATUS          : constant := 16#8B82#;
   GL_ARRAY_BUFFER         : constant := 16#8892#;
   GL_ELEMENT_ARRAY_BUFFER : constant := 16#8893#;
   GL_STATIC_DRAW          : constant := 16#88E4#;
   GL_DYNAMIC_DRAW         : constant := 16#88E5#; --FIx
   GL_LINES                : constant := 16#0001#;
   GL_LINE_STRIP           : constant := 16#0003#;
   GL_TRIANGLES            : constant := 16#0004#;
   GL_ACTIVE_ATTRIBUTES    : constant := 16#8B89#;
   GL_MAJOR_VERSION        : constant := 16#821B#;
   GL_COLOR_BUFFER_BIT     : constant := 16#00004000#;
   GL_DEPTH_BUFFER_BIT     : constant := 16#00000100#;
   GL_DEPTH_TEST           : constant := 16#0B71#;
   GL_TEXTURE_1D           : constant := 16#0DE0#;
   GL_TEXTURE_2D           : constant := 16#0DE1#;
   GL_TEXTURE_2D_ARRAY               : constant :=  16#8C1A#;
   GL_RED                  : constant := 16#1903#;
   GL_GREEN                : constant := 16#1904#;
   GL_BLUE                 : constant := 16#1905#;
   GL_RGB                  : constant := 16#1907#;
   GL_RGBA                 : constant := 16#1908#;
   GL_NEAREST              : constant := 16#2600#;
   GL_LINEAR               : constant := 16#2601#;
   GL_CLAMP                : constant := 16#2900#;
   GL_REPEAT               : constant := 16#2901#;

   GL_CLAMP_TO_EDGE        : constant := 16#812F#;

   GL_TEXTURE0             : constant := 16#84C0#;
   GL_TEXTURE1             : constant := 16#84C1#;

   GL_BLEND                : constant := 16#0BE2#;

   GL_UNPACK_ALIGNMENT     : constant := 16#0CF5#;
   GL_PACK_ALIGNMENT       : constant := 16#0D05#;
   GL_TEXTURE_MAG_FILTER   : constant := 16#2800#;
   GL_TEXTURE_MIN_FILTER   : constant := 16#2801#;
   GL_TEXTURE_WRAP_S       : constant := 16#2802#;
   GL_TEXTURE_WRAP_T       : constant := 16#2803#;

   GL_SRC_ALPHA            : constant :=  16#0302#;
   GL_ONE_MINUS_SRC_ALPHA  : constant :=  16#0303#;

   GL_RGBA2                : constant :=  16#8055#;
   GL_R8                   : constant :=  16#8229#;
   GL_R16                  : constant :=  16#822A#;
   GL_NO_ERROR : constant := 16#0#;



   procedure glViewport (x : GLint; y : GLint; width : GLsizei; height : GLsizei) with
     Import,
     Convention => StdCall,
     External_Name => "glViewport";

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

   procedure glBlendFunc (sfactor : GLenum; dfactor : GLenum) with
     Import,
     Convention => StdCall,
     External_Name => "glBlendFunc";

   glCreateProgram           : Accessories.glCreateProgram;
   glDeleteProgram           : Accessories.glDeleteProgram;
   glDeleteShader            : Accessories.glDeleteShader;
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
   glUniform1f               : Accessories.glUniform1f;
   glUniform1i : Accessories.glUniform1i;
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

   glPixelStorei   : Accessories.glPixelStorei;
   glGenTextures   : Accessories.glGenTextures;
   glBindTexture   : Accessories.glBindTexture;
   glTexImage2D    : Accessories.glTexImage2D;
   glTexParameteri : Accessories.glTexParameteri;
   glIsTexture     : Accessories.glIsTexture;
   glActiveTexture : Accessories.glActiveTexture;

   -- OpenGL Version 4.5
   glVertexArrayAttribBinding : Accessories.glVertexArrayAttribBinding;
   glVertexArrayAttribFormat  : Accessories.glVertexArrayAttribFormat;
   glVertexArrayVertexBuffer  : Accessories.glVertexArrayVertexBuffer;
   glEnableVertexArrayAttrib  : Accessories.glEnableVertexArrayAttrib;
   glCreateVertexArrays       : Accessories.glCreateVertexArrays;
   glCreateBuffers            : Accessories.glCreateBuffers;
   glNamedBufferData          : Accessories.glNamedBufferData;
   glNamedBufferSubData       : Accessories.glNamedBufferSubData;

   glTextureParameteri       : Accessories.glTextureParameteri;
   glTextureSubImage2D       : Accessories.glTextureSubImage2D;
   glCreateTextures          : Accessories.glCreateTextures;
   glTextureStorage2D        : Accessories.glTextureStorage2D;

   glTextureSubImage3D    : Accessories.glTextureSubImage3D;
   glTextureStorage3D    : Accessories.glTextureStorage3D;
end;
