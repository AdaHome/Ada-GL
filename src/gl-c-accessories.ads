package GL.C.Accessories is

   type glCreateProgram           is access function return GLuint with Convention => StdCall;
   type glLinkProgram             is access procedure (program : GLuint) with Convention => StdCall;
   type glGetProgramiv            is access procedure (program : GLuint; pname : GLenum; params : access GLint) with Convention => StdCall;
   type glGetProgramInfoLog       is access procedure (program : GLuint; maxLength : GLsizei; length : access GLsizei; infoLog : Address) with Convention => StdCall;
   type glUseProgram              is access procedure (program : GLuint) with Convention => StdCall;
   type glCreateShader            is access function (shaderType : GLenum) return GLuint with Convention => StdCall;
   type glShaderSource            is access procedure (shader : GLuint; count : GLsizei; string : GLstringv; length : GLintv) with Convention => StdCall;
   type glAttachShader            is access procedure (program, shader : GLuint) with Convention => StdCall;
   type glCompileShader           is access procedure (shader : GLuint) with Convention => StdCall;
   type glGetShaderInfoLog        is access procedure (shader : GLuint; maxLength : GLsizei; length : access GLsizei; infoLog : Address) with Convention => StdCall;
   type glGetShaderiv             is access procedure (shader : GLuint; pname : GLenum; params : access GLint);
   type glUniformMatrix4fv        is access procedure (location : GLint; count : GLsizei; transpose : GLboolean; value : Address) with Convention => StdCall;
   type glGetUniformLocation      is access function (program : GLuint; name : GLstring) return GLint with Convention => StdCall;
   type glGenVertexArrays         is access procedure (n : GLsizei; arrays : access GLuint) with Convention => StdCall;
   type glBindVertexArray         is access procedure (arr : GLuint) with Convention => StdCall;
   type glGenBuffers              is access procedure (n : GLsizei; buffers : access GLuint) with Convention => StdCall;
   type glBindBuffer              is access procedure (target : GLenum; buf : GLuint) with Convention => StdCall;
   type glBufferData              is access procedure (target : GLenum; size : GLsizeiptr; data : System.Address; usage : GLenum) with Convention => StdCall;
   type glVertexAttribPointer     is access procedure (index : GLuint; size : GLint; t : GLenum; normalized : GLboolean; stride : GLsizei; pointer : System.Address) with Convention => StdCall;
   type glEnableVertexAttribArray is access procedure (index : GLuint) with Convention => StdCall;
   type glIsShader                is access function (shader : GLuint) return GLboolean with Convention => StdCall;
   type glIsProgram               is access function (program : GLuint) return GLboolean with Convention => StdCall;
   type glIsBuffer                is access function (buffer : GLuint) return GLboolean with Convention => StdCall;
   type glIsVertexArray           is access function (arr : GLuint) return GLboolean with Convention => StdCall;
   type glGetAttribLocation       is access function (program : GLuint; name : GLstring) return GLint with Convention => StdCall;
   type glVertexArrayAttribFormat is access procedure (vaobj : GLuint; attribindex : GLuint; size : GLint; t : GLenum; normalized : GLboolean; relativeoffset : GLuint) with Convention => StdCall;
   type glBufferSubData           is access procedure (target : GLenum; offset : GLintptr; size : GLsizeiptr; data : Address) with Convention => StdCall;

end;
