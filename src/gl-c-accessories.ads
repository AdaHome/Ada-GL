package GL.C.Accessories is

   type glCreateProgram           is access function return GLuint with Convention => StdCall;
   type glDeleteProgram           is access procedure (program : GLuint) with Convention => StdCall;
   type glDeleteShader            is access procedure (shader : GLuint) with Convention => StdCall;
   type glLinkProgram             is access procedure (program : GLuint) with Convention => StdCall;
   type glGetProgramiv            is access procedure (program : GLuint; pname : GLenum; params : access GLint) with Convention => StdCall;
   type glGetProgramInfoLog       is access procedure (program : GLuint; maxLength : GLsizei; length : access GLsizei; infoLog : Address) with Convention => StdCall;
   type glUseProgram              is access procedure (program : GLuint) with Convention => StdCall;
   type glCreateShader            is access function (shaderType : GLenum) return GLuint with Convention => StdCall;
   --type glShaderSource            is access procedure (shader : GLuint; count : GLsizei; string : GLstringv; length : GLintv) with Convention => StdCall;
   type glShaderSource            is access procedure (shader : GLuint; count : GLsizei; string : GLstringv; length : GLintv) with Convention => StdCall;
   type glAttachShader            is access procedure (program, shader : GLuint) with Convention => StdCall;
   type glCompileShader           is access procedure (shader : GLuint) with Convention => StdCall;
   type glGetShaderInfoLog        is access procedure (shader : GLuint; maxLength : GLsizei; length : out GLsizei; infoLog : Address) with Convention => StdCall;
   type glGetShaderiv             is access procedure (shader : GLuint; pname : GLenum; params : access GLint);
   type glUniformMatrix4fv        is access procedure (location : GLint; count : GLsizei; transpose : GLboolean; value : Address) with Convention => StdCall;
   type glUniform1f               is access procedure (location : GLint; value : GLfloat) with Convention => StdCall;
   type glUniform1i               is access procedure (location : GLint; value : GLint) with Convention => StdCall;
   type glGetUniformLocation      is access function (program : GLuint; name : GLstring) return GLint with Convention => StdCall;
   type glGetAttribLocation       is access function (program : GLuint; name : GLstring) return GLint with Convention => StdCall;
   type glGenVertexArrays         is access procedure (n : GLsizei; arrays : access GLuint) with Convention => StdCall;
   type glBindVertexArray         is access procedure (arr : GLuint) with Convention => StdCall;
   type glGenBuffers              is access procedure (n : GLsizei; buffers : access GLuint) with Convention => StdCall;
   type glBindBuffer              is access procedure (target : GLenum; buf : GLuint) with Convention => StdCall;
   type glBufferData              is access procedure (target : GLenum; size : GLsizeiptr; data : Address; usage : GLenum) with Convention => StdCall;
   type glVertexAttribPointer     is access procedure (index : GLuint; size : GLint; t : GLenum; normalized : GLboolean; stride : GLsizei; pointer : Address) with Convention => StdCall;
   type glEnableVertexAttribArray is access procedure (index : GLuint) with Convention => StdCall;
   type glIsShader                is access function (shader : GLuint) return GLboolean with Convention => StdCall;
   type glIsProgram               is access function (program : GLuint) return GLboolean with Convention => StdCall;
   type glIsBuffer                is access function (buffer : GLuint) return GLboolean with Convention => StdCall;
   type glIsVertexArray           is access function (arr : GLuint) return GLboolean with Convention => StdCall;

   type glPixelStorei             is access procedure (pname : GLenum; param : GLint) with Convention => StdCall;
   type glGenTextures             is access procedure (n : GLsizei; textures : access GLuint) with Convention => StdCall;
   type glBindTexture             is access procedure (target : GLenum; texture : GLuint) with Convention => StdCall;
   type glTexImage2D              is access procedure (target : GLenum; level: GLint; internalFormat : GLint; width : GLsizei; height : GLsizei; border : GLint; format : GLenum; kind : GLenum; data : Address) with Convention => StdCall;
   type glTexParameteri           is access procedure (target : GLenum; pname : GLenum; param : GLint) with Convention => StdCall;
   type glIsTexture               is access function (texture : GLuint) return GLboolean with Convention => StdCall;
   type glActiveTexture           is access procedure (texture : GLenum) with Convention => StdCall;

   type glBufferSubData           is access procedure (target : GLenum; offset : GLintptr; size : GLsizeiptr; data : Address) with Convention => StdCall;
   type glGetActiveAttrib         is access procedure (program : GLuint; index : GLuint; bufSize : GLsizei; length : out GLsizei; size : out GLint; kind : out GLenum; name : Address);
   type glDrawArrays              is access procedure (mode : GLenum; first : GLint; count : GLsizei) with Convention => StdCall;
   type glClear                   is access procedure (mask : GLbitfield) with Convention => StdCall;

   -- OpenGL Version 4.5
   type glVertexArrayAttribBinding is access procedure (vaobj : GLuint; attribindex : GLuint; bindingindex : GLuint) with Convention => StdCall;
   type glVertexArrayAttribFormat is access procedure (vaobj : GLuint; attribindex : GLuint; size : GLint; t : GLenum; normalized : GLboolean; relativeoffset : GLuint) with Convention => StdCall;
   type glVertexArrayVertexBuffer is access procedure (vaobj : GLuint; bindingindex : GLuint; buffer : GLuint; offset : GLintptr; stride : GLsizei) with Convention => StdCall;
   type glEnableVertexArrayAttrib is access procedure (vaobj : GLuint; index : GLuint) with Convention => StdCall;
   type glCreateVertexArrays is access procedure ( n : GLsizei; arrays : access GLuint) with Convention => StdCall;
   type glCreateBuffers is access procedure (n : GLsizei; buffers : access GLuint) with Convention => StdCall;
   type glNamedBufferData is access procedure (buffer : GLuint; size : GLsizeiptr; data : Address; usage : GLenum) with Convention => StdCall;
   type glNamedBufferSubData is access procedure (buffer : GLuint; offset : GLintptr; size : GLsizei; data : Address) with Convention => StdCall;

   type glTextureParameteri is access procedure
     (texture : GLuint; pname : GLenum; param : GLint) with Convention => StdCall;

   type glTextureSubImage2D is access procedure
     (texture : GLuint; level: GLint; xoffset : GLint; yoffset : GLint; width : GLsizei; height : GLsizei; format : GLenum; kind : GLenum; data : Address) with Convention => StdCall;

   type glTextureSubImage3D is access procedure
     (texture : GLuint; level: GLint; xoffset : GLint; yoffset : GLint; zoffset : GLint; width : GLsizei; height : GLsizei; depth : GLsizei; format : GLenum; kind : GLenum; data : Address) with Convention => StdCall;

   type glTextureStorage2D  is access procedure
     (texture : GLuint; levels: GLint; internalformat : GLenum; width : GLsizei; height : GLsizei) with Convention => StdCall;

   type glTextureStorage3D  is access procedure
     (texture : GLuint; levels: GLint; internalformat : GLenum; width : GLsizei; height : GLsizei; depth : GLsizei) with Convention => StdCall;

   type glCreateTextures    is access procedure
     (target : GLenum; n : GLsizei; textures : access GLuint) with Convention => StdCall;
end;
