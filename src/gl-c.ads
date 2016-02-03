with System;
with Interfaces.C;
with Interfaces.C.Strings;


package GL.C is

   use System;
   use Interfaces.C;
   use Interfaces.C.Strings;

   subtype GLbitfield   is unsigned;
   subtype GLboolean    is unsigned_char;
   subtype GLfloat      is C_float;
   subtype GLint        is int;
   subtype GLintptr     is ptrdiff_t;
   subtype GLuint       is unsigned;
   subtype GLenum       is unsigned;
   subtype GLsizei      is int;
   subtype GLsizeiptr   is ptrdiff_t;
   subtype GLbyte       is signed_char;
   subtype GLubyte      is unsigned_char;
   subtype GLvoid       is Address;
   subtype GLchar       is char;

   -- Specials
   type GLintv          is array (size_t range <>) of aliased int with Convention => C;
   subtype GLstring     is char_array;
   subtype GLstringv    is chars_ptr_array;

end;
