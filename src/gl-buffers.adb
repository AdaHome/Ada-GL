with Ada.Text_IO;


package body GL.Buffers is

   function Is_Buffer (Item : Buffer) return Boolean is
      use type GL.C.GLboolean;
   begin
      return glIsBuffer (GLuint (Item)) = GL_TRUE;
   end Is_Buffer;

   procedure Generate_Buffer (Item : out Buffer_Array) is
   begin
      Item := (others => 0);
      glGenBuffers (Item'Length, GLuint (Item (Item'First))'Unrestricted_Access);
   end;

   function Generate_Buffer return Buffer is
      Item : Buffer_Array (1 .. 1);
   begin
      Item := (others => 0);
      glGenBuffers (Item'Length, GLuint (Item (Item'First))'Unrestricted_Access);
      return Item (Item'First);
   end;

   function Create_Buffer return Buffer is
      Item : Buffer_Array (1 .. 1);
   begin
      Item := (others => 0);
      glCreateBuffers (Item'Length, GLuint (Item (Item'First))'Unrestricted_Access);
      return Item (Item'First);
   end;

   procedure Bind (To : Buffer_Slot; Item : Buffer) is
   begin
      glBindBuffer (To'Enum_Rep, GLuint (Item));
   end;

   procedure Create_New_Storage (B : Buffer; Size_Bytes : Natural; Data : Address; Usage : Buffer_Usage) is
   begin
      glNamedBufferData (GLuint (B), GLsizeiptr (Size_Bytes), Data, Usage'Enum_Rep);
   end;

   procedure Redefine_Storage (B : Buffer; Offset_Byte : Natural; Size_Bytes : Natural; Data : Address) is
   begin
      glNamedBufferSubData (GLuint (B), GLintptr (Offset_Byte), GLsizei (Size_Bytes), Data);
   end;

   procedure Put_Line_Fancy (Item : Buffer) is
      use Ada.Text_IO;
   begin
      Put_Line ("Buffer: " & Item'Img);
   end;

   function Identity (Item : Buffer) return GLuint is
   begin
      return GLuint (Item);
   end;

end;
