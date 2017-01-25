with Interfaces.C;
with Ada.Text_IO;

package body GL.Vertex_Attributes is

   procedure Enable (Item : Attribute) is
   begin
      glEnableVertexAttribArray (GLuint (Item));
   end;

   procedure Set_Memory_Layout (Item : Attribute; Component_Count : Natural; Kind : Component_Kind; Normalized : Boolean; Stride_Bytes : Natural; Offset_Bytes : Natural) is
   begin
      glVertexAttribPointer (GLuint (Item), GLint (Component_Count), Kind'Enum_Rep, Normalized'Enum_Rep, GLsizei (Stride_Bytes), System'To_Address (Offset_Bytes));
   end;

   function Get_Index_By_Name (From_Program : GLuint; Name : String) return Attribute is
      use Interfaces.C;
   begin
      return Attribute (glGetAttribLocation (From_Program, To_C (Name)));
   end;

   procedure Put_Line_Fancy (Item : Attribute) is
      use Ada.Text_IO;
   begin
      Put_Line ("Vertex attribute index: " & Item'Img);
   end;

end;
