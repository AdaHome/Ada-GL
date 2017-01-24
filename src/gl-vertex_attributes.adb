with Interfaces.C;
with Ada.Text_IO;

package body GL.Vertex_Attributes is


   --glEnableVertexAttribArray enables the generic vertex attribute array specified by index.
   --glDisableVertexAttribArray disables the generic vertex attribute array specified by index.
   --By default, all client-side capabilities are disabled, including all generic vertex attribute arrays.
   --If enabled, the values in the generic vertex attribute array will be accessed and used for rendering when calls are made to vertex array commands such as glDrawArrays or glDrawElements.
   procedure Enable (Index_Item : Index) is
   begin
      glEnableVertexAttribArray (GLuint (Index_Item));
   end;

   procedure Set_Memory_Layout (Index_Item : Index; Component_Count : Natural; Kind : Component_Kind; Normalized : Boolean; Stride_Bytes : Natural; Offset_Bytes : Natural) is
   begin
      glVertexAttribPointer (GLuint (Index_Item), GLint (Component_Count), Kind'Enum_Rep, Normalized'Enum_Rep, GLsizei (Stride_Bytes), System'To_Address (Offset_Bytes));
   end;

   function Get_Index_By_Name (From_Program : GLuint; Name : String) return Index is
      use Interfaces.C;
   begin
      return Index (glGetAttribLocation (From_Program, To_C (Name)));
   end;

   function Create_Index (Index_Item : Natural) return Index is
   begin
      return Index (Index_Item);
   end;

   procedure Put_Line_Fancy (Index_Item : Index) is
      use Ada.Text_IO;
   begin
      Put_Line ("Vertex attribute index: " & Index_Item'Img);
   end;

end;
