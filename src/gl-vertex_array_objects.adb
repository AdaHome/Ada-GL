with Interfaces.C;
with Ada.Text_IO;
use Ada.Text_IO;

package body GL.Vertex_Array_Objects is

   function Generate return Vertex_Array_Object is
      use GL.C;
      use GL.C.Complete;
      type S is array (1 .. 1) of aliased GLuint with Convention => C;
      Name_Array : aliased S := (others => 0);
   begin
      --Put_Line ("glGenVertexArrays");
      --Get_Immediate (K);
      glGenVertexArrays (Name_Array'Length, Name_Array (Name_Array'First)'Unrestricted_Access);
      --Put_Line ("glGenVertexArrays.");
      --Put_Line_Fancy (Name_Array (Name_Array'First));

      -- The names returned in arrays are marked as used, for the purposes of glGenVertexArrays only,
      -- but they acquire state and type only when they are first bound.
      glBindVertexArray (Name_Array (Name_Array'First));
      glBindVertexArray (0);

      return Vertex_Array_Object (Name_Array (Name_Array'First));
   end;

   function Create return Vertex_Array_Object is
      use GL.C;
      use GL.C.Complete;
      type S is array (1 .. 1) of aliased GLuint with Convention => C;
      Name_Array : aliased S := (others => 0);
   begin
      glCreateVertexArrays (Name_Array'Length, Name_Array (Name_Array'First)'Unrestricted_Access);
      return Vertex_Array_Object (Name_Array (Name_Array'First));
   end;

   procedure Bind (Item : Vertex_Array_Object) is
      use GL.C;
      use GL.C.Complete;
   begin
      glBindVertexArray (GLuint (Item));
   end;

   procedure Set_Attribute_Enable (Item : Attribute) is
      use GL.C;
      use GL.C.Complete;
   begin
      glEnableVertexAttribArray (GLuint (Item));
   end;

   procedure Set_Attribute_Enable (Item : Vertex_Array_Object; Index : Attribute) is
      use GL.C;
      use GL.C.Complete;
   begin
      glEnableVertexArrayAttrib (GLuint (Item), GLuint (Index));
   end;




   procedure Set_Attribute_Memory_Layout (Item : Attribute; Component_Count : Natural; Kind : Component_Kind; Normalized : Boolean; Stride_Bytes : Natural; Offset_Bytes : Natural) is
      use GL.C;
      use GL.C.Complete;
   begin
      glVertexAttribPointer (GLuint (Item), GLint (Component_Count), Kind'Enum_Rep, Normalized'Enum_Rep, GLsizei (Stride_Bytes), System'To_Address (Offset_Bytes));
   end;

   procedure Set_Attribute_Memory_Layout (VAO : Vertex_Array_Object; Index : Attribute; Component_Count : Natural; Kind : Component_Kind; Normalized : Boolean; Offset_Bytes : Natural) is
      use GL.C;
      use GL.C.Complete;
   begin
      glVertexArrayAttribFormat (GLuint (VAO), GLuint (Index), GLint (Component_Count), Kind'Enum_Rep, Normalized'Enum_Rep, GLuint (Offset_Bytes));
   end;


   function Get_Attribute_By_Name (From_Program : GL.C.GLuint; Name : String) return Attribute is
      use GL.C.Complete;
      use Interfaces.C;
   begin
      return Attribute (glGetAttribLocation (From_Program, To_C (Name)));
   end;

   procedure Put_Line_Fancy (Item : Vertex_Array_Object) is
      use Ada.Text_IO;
   begin
      Put_Line ("Vertex array object name: " & Item'Img);
   end;

end;
