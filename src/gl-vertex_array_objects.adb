with Interfaces.C;
with Ada.Text_IO;
use Ada.Text_IO;

package body GL.Vertex_Array_Objects is

   function Generate return Name is
      use GL.C;
      use GL.C.Complete;
      type S is array (1 .. 1) of aliased Name with Convention => C;
      Name_Array : aliased S := (others => 0);
      --k : Character;
   begin
      --Put_Line ("glGenVertexArrays");
      --Get_Immediate (K);
      glGenVertexArrays (Name_Array'Length, GLuint (Name_Array (Name_Array'First))'Unrestricted_Access);
      --Put_Line ("glGenVertexArrays.");
      --Put_Line_Fancy (Name_Array (Name_Array'First));
      return Name_Array (Name_Array'First);
   end;

   procedure Bind (Item : Name) is
      use GL.C;
      use GL.C.Complete;
   begin
      glBindVertexArray (GLuint (Item));
   end;

   procedure Put_Line_Fancy (Item : Name) is
      use Ada.Text_IO;
   begin
      Put_Line ("Vertex array object name: " & Item'Img);
   end;

end;
