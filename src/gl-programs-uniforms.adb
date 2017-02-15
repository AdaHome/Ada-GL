with Interfaces.C;
with Ada.Text_IO;

package body GL.Programs.Uniforms is

   function Identity (Item : Location) return GLint is
   begin
      return GLint (Item);
   end;

   function Get (From : Program; Name : String) return Location is
      use GL.C.Complete;
      use Interfaces.C;
   begin
      return Location (glGetUniformLocation (Identity (From), To_C (Name)));
   end;


   function Get_Checked (From : Program; Name : String) return Location is
      use GL.C.Complete;
      --use Interfaces.C;
      use type GL.C.GLint;
      L : GLint;
   begin
      L := glGetUniformLocation (Identity (From), Interfaces.C.To_C (Name));
      if L = -1 then
         raise Program_Error with "Name (" & Name & ") does not correspond to an active uniform variable in program or if name starts with the reserved prefix gl_.";
      end if;
      return Location (L);
   end;



   procedure Modify_Matrix_4f (Item : Location; Data : Address) is
      use GL.C.Complete;
      use GL.C;
   begin
      glUniformMatrix4fv (GLint (Item), 1, GL_FALSE, Data);
   end;

   procedure Modify_1f (Item : Location; Data : GLFloat) is
      use GL.C.Complete;
      use GL.C;
   begin
      glUniform1f (GLint (Item), Data);
   end;

   procedure Modify_1i (Item : Location; Data : GLint) is
      use GL.C.Complete;
      use GL.C;
   begin
      glUniform1i (GLint (Item), Data);
   end;




   procedure Put_Line_Fancy (Item : Location) is
      use Ada.Text_IO;
   begin
      Put_Line ("Uniform location: " & Item'Img);
   end;

end;
