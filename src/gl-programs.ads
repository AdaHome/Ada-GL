with GL.C;
with GL.C.Complete;

package GL.Programs is

   use GL.C;
   use GL.C.Complete;

   type Program is private;

   type Program_Info is (Delete_Info, Compile_Info, Link_Info, Log_Length_Info);

   function Identity (Item : Program) return GLuint;
   function Validate (Item : Program) return Boolean;
   function Create_Empty return Program;
   procedure Link (Item : Program);
   procedure Link_Checked (Item : Program);
   procedure Set_Current (Item : Program);
   procedure Attach (To : Program; Attachment : GLuint);
   procedure Get_Link_Log (Item : Program; Message : out String; Count : out Natural);
   function Get_Link_Log (Item : Program; Count : Natural := 1024) return String;
   function Link_Succeess (Item : Program) return Boolean;

   Link_Error : exception;

private


   type Program is new GLuint range 1 .. GLuint'Last;

   for Program_Info'Size use GLenum'Size;
   for Program_Info use
     (
      Delete_Info => GL_DELETE_STATUS,
      Compile_Info => GL_COMPILE_STATUS,
      Link_Info => GL_LINK_STATUS,
      Log_Length_Info => GL_INFO_LOG_LENGTH
     );

end;
