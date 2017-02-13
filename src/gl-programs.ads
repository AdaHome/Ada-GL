with GL.C;
with GL.C.Complete;
with GL.Errors;

package GL.Programs is

   use GL.C;
   use GL.C.Complete;
   use GL.Errors;

   type Program is private;

   type Program_Info is (Delete_Info, Compile_Info, Link_Info, Log_Length_Info);

   function Identity (Item : Program) return GLuint;

   function Validate (Item : Program) return Boolean with
     Post => Check_No_Error;

   function Create_Empty return Program with
     Post => Validate (Create_Empty'Result) and Check_No_Error;

   procedure Delete (Item : Program) with
     Post => Check_No_Error;

   procedure Link (Item : Program) with
     Post => Check_No_Error;

   procedure Link_Checked (Item : Program) with
     Post => Check_No_Error;

   procedure Set_Current (Item : Program) with
     Pre => Validate (Item),
     Post => Check_No_Error;

   procedure Attach (To : Program; Attachment : GLuint) with
     Post => Check_No_Error;

   procedure Get_Link_Log (Item : Program; Message : out String; Count : out Natural) with
     Post => Check_No_Error;

   function Get_Link_Log (Item : Program; Count : Natural := 1024) return String with
     Post => Check_No_Error;

   function Link_Succeess (Item : Program) return Boolean with
     Post => Check_No_Error;

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
