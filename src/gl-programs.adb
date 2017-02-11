with GL.C.Complete;
with Interfaces.C;

package body GL.Programs is

   use GL.C.Complete;



   function Create_Empty return Program is
      P : Program;
   begin
      P := Program (glCreateProgram.all);
      return P;
   end;

   procedure Delete (Item : Program) is
   begin
      glDeleteProgram (GLuint (Item));
   end;

   procedure Link (Item : Program) is
      use GL.C;
   begin
      glLinkProgram (GLuint (Item));
   end;

   procedure Link_Checked (Item : Program) is
      Log_Buffer : String (1 .. 1000);
      Log_Length : Natural;
   begin
      Link (Item);
      if not Link_Succeess (Item) then
         Get_Link_Log (Item, Log_Buffer, Log_Length);
         raise Link_Error with Log_Buffer (1 .. Log_Length);
      end if;
   end;

   procedure Set_Current (Item : Program) is
      use GL.C;
   begin
      glUseProgram (GLuint (Item));
   end;

   function Validate (Item : Program) return Boolean is
      use GL.C;
      use type GLboolean;
      B : GLboolean;
   begin
      B := glIsProgram (GLuint (Item));
      return B = GL_TRUE;
   end;

   function Identity (Item : Program) return GLuint is
   begin
      return GLuint (Item);
   end;

   procedure Attach (To : Program; Attachment : GLuint) is
   begin
      glAttachShader (GLuint (To), Attachment);
   end;

   procedure Get_Info (From : Program; Kind : Program_Info; Result : access GLint) is
   begin
      glGetProgramiv (GLuint (From), Kind'Enum_Rep, Result);
   end;

   function Link_Succeess (Item : Program) return Boolean is
      use type GLint;
      R : aliased GLint;
   begin
      Get_Info (Item, Link_Info, R'Access);
      return R = GL_TRUE;
   end;

   procedure Get_Link_Log (Item : Program; Message : out String; Count : out Natural) is
      use Interfaces.C;
      Length : aliased GLsizei := 0;
      Text : aliased GLstring (1 .. Message'Length);
   begin
      glGetProgramInfoLog (GLuint (Item), Message'Length, Length'Access, Text'Address);
      To_Ada (Text, String (Message), Count);
   end;

   function Get_Link_Log (Item : Program; Count : Natural := 1024) return String is
      Text : String (1 .. Count);
      Length : Natural := 0;
   begin
      Get_Link_Log (Item, Text, Length);
      return Text (1 .. Length);
   end;

end;
