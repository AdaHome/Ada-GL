with GL.C.Complete;
with GL.C.Initializations;
with Interfaces.C;

package body GL.Programs is

   use GL.C.Complete;
   use GL.C.Initializations;



   function Create_Empty return Program is
      P : Program;
   begin
      P := Program (glCreateProgram.all);
      return P;
   end;

   procedure Link (Item : Program) is
      use GL.C;
   begin
      glLinkProgram (GLuint (Item));
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

   procedure Attach (Item : Program; S : GLuint) is
   begin
      glAttachShader (GLuint (Item), S);
   end;

   procedure Get_Info (Item : Program; P : Program_Info; R : access GLint) is
   begin
      glGetProgramiv (GLuint (Item), P'Enum_Rep, R);
   end;

   function Link_Succeess (Item : Program) return Boolean is
      use type GLint;
      R : aliased GLint;
   begin
      Get_Info (Item, Link_Info, R'Access);
      return R = GL_TRUE;
   end;

   procedure Get_Compile_Log (Item : Program; Message : out String; Count : out Natural) is
      use Interfaces.C;
      Length : aliased GLsizei := 0;
      Text : aliased GLstring (1 .. Message'Length);
   begin
      glGetProgramInfoLog (GLuint (Item), Message'Length, Length'Access, Text'Address);
      To_Ada (Text, String (Message), Count);
   end;

   function Get_Compile_Log (Item : Program; Count : Natural := 1024) return String is
      Text : String (1 .. Count);
      Length : Natural := 0;
   begin
      Get_Compile_Log (Item, Text, Length);
      return Text (1 .. Length);
   end;

end;
