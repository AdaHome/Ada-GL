with GL.Programs;
with GL.Programs.Shaders.Files;

package GL.Programs_Overhaul is

   subtype Program is GL.Programs.Program;
   subtype Shader_File_Name is GL.Programs.Shaders.Files.Shader_File_Name;


   Create_Empty_Program_Object_Error : exception;
   Create_Empty_Shader_Object_Error : exception;
   Compile_Error : exception;
   Link_Error : exception;
   Source_Length_Error : exception;
   Empty_File_Error : exception;
   Unknown_Shader_File_Extension_Error : exception;

   function Create_Empty return Program;
   procedure Attach (Item : Program; Name : Shader_File_Name);
   procedure Link (Item : Program);
   procedure Enable (Item : Program);

end;
