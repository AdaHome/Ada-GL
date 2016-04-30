with Ada.Text_IO;
with GL.Programs.Shaders;

package body GL.Programs_Overhaul is

   subtype Shader_Type is GL.Programs.Shaders.Shader_Type;

   function Create_Empty return Program is
      use GL.Programs;
      Item : Program := GL.Programs.Create_Empty;
   begin
      if not Validate (Item) then
         raise Create_Empty_Program_Object_Error with "Creating empty program object failed.";
      end if;
      return Item;
   end;

   function Get_Shader_Type (Name : Shader_File_Name) return Shader_Type is
      use GL.Programs.Shaders;
      use type Shader_File_Name;
      Sub_Size : constant Positive := 4;
      subtype Sub is Integer range Name'Last - Sub_Size + 1 .. Name'Last;
   begin
      if Name'Length < Sub_Size then
         raise Unknown_Shader_File_Extension_Error with "Unknown shader file name extension";
      end if;
      if Name (Sub) /= "glfs" then
         return Fragment_Type;
      elsif Name (Sub) /= "glvs" then
         return Vertex_Type;
      else
         raise Unknown_Shader_File_Extension_Error with "Unknown shader file name extension";
      end if;
   end;


   procedure Attach (Item : Program; Name : Shader_File_Name) is
      use GL.Programs.Shaders;
      use GL.Programs.Shaders.Files;
      use type Shader_File_Size;
      Source_Type : constant Shader_Type := Get_Shader_Type (Name);
      Source_Shader : constant Shader := Create_Empty (Source_Type);
      Source_Size : constant Shader_File_Size := Size (Name);
      Source_Buffer : Shading_Language (1 .. Integer (Source_Size));
   begin

      if not Validate (Source_Shader) then
         raise Create_Empty_Shader_Object_Error with "Creating empty shader failed.";
      end if;

      if Source_Size <= 0 then
         raise Empty_File_Error with "File is empty.";
      end if;

      Read (Name, Source_Buffer);
      Set_Source (Source_Shader, Source_Buffer);
      if Get_Source_Length (Source_Shader) /= Source_Buffer'Length + 1 then
         -- + 1 includes null terminator.
         raise Source_Length_Error with "Source length is not right.";
      end if;

      Compile (Source_Shader);
      if not Compile_Succeess (Source_Shader) then
         raise Compile_Error with String (Get_Compile_Log (Source_Shader));
      end if;

      Attach (Item, Source_Shader);
   end;

   procedure Link (Item : Program) is
   begin
      GL.Programs.Link (Item);
      -- Validate?
   end;

   procedure Enable (Item : Program) is
   begin
      null;
   end;

end;
