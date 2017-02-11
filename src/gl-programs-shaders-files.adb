with Ada.Streams.Stream_IO;
with Ada.Directories;

package body GL.Programs.Shaders.Files is


   procedure Read (Name : String; Content : out String) is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Stream_Item : Stream_Access;
   begin
      Open (File, In_File, Name);
      Stream_Item := Stream (File);
      String'Read (Stream_Item, Content);
      Close (File);
   end;


   procedure Set_Source_File (Item : Shader; Name : String) is
      use Ada.Directories;
      Source_Buffer : String (1 .. Natural (Size (Name)));
   begin
      Read (Name, Source_Buffer);
      Set_Source (Item, Source_Buffer);
      null;
   end;

   procedure Set_Source_File (Item : Shader; Name : String; Source_Buffer : out String) is
      use Ada.Directories;
      subtype I is Integer range Source_Buffer'First .. Source_Buffer'First + Natural (Size (Name)) - 1;
   begin
      Read (Name, Source_Buffer (I));
      Set_Source (Item, Source_Buffer (I));
      null;
   end;

end;
