with Ada.Streams.Stream_IO;
with Ada.Directories;

package body GL.Shaders.Files is


   procedure Read (Name : String; Content : out String) is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Stream_Item : Stream_Access;
   begin
      Open  (File, In_File, String (Name));
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

end;
