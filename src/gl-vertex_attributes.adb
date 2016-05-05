with Interfaces.C;
with Ada.Text_IO;

package body GL.Vertex_Attributes is


   --glEnableVertexAttribArray enables the generic vertex attribute array specified by index.
   --glDisableVertexAttribArray disables the generic vertex attribute array specified by index.
   --By default, all client-side capabilities are disabled, including all generic vertex attribute arrays.
   --If enabled, the values in the generic vertex attribute array will be accessed and used for rendering when calls are made to vertex array commands such as glDrawArrays or glDrawElements.
   procedure Enable_Vertex_Attribute_Array (Index : Location) is
   begin
      glEnableVertexAttribArray (GLuint (Index));
   end;

   procedure Set_Vertex_Attribute (Index : Location; Size : Natural; T : Component_Type; Normalized : Boolean; Stride : Natural; Pointer : Natural) is
   begin
      glVertexAttribPointer (GLuint (Index), GLint (Size), T'Enum_Rep, Normalized'Enum_Rep, GLsizei (Stride), System'To_Address (Pointer));
   end;

   function Get (From : GLuint; Name : String) return Location is
      use Interfaces.C;
   begin
      return Location (glGetAttribLocation (From, To_C (Name)));
   end;

   function Create (Index : Natural) return Location is
   begin
      return Location (Index);
   end;

   procedure Put_Line_Fancy (Item : Location) is
      use Ada.Text_IO;
   begin
      Put_Line ("Vertex attribute location: " & Item'Img);
   end;


   procedure Generate (Item : out Config_Array) is
   begin
      Item := (others => 0);
      glGenVertexArrays (Item'Length, GLuint (Item (Item'First))'Unrestricted_Access);
   end;

   function Generate return Config is
      Item : Config_Array (1 .. 1);
   begin
      Generate (Item);
      return Item (Item'First);
   end;

   procedure Bind (Item : Config) is
   begin
      glBindVertexArray (GLuint (Item));
   end;


end;
