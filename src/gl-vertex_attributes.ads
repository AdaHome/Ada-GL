with GL.C;
with GL.C.Complete;
with System;

package GL.Vertex_Attributes is

   use GL.C;
   use GL.C.Complete;

   type Config is private;
   type Config_Array is array (Integer range <>) of Config;
   type Location is private;
   type Component_Type is (Byte_Type, Unsigned_Byte_Type, Short_Type, Unsigned_Short_Type, Float_Type, Fixed_Type);
   type Byte_Unit is new Natural;
   type Bit_Unit is new Natural;

   --glEnableVertexAttribArray enables the generic vertex attribute array specified by index.
   --glDisableVertexAttribArray disables the generic vertex attribute array specified by index.
   --By default, all client-side capabilities are disabled, including all generic vertex attribute arrays.
   --If enabled, the values in the generic vertex attribute array will be accessed and used for rendering when calls are made to vertex array commands such as glDrawArrays or glDrawElements.
   procedure Enable (Index : Location);

   procedure Set (Index : Location; Size : Natural; T : Component_Type; Normalized : Boolean; Stride : Byte_Unit; Pointer : Byte_Unit);
   procedure Set (Index : Location; Size : Natural; T : Component_Type; Normalized : Boolean; Stride : Bit_Unit; Pointer : Bit_Unit);

   function Get (From : GLuint; Name : String) return Location;
   function Use_Index (Index : Natural) return Location;

   procedure Put_Line_Fancy (Item : Location);
   procedure Put_Line_Fancy (Item : Config);

   function Generate return Config;
   procedure Bind (Item : Config);

private

   type Config is new GLuint;
   type Location is new GLuint range 0 .. GLuint'Last;

   for Component_Type'Size use GLenum'Size;
   for Component_Type use
     (
      Byte_Type => GL_BYTE,
      Unsigned_Byte_Type => GL_UNSIGNED_BYTE,
      Short_Type => GL_SHORT,
      Unsigned_Short_Type => GL_UNSIGNED_SHORT,
      Float_Type => GL_FLOAT,
      Fixed_Type => GL_FIXED
     );

end;
