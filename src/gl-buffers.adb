with Ada.Text_IO;


package body GL.Buffers is

   procedure Generate (Item : out Buffer_Array) is
   begin
      Item := (others => 0);
      glGenBuffers (Item'Length, GLuint (Item (Item'First))'Unrestricted_Access);
   end;

   function Generate return Buffer is
      Item : Buffer_Array (1 .. 1);
   begin
      Generate (Item);
      return Item (Item'First);
   end;

   procedure Bind (To : Buffer_Slot; Item : Buffer) is
   begin
      glBindBuffer (To'Enum_Rep, GLuint (Item));
   end;



   procedure Generic_Allocate_Initialized (Target : Buffer_Slot; Obj : Object; Usage : Buffer_Usage) is
   begin
      glBufferData (Target'Enum_Rep, GLsizeiptr (Data_Size (Obj) / System.Storage_Unit), Data_Address (Obj), Usage'Enum_Rep);
   end;

   procedure Allocate_Initialized_Bytes (Target : Buffer_Slot; Size_Bytes : Natural; Data : Address; Usage : Buffer_Usage) is
   begin
      glBufferData (Target'Enum_Rep, GLsizeiptr (Size_Bytes), Data, Usage'Enum_Rep);
   end;

   procedure Allocate_Initialized_Bits (Target : Buffer_Slot; Size_Bits : Natural; Data : Address; Usage : Buffer_Usage) is
   begin
       Allocate_Initialized_Bytes (Target, Size_Bits / Storage_Unit, Data, Usage);
   end;

   procedure Allocate_Uninitialized_Bytes (Target : Buffer_Slot; Size_Bytes : Natural; Usage : Buffer_Usage) is
   begin
      Allocate_Initialized_Bytes (Target, Size_Bytes, Null_Address, Usage);
   end;

   procedure Allocate_Uninitialized_Bits (Target : Buffer_Slot; Size_Bytes : Natural; Usage : Buffer_Usage) is
   begin
      Allocate_Initialized_Bits (Target, Size_Bytes, Null_Address, Usage);
   end;

   procedure Redefine_Bytes (Target : Buffer_Slot; Offset_Byte : Natural; Size_Byte : Natural; Data : System.Address) is
   begin
      glBufferSubData (Target'Enum_Rep, GLintptr (Offset_Byte), GLsizeiptr (Size_Byte), Data);
   end;

   procedure Redefine_Bits (Target : Buffer_Slot; Offset_Bits : Natural; Size_Bits : Natural; Data : System.Address) is
   begin
      glBufferSubData (Target'Enum_Rep, GLintptr (Offset_Bits / Storage_Unit), GLsizeiptr (Size_Bits / Storage_Unit), Data);
   end;

   procedure Clear (Item : Bitplane) is
   begin
      glClear (Item'Enum_Rep);
   end;


   procedure Put_Line_Fancy (Item : Buffer) is
      use Ada.Text_IO;
   begin
      Put_Line ("Buffer: " & Item'Img);
   end;

   function Identity (Item : Buffer) return GLuint is
   begin
      return GLuint (Item);
   end;

end;
