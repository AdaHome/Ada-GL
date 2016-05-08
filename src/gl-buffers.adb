with Ada.Text_IO;


package body GL.Buffers is

   procedure Generate (Item : out Buffer_Array) is
      --pragma Suppress (Index_Check);
      --pragma Suppress (Range_Check);
      --pragma Suppress (All_Checks);
   begin
      --pragma Warnings (Off);
      --Item := (others => 0);
      --pragma Warnings (On);
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

   procedure Allocate (Target : Buffer_Slot; Size : Byte; Data : Address; Usage : Buffer_Usage) is
   begin
      glBufferData (Target'Enum_Rep, GLsizeiptr (Size), Data, Usage'Enum_Rep);
   end;

   procedure Allocate (Target : Buffer_Slot; Size : Byte; Usage : Buffer_Usage) is
   begin
      Allocate (Target, Size, Null_Address, Usage);
   end;

   procedure Allocate (Target : Buffer_Slot; Size : Bit; Data : Address; Usage : Buffer_Usage) is
   begin
       Allocate (Target, Byte (Size / Storage_Unit), Data, Usage);
   end;

   procedure Allocate (Target : Buffer_Slot; Size : Bit; Usage : Buffer_Usage) is
   begin
      Allocate (Target, Size, Null_Address, Usage);
   end;

   procedure Redefine (Target : Buffer_Slot; Offset : Byte; Size : Byte; Data : System.Address) is
   begin
      glBufferSubData (Target'Enum_Rep, GLintptr (Offset), GLsizeiptr (Size), Data);
   end;

   procedure Redefine (Target : Buffer_Slot; Offset : Bit; Size : Bit; Data : System.Address) is
   begin
      Redefine (Target, Byte (Offset / Storage_Unit), Byte (Size / Storage_Unit), Data);
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
