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

   procedure Bind (Item : Buffer; Slot : Buffer_Slot) is
   begin
      glBindBuffer (GLuint (Item), Slot'Enum_Rep);
   end;

   procedure Allocate (Target : Buffer_Slot; Size : Natural; Data : System.Address; Usage : Buffer_Usage) is
   begin
      glBufferData (Target'Enum_Rep, GLsizeiptr (Size), Data, Usage'Enum_Rep);
   end;

   procedure Allocate (Target : Buffer_Slot; Size : Natural; Usage : Buffer_Usage) is
   begin
      Allocate (Target, Size, System.Null_Address, Usage);
   end;

   procedure Redefine (Target : Buffer_Slot; Offset : Natural; Size : Natural; Data : System.Address) is
   begin
      glBufferSubData (Target'Enum_Rep, GLintptr (Offset), GLsizeiptr (Size), Data);
   end;



end;
