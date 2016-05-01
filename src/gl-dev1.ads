with System;

package GL.Dev1 is

   type Program is new Integer;
   type Create_Program is access function return Program with Convention => StdCall;

   subtype Address is System.Address;

   type Proc_Name is (glCreateProgram, glLinkProgram);

   type Proc_Array is array (Proc_Name) of Address;


end;
