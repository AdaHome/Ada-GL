package GL.C.Initializations is

   type Loader is access function (Name : String) return Address;
   procedure Initialize (Load : Loader);

end;
