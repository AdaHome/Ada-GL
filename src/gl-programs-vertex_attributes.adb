package body GL.Programs.Vertex_Attributes is




   function Get (From : Program; Name : String) return Location is
   begin
      return GL.Vertex_Attributes.Get (Identity (From), Name);
   end;

end;
