package body GL.Programs.Vertex_Attributes is

   function Get_Index_By_Name (From : Program; Name : String) return GL.Vertex_Attributes.Index is
   begin
      return GL.Vertex_Attributes.Get_Index_By_Name (Identity (From), Name);
   end;

end;
