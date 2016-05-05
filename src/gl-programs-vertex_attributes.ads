with GL.Vertex_Attributes;

package GL.Programs.Vertex_Attributes is

   subtype Location is GL.Vertex_Attributes.Location;

   function Get (From : Program; Name : String) return Location;

end;
