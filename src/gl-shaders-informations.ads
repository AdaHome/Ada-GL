package GL.Shaders.Informations is

   type Shader_Info is
     (
      Type_Info,
      Delete_Info,
      Compile_Info,
      Log_Length_Info,
      Source_Length_Info
     );

private

   for Shader_Info use
     (
      Type_Info => GL_SHADER_TYPE,
      Delete_Info => GL_DELETE_STATUS,
      Compile_Info => GL_COMPILE_STATUS,
      Log_Length_Info => GL_INFO_LOG_LENGTH,
      Source_Length_Info => GL_SHADER_SOURCE_LENGTH
     );

end;
