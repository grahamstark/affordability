with Text_Utils;
with Ada.Text_IO;
with Ada.Command_Line;
package body Model.Web_Constants is

   use Text_Utils;
   use Ada.Text_IO;

   paths : OSCR_Paths_Record;

   function Read_OSCR_Paths return OSCR_Paths_Record is
      paths : OSCR_Paths_Record;
   begin
      return paths;   
   end Read_OSCR_Paths;
   
   function OSCR_Paths return OSCR_Paths_Record is
      use UK_Key_Value_IO;
      file : Ada.Text_IO.File_Type;  
   begin
      if( Ada.Command_Line.Argument_Count > 0 ) then
         Open( file, In_File, Ada.Command_Line.Argument( 1 ));
      else
         Open( file, In_File, "oscr_paths.txt" );
      end if;
      paths.physical_server_root := Read( file, "physical_server_root" );
      paths.charts_driver_script := Read( file,  "charts_driver_script" );
      paths.datafile_directory := Read( file,  "datafile_directory" );
      paths.template_components_path := Read( file,  "template_components_path" );
      paths.work_dir := Read( file,  "work_dir" );
      paths.log_file_dir := Read( file, "log_file_dir" );
      paths.port := Read( file, "port" );
      paths.create_zip_file_and_static_images := Read( file, "create_zip_file_and_static_images" );
      paths.root := Read( file, "root" );
      Close( file );
      return paths;
   end OSCR_Paths;


begin
   paths := Read_OSCR_Paths;
end Model.Web_Constants;
