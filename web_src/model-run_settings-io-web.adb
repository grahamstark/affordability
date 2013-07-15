with Ada.Text_IO;
with Text_Utils;
with Model.Web_Constants;

package body Model.Run_Settings.IO.Web is
   use Ada.Text_IO;
   use Text_Utils;
   
   procedure Save( dirname : Unbounded_String; settings : Settings_Rec ) is
      filename : String := TS( dirname & DIR_SEPARATOR & Model.Web_Constants.RUN_SETTINGS_FILENAME );
      file : File_Type;
   begin
      Create( file, Out_File, filename );
      Write( file, settings.inc_ctl );
      Close( file );
   end Save;
   
   procedure Load( dirname : Unbounded_String; settings : out Settings_Rec ) is
      filename : String := TS( dirname & DIR_SEPARATOR & Model.Web_Constants.RUN_SETTINGS_FILENAME );
      file : File_Type;
   begin
      Put_Line( "opening " & filename );
      Open( file, In_File, filename );
      settings.inc_ctl := Read( file );
      Close( file );
   end Load;

end Model.Run_Settings.IO.Web;
