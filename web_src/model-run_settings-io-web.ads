with Ada.Text_IO;
with Ada.Strings.Unbounded;

package Model.Run_Settings.IO.Web is

   subtype File_Type is Ada.Text_IO.File_Type;
   use Ada.Strings.Unbounded;
   
   procedure Save( dirname : Unbounded_String; settings : Settings_Rec );
   procedure Load( dirname : Unbounded_String; settings : out Settings_Rec );
   
end Model.Run_Settings.IO.Web;
