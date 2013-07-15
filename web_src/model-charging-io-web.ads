with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Templates_Parser;

package Model.Charging.IO.Web is

   subtype File_Type is Ada.Text_IO.File_Type;   
   use Ada.Strings.Unbounded;
   --
   -- Make a file set with Tags filenames", "full-paths", "file-times", "names", names" descriptions
   -- for 
   --
   function Make_File_Set( prefix : String; username : Unbounded_String ) return Templates_Parser.Translate_Set;
   procedure Save( dirname : Unbounded_String; regime : Charging_Regime);
   procedure Load( full_filename : String; regime : out Charging_Regime );
    

end Model.Charging.IO.Web;
