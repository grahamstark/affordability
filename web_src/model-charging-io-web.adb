with Ada.Calendar.Formatting;
with Ada.Calendar;   
with Ada.Containers;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;

with Model.Web_Constants;

with Templates_Parser;

with Text_Utils;
-- with Time_Format;

package body Model.Charging.IO.Web is

   use Ada.Text_IO;
   use UK_Key_Value_IO;
   use Text_Utils;
   
   --
   -- Make a file set with Tags filenames", "full-paths", "file-times", "names", names" descriptions
   -- for 
   --
   function Make_File_Set( prefix : String; username : Unbounded_String ) return Templates_Parser.Translate_Set is
   
   use Ada.Calendar;
   use Ada.Directories;
   use Templates_Parser;
      filenames       : Templates_Parser.Tag; 
      file_times      : Templates_Parser.Tag;
      full_paths      : Templates_Parser.Tag;
      descriptions    : Templates_Parser.Tag;
      names           : Templates_Parser.Tag;
      
      -- 
      -- borrowed from Rosetta Code: http://www.rosettacode.org/wiki/Walk_Directory_Tree#Ada
      --
      procedure Walk( dir_name : String; file_pattern : String ) is
         
         procedure Add_To_Tags( directory_entry : Directory_Entry_Type ) is
            file            : File_Type;
            regime          : Charging_Regime; 
            mtime           : Time;
         begin
            Put_Line( "opening " & Full_Name( directory_entry ));
            Open( file, in_file, Full_Name( directory_entry ));
            regime := Read( file, True ); -- peek at top 2 lines
            descriptions := descriptions & regime.description;
            names := names & regime.name;
            Close( file );
            mtime       := Modification_Time( directory_entry );
            filenames   := filenames & Base_Name( Simple_Name( directory_entry ));
            full_paths  := full_paths & Full_Name( directory_entry );
            file_times  := file_times & Ada.Calendar.Formatting.Image( mtime, False );
         exception
            when others => Put_Line( "failed to open " & Full_Name( directory_entry ));
         end Add_To_Tags;
         
         procedure Walk( directory_entry : Directory_Entry_Type ) is
         begin
            if Simple_Name( directory_entry ) /= "." and 
               then Simple_Name( directory_entry ) /= ".." then
               Walk( Full_Name( directory_entry ), file_pattern );
            end if;
         exception
            when Ada.IO_Exceptions.Name_Error => null;
         end Walk;
         
      begin
         Search( dir_name, file_pattern, (others => True), Add_To_Tags'Access );
         Search( dir_name, "", (Directory => True, others => False), Walk'Access );
         exception
            when Ada.IO_Exceptions.Name_Error => null; -- don't care if there's no such directory.
      end Walk;
      
      users_work_directory  : constant String := TS( Model.Web_Constants.OSCR_Paths.work_dir & 
                           Censor_String( username ) & DIR_SEPARATOR );
      translations    : Translate_Set;
   begin
      Walk( users_work_directory, Model.Web_Constants.REGIME_FILENAME );
      Insert( translations, Templates_Parser.Assoc( prefix & "-FILENAMES", filenames ) );
      Insert( translations, Templates_Parser.Assoc( prefix & "-FULL-PATHS", full_paths ) );
      Insert( translations, Templates_Parser.Assoc( prefix & "-FILE-TIMES", file_times ) );
      Insert( translations, Templates_Parser.Assoc( prefix & "-NAMES", names ) );
      Insert( translations, Templates_Parser.Assoc( prefix & "-DESCRIPTIONS", descriptions ) );
      return translations;
   end Make_File_Set;
   
   procedure Save( dirname : Unbounded_String; regime : Charging_Regime ) is
      filename : String := TS( dirname & Model.Web_Constants.REGIME_FILENAME );
      file : File_Type;
   begin
      Create( file, Out_File, filename );
      Write( file, regime );
      Close( file );
   end Save;
   
   procedure Load( full_filename : String; regime : out Charging_Regime ) is
      file : File_Type;
   begin
      Open( file, In_File, full_filename );
      regime := Read( file );
      Close( file );
   end Load;
   

end Model.Charging.IO.Web;
