--
--  $Author: graham_s $
--  $Date: 2009-06-18 09:57:13 +0100 (Thu, 18 Jun 2009) $
--  $Revision: 7463 $
--
with Templates_Parser;
with legal_aid_web_commons; 
with Ada.Calendar;          
with Ada.Directories;       
with Text_IO;               
with Text_Utils;
with Ada.Strings.Unbounded;
with web_constants;

package body File_Lister is

   use legal_aid_web_commons;
   use Ada.Calendar;
   use Ada.Directories;
   use Ada.Strings.Unbounded;
   use Templates_Parser;
   

   function Make_Full_name
     (username  : String;
      file_name : Bounded_String;
      ext       : String)
      return      String
   is
   begin
      return web_constants.SERVER_ROOT & username & "/" & To_String (file_name) & "." & ext;
   end Make_Full_Name;

   function Make_File_Set( username : Unbounded_String ) return Translate_Set is
   
   
      filenames, file_times, full_paths : Templates_Parser.Tag;
      search_pattern  : constant String := "";
      search          : Search_Type;
      directory_entry : Directory_Entry_Type;
      FILES_FILTER    : constant Filter_Type := (Ordinary_File => True, others => False);
      mtime           : Time;
      time_string     : Unbounded_String;
      translations    : Translate_Set;
   begin
      --  FIXME: the delimiter must be defined somewhere
      Start_Search (search, root_directory, search_pattern, FILES_FILTER);
      while More_Entries (search) loop
         Get_Next_Entry (search, directory_entry);
         mtime       := Modification_Time (directory_entry);
         filenames   := filenames & Base_Name (Simple_Name (directory_entry));
         full_paths  := full_paths & Full_Name (directory_entry);
         time_string :=
            To_Bounded_String
              (Year (mtime)'Img & '-' & Month (mtime)'Img & "-" & Day (mtime)'Img);
         file_times  := file_times & To_String (time_string);

         --  Text_IO.Put ("adding filename " & Full_Name (directory_entry));
      end loop;
      Insert( translations, Templates_Parser.Assoc( "filenames", filenames ) );
      Insert( translations, Templates_Parser.Assoc( "full-paths", full_paths ) );
      Insert( translations, Templates_Parser.Assoc ("file-times", file_times ) );
      return translations;
   end Make_File_List;
   
   procedure Make_Parameter_File_List
      (root_directory, username : String;
      trans                    : in out LA_Translate_Table;
      insert_Start_Position    : Integer)
   is
   begin
      Make_File_List
        (root_directory & "/" & username & "/",
         "*.bpr",
         trans,
         insert_Start_Position);
   end Make_Parameter_File_List;



end Lile_Lister;
