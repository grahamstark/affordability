--
--  $Author: graham_s $
--  $Date: 2009-06-18 09:57:13 +0100 (Thu, 18 Jun 2009) $
--  $Revision: 7463 $
--
with legal_aid_web_commons; use legal_aid_web_commons;
with text_utils;
package file_lister is

        use text_utils;

        function make_full_name ( username : String; file_name : Bounded_String; ext : String ) return String;

        procedure make_parameter_file_list
               (root_directory, username : String;
                trans                    : in out LA_Translate_Table;
                insert_Start_Position    : Integer);
                
        procedure make_file_list
               (root_directory        : String;
                search_pattern        : String;
                trans                 : in out LA_Translate_Table;
                insert_Start_Position : Integer);

end file_lister;
