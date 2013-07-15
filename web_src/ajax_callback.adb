--
--  $Author: graham_s $
--  $Date: 2009-06-18 09:57:13 +0100 (Thu, 18 Jun 2009) $
--  $Revision: 7463 $
--
with AWS.Log;
with AWS.Parameters;
with AWS.Session;
with Templates_Parser;

with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Format_Utils;
with Text_Utils;
with Model;
with Web_Utils;
with Model.Web_Commons;
with Model.Web_Constants;
       
package body Ajax_Callback is

   use Ada.Text_IO;
   

   function Home_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is

       use Model.Web_Commons;
       use Web_Utils;
       use Model.Web_Constants;
       use Ada.Strings.Unbounded;
       
       params : constant AWS.Parameters.List := AWS.Status.Parameters( Request );
       URI    : constant String := AWS.Status.URI( request );
       action : constant Unbounded_String := Get_Param( params, "action" );
       translations   : Templates_Parser.Translate_Set;
    begin
       put_line( "got action as " & To_String(action) );
       return Build_Input_Page( "ajax_test/index", translations );
    end Home_Page_Callback;

    use Model.UK_Tax_Utils;
    new_rbs : Rates_And_Bands;
    base_rbs : Rates_And_Bands;

   
   function Test_Callback(request : in AWS.Status.Data) return AWS.Response.Data is
       use Ada.Strings.Unbounded;
       use Web_Utils;
       use Model.Web_Constants;
       use Model.Web_Commons;
       
       params      : constant AWS.Parameters.List := AWS.Status.Parameters( Request );
       URI         : constant String              := AWS.Status.URI( request );
       action      : constant Unbounded_String    := Get_Param( params, "action" );
       row_s       : constant Unbounded_String    := Get_Param( params, "row" );
       rb_table    : Unbounded_String;
       row    : constant Positive := Positive'Value( To_String(row_s) );
   begin
      if( action = To_Unbounded_String("insert_after") ) then
         Set_Rate_And_Band( new_rbs, ( Rate=>0.0, Band=>0.0 ), row, False );
         put_line( "inserting after row " & row'Img );
      elsif( action = To_Unbounded_String("delete") ) then
         put_line( "deleting row " & row'Img );
         Delete_Rate_And_Band( new_rbs, row  );         
      end if;
      Rate_Bands_To_HTML( rb_table, new_rbs, base_rbs, "1" );
      put_line( "got action as " & To_String(action) & " row = " & To_String(row_s) );
      put_line( To_String( params ));
      return AWS.Response.Build ( "text/html", rb_table );
   end Test_Callback;

begin

   Set_Rate_And_Band( new_rbs, ( Rate=>0.20, Band=>19_000.0 ), 1 );
   Set_Rate_And_Band( new_rbs, ( Rate=>0.40, Band=>9999_000.0 ), 2 );
   Set_Rate_And_Band( base_rbs, ( Rate=>0.20, Band=>19_000.0 ), 1 );
   Set_Rate_And_Band( base_rbs, ( Rate=>0.40, Band=>9999_000.0 ), 2 );

end Ajax_Callback;
