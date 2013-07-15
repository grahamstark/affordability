------------------------------------------------------------------------------
--                                                                          --
--  Handlers for each of OSCR's callbacks, plus some support functions      --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
--                                                                          --
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Containers;
with Ada.Strings;
with Ada.Characters.Handling;

with AWS.Response;
with AWS.Response.Set;
with AWS.Mime;
with AWS.Log;
with AWS.Resources;
with AWS.Config;
with AWS.Resources;
with AWS.Server;
with AWS.Messages;

with GNAT.Regexp;

with Html_Utils;
with Model.Web_Constants;
with Templates_Parser;
with Text_Utils;
with Utils;
with Web_Utils;
with T_Utils.Web_IO;

with Model.Charging.IO.Web;
with Model.Charging.IO;
with Model.Charging;
with Model.Household;
with model.Income_Measures;
with model.Income_Measure_Types;
with Model.OSCR_Output.Generator;
with Model.Run_Settings;
with Model.Run_Settings.IO.Web;
with Model.Charging.Buffer;
with OSCR_Users;

package body OSCR_Callbacks is

   use Ada.Text_IO;
   use AWS.Log;
   package u renames OSCR_Users;
   package rs   renames Model.Run_Settings;
   package mwcn renames Model.Web_Constants;
   package mwcm renames Model.Web_Commons;
   package mhh  renames Model.Household;
   package moo  renames Model.OSCR_Output;
   package rp   renames Model.Charging.Charging_Regime_Package;
   package ap   renames Model.Charging.Application_Package;
   package mc   renames Model.Charging;
   package mcb  renames Model.Charging.Buffer;
   package mcw  renames Model.Charging.IO.Web;
   package mrsw renames Model.Run_Settings.IO.Web;
   package ach  renames Ada.Characters.Handling;
   
   -- package mim  renames Model.Income_Measures;
   package mimt  renames Model.Income_Measure_Types;
   
   function Serve_Static_Resource( Request : in AWS.Status.Data ) return AWS.Response.Data is
      use Ada.Strings;
      WWW_Root : String renames AWS.Config.WWW_Root
        (AWS.Server.Config (AWS.Server.Get_Current.all));
      URI      : constant String := AWS.Status.URI (Request);
      root     : constant String := TS( mwcn.OSCR_Paths.root );      
      filename : constant String := WWW_Root & URI ( root'Last .. URI'Last);
   begin
      Put_Line( "serving |"& filename & "| " );
      if AWS.Resources.Is_Regular_File( filename ) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Content_Type( filename ),
            Filename     => filename);
      else          
         return AWS.Response.Acknowledge
              (AWS.Messages.S404,
               "<p>The page '"
               --  Replace HTML control characters to the HTML inactive symbols
               --  to avoid correct HTML pages initiated from the client side.
               --  See http://www.securityfocus.com/bid/7596
               & Fixed.Translate (URI, Maps.To_Mapping ("<>&", "{}@"))
               & "' was not found.");
      end if;         
   end Serve_Static_Resource;
   
   function Create_Dirname_For_Run(
      username    : Unbounded_String;
      regime_name : Unbounded_String ) return Unbounded_String is
      dirname     : Unbounded_String := mwcn.OSCR_Paths.work_dir;
   begin
      return
         dirname &
         Censor_String( username ) &
         DIR_SEPARATOR &
         Censor_String( regime_name ) &
         DIR_SEPARATOR;
   end  Create_Dirname_For_Run;

   function Create_Dirname_For_Run( session_id : AWS.Session.Id ) return Unbounded_String is
      user : u.User_Type := u.User_Session_Data.Get( session_id, mwcn.SESSION_USER_ID );
      regime : mc.Charging_Regime := Costs_Model_Session_Data.Get(
         session_id,
         mwcn.SESSION_COST_MODEL );
   begin
      return Create_Dirname_For_Run( user.username, regime.name );
   end Create_Dirname_For_Run;

   function Handle_Login( request : in AWS.Status.Data ) return u.Login_Result is
      use type u.User_Type;

      username   : constant Unbounded_String := TuS(AWS.Status.Authorization_Name( request ));
      password   : constant Unbounded_String := TuS(AWS.Status.Authorization_Password (Request));
      session_id : constant AWS.Session.Id := AWS.Status.Session( request );
      this_user  : u.User_Type;
      result     : u.Login_Result;
      root : constant String := TS( mwcn.OSCR_Paths.root );

   begin
      if( username = "" )then
          result.response := AWS.Response.Authenticate( mwcn.DOMAIN, AWS.Response.Basic );
      end if;
      this_user := u.Validate( username, password );
      if( this_user = u.INVALID_USER )then
          result.response := AWS.Response.Authenticate( mwcn.DOMAIN, AWS.Response.Basic );
      else
         result.user := this_user;
         result.validated := True;
         if( u.User_Session_Data.Get( session_id, mwcn.SESSION_USER_ID ) = u.INVALID_USER )then
            --
            --  no user in the session; put the user record in the session
            --
            u.User_Session_Data.Set( session_id, mwcn.SESSION_USER_ID, this_user );
            declare
               run_state     : rs.State_Type;
               run_settings  : rs.Settings_Rec;
               output_tables : moo.Table_Set;
               control       :  rs.Settings_Rec;
               regime_buff   : constant mcb.Charging_Regime_Buffer := mcb.Make_Initialised_Regime;
               regime        : constant mc.Charging_Regime := mcb.Map_To( regime_buff );               
            begin
               Run_State_Session_Data.Set( session_id, mwcn.SESSION_RUN_STATE, rs.BLANK_STATE_TYPE );
               Run_Settings_Session_Data.Set( session_id, mwcn.SESSION_RUN_SETTINGS, run_settings );
               Tables_Session_Data.Set( session_id, mwcn.SESSION_OUTPUT_TABLES, output_tables );
               Costs_Model_Session_Data.Set( session_id, mwcn.SESSION_COST_MODEL, regime );
               Costs_Model_Buffer_Session_Data.Set( session_id, mwcn.SESSION_COST_MODEL_BUFFER, regime_buff );
            end;
            result.new_session := True;
            result.response := AWS.Response.URL( Location => root );
         end if;
      end if;
      return result;
   end Handle_Login;

   function Create_Directory_For_Run(
      session_id : AWS.Session.Id;
      user : u.User_Type;
      regime : mc.Charging_Regime ) return Unbounded_String is
      dirname : Unbounded_String := mwcn.OSCR_Paths.work_dir;
   begin
      dirname := Create_Dirname_For_Run( user.username, regime.name );
      Utils.Make_Directory_Path( TS(dirname) );
      AWS.Session.Set( session_id, mwcn.SESSION_WORK_DIR, TS(dirname) );
      return dirname;
   end  Create_Directory_For_Run;

   function Logout_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      session_id : constant AWS.Session.Id := AWS.Status.Session( request );
      d : AWS.Response.Data;
   begin
      AWS.Response.Set.Clear_Session( d );
      AWS.Session.Delete( session_id );
      return AWS.Response.URL( Location => TS(mwcn.OSCR_Paths.root) );
   end Logout_Callback;

   function Popup_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use OSCR_Users;
      translations : Translate_Set;
      params           : constant AWS.Parameters.List := AWS.Status.Parameters( Request );
      graph_name       : constant String := AWS.Parameters.Get( params, "file", 1 );
      regime_name      : constant String := AWS.Parameters.Get( params, "regime", 1 ); -- FIXME: unused
      logresult        : constant u.Login_Result := Handle_Login( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      application_name : constant String := AWS.Parameters.Get( params, "application", 1 );
      application_text : constant String := AWS.Parameters.Get( params, "application_text", 1 );
      regime_text      : constant String := AWS.Parameters.Get( params, "regime_text", 1 );
      root : constant String := TS( mwcn.OSCR_Paths.root );
   begin
      Put_Line( "Popup_Callback: chart = |" & graph_name & "| regime_name |" & regime_name & "| application_name |" & application_name );
      Put_Line( "Popup_Callback: regime_text " & regime_text & " application_text " & application_text );
      --
      -- fixme: this is overkill
      --
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      Insert( translations, Assoc( "ROOT", root )); 

      Insert( translations, Assoc( "CHART", graph_name ));
      Insert( translations, Assoc( "REGIME", regime_name ));
      Insert( translations, Assoc( "APPLICATION", application_name ));
      Insert( translations, Assoc( "APPLICATION-TEXT", application_text ));
      Insert( translations, Assoc( "REGIME-TEXT", regime_text ));
      Insert( translations, Assoc( "ALT", "Graphic of " & regime_name & " : " & application_name ));
      Insert( translations, Assoc( "USERNAME", logresult.user.title ));
      Insert( translations, Assoc( "IS-SVG", False )); -- logresult.user.preferences( use_svg_graphics )) );
      return Web_Utils.Build_Input_Page(
         mwcn.OSCR_Paths.template_components_path & "popup",
         translations );

   end Popup_Callback;

   function Serve_File_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use OSCR_Users;
      filename : Unbounded_String;
      -- logresult : constant u.Login_Result := Handle_Login( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      graph_name : constant String := AWS.Parameters.Get( params, "file", 1 );
      application_name : constant String := AWS.Parameters.Get( params, "application", 1 );
      regime_name      : constant String := AWS.Parameters.Get( params, "regime", 1 ); -- FIXME: unused
      extension        : constant String := AWS.Parameters.Get( params, "type", 1 ); -- FIXME: unused
      user : User_Type := User_Session_Data.Get( session_id, mwcn.SESSION_USER_ID ); 
   begin
      --
      -- fixme: this is overkill
      --
      -- if(( not logresult.validated ) or ( logresult.new_session )) then
         -- return logresult.response;
      -- end if;
      if( user = INVALID_USER ) then
         return AWS.Response.URL( Location => TS(mwcn.OSCR_Paths.root) );
      end if;
      filename := Create_Dirname_For_Run( user.username, TuS(regime_name) ) &
           application_name & DIR_SEPARATOR & graph_name & "." & extension;
      Put_Line( "Chart_Callback: searching for " & TS( filename ));
      if( extension = "svg" ) then
         return AWS.Response.File( mwcn.MIME_TYPE_IMAGE_SVG, TS( filename ));
      elsif( extension = "csv" ) then
         declare
            save_filename : constant String := regime_name & "_" & application_name & ".csv";
         begin
            return AWS.Response.File(
               Content_Type  => mwcn.MIME_TYPE_TEXT_CSV,
               Filename      => TS( filename ),
               Disposition   => AWS.Response.Attachment,
               User_Filename => save_filename );
         end;
      elsif( extension = "zip" ) then
         declare
            save_filename : constant String := regime_name & "_" & application_name & ".zip";
         begin
            return AWS.Response.File(
               Content_Type  => AWS.Mime.Application_Zip,
               Filename      => TS( filename ),
               Disposition   => AWS.Response.Attachment,
               User_Filename => save_filename );
         end;
      end if;
      return AWS.Response.File( AWS.Mime.Image_Png, TS( filename ));
   end Serve_File_Callback;
   
   function Run_Settings_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      logresult        : constant u.Login_Result := Handle_Login( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
   
   use Templates_Parser;  
      params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      run_settings     : rs.Settings_Rec := Run_Settings_Session_Data.Get(
         session_id,
         mwcn.SESSION_RUN_SETTINGS );
      package Slots_Web is new mimt.Slots_Package.Web_IO;
      slot_box : Unbounded_String; 
         
      package Aggregates_Web is new mhh.Agg_Level_Package.Web_IO;
      translations : Translate_Set;
      action : String := AWS.Parameters.Get( params, "action", 1 );
      dirname : Unbounded_String := Create_Dirname_For_Run( session_id );
      root : constant String := TS( mwcn.OSCR_Paths.root );
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      if( action = "Save" ) then
         run_settings.inc_ctl.summary_slot := mimt.Slot_Range'Value( 
            AWS.Parameters.Get( params, "summary_slot", 1 ));
         run_settings.inc_ctl.aggregate_incomes_to := mhh.Aggregation_Level'Value(
            ach.To_Upper( AWS.Parameters.Get( params, "aggregate_incomes_to", 1 )));
         -- run_settings.inc_ctl.produce_results_for := mhh.Aggregation_Level'Value(
         --   ach.To_Upper( AWS.Parameters.Get( params, "produce_results_for", 1 )));
         run_settings.inc_ctl.include_child_care_costs_in_allowances := 
            AWS.Parameters.Get( params, "include_child_care_costs_in_allowances", 1 ) = "on";
         run_settings.inc_ctl.include_housing := 
            AWS.Parameters.Get( params, "include_housing", 1 ) = "on";
         Run_Settings_Session_Data.Set(
            session_id,
            mwcn.SESSION_RUN_SETTINGS, run_settings );
         mrsw.Save( dirname, run_settings );
         Put_Line( "run_settings.inc_ctl.aggregate_incomes_to = " & run_settings.inc_ctl.aggregate_incomes_to'Img );
      end if;
      slot_box := 
         Slots_Web.Make_Select_Elements( 
            run_settings.inc_ctl.summary_slot, 
            mimt.Pretty_Print'Access, 
            mimt.Slots_Package.Construct_Set( 1, 4 ) );
      Insert( translations, Assoc( "SLOT-SELECTS", slot_box ));
      if( run_settings.inc_ctl.include_child_care_costs_in_allowances ) then
         Insert( translations, 
            Assoc( "INCLUDE-CHILD-CARE-COSTS-IN-ALLOWANCES", " checked='checked' " ));
      end if;
      Insert( translations, Assoc( "ROOT", root ));
      if( run_settings.inc_ctl.include_housing ) then
         Insert( translations, Assoc( "INCLUDE-HOUSING", " checked='checked' " ));
      end if;
      
      Insert( translations, 
         Assoc( "AGGREGATE-SELECTS-INCOMES", 
            Aggregates_Web.Make_Select_Elements(
               run_settings.inc_ctl.aggregate_incomes_to,
               mhh.Pretty_Print'Access )));
      Insert( translations, 
         Assoc( "AGGREGATE-SELECTS-RESULTS", 
            Aggregates_Web.Make_Select_Elements(
               run_settings.inc_ctl.produce_results_for,
               mhh.Pretty_Print'Access )));
      Insert( translations, Assoc( "RAND", Utils.Random_String ));
       
      return Web_Utils.Build_Input_Page(
         mwcn.OSCR_Paths.template_components_path & "input_run_settings",
         translations );
   end Run_Settings_Callback;

   function Output_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use type Ada.Containers.Count_Type;
      URI              : constant String := AWS.Status.URI( request );
      params           : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      logresult        : constant u.Login_Result := Handle_Login( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      application_name : Unbounded_String := TuS(AWS.Parameters.Get( params, "application", 1 ));
      regime_name      : constant String := AWS.Parameters.Get( params, "regime", 1 ); -- FIXME: unused
      regime           : constant mc.Charging_Regime := Costs_Model_Session_Data.Get(
         session_id,
         mwcn.SESSION_COST_MODEL );
      filename : Unbounded_String;
      default_application : mc.Application_Type;
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      if( application_name = "" ) then -- assume we're on the 1st
         if( ap.Length( regime.applications ) > 0 ) then
            default_application := ap.Element( regime.applications, 1 );
            application_name := Censor_String( default_application.name );
         end if;
      end if;
      filename := Create_Dirname_For_Run( logresult.user.username, regime.name );
      if( Length( application_name ) > 0 ) then
         filename := filename & application_name & DIR_SEPARATOR;
      end if;
      filename := filename & "output.html";
      Write( mwcm.logger,  "Output_Callback:: looking for " & TS( filename ));
      Put_Line( "Output_Callback:: looking for " & TS( filename ));
         
      if( Ada.Directories.Exists( TS(filename) )) then
         Write( mwcm.logger,  "Output_Callback:: uri " & uri );
         return AWS.Response.File( AWS.MIME.Text_HTML, TS( filename ));
      else
         return AWS.Response.File( AWS.MIME.Text_HTML, TS(mwcn.OSCR_Paths.template_components_path) & "no_output_yet.html" );
      end if;
   end Output_Callback;

   function Ajax_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use type rs.State_Type;
   use Model.Charging.Buffer;
   use Ada.Characters.Handling;
      params         : constant AWS.Parameters.List := AWS.Status.Parameters( request );
      translations   : Translate_Set;
      session_id     : constant AWS.Session.Id := AWS.Status.Session( request );
      action_str     : constant String := AWS.Parameters.Get( params, "action", 1 );
      run_id         : constant String := AWS.Parameters.Get( params, "run_id", 1 );
      atype          : constant String := AWS.Parameters.Get( params, "type", 1 );
      regime         : mc.Charging_Regime := Costs_Model_Session_Data.Get( session_id, mwcn.SESSION_COST_MODEL );
      regime_buff    : Charging_Regime_Buffer :=  Costs_Model_Buffer_Session_Data.Get( session_id, mwcn.SESSION_COST_MODEL_BUFFER );
      outs           : Unbounded_String;
      action         : constant Ajax_Action_Type := Ajax_Action_Type'Value( To_Upper(action_str) );
      application_id : constant Unbounded_String := TuS(AWS.Parameters.Get( params, "application_id", 1 ));
      block          : Block_Type;
      application_buff : Application_Buffer_Type := Get_Application( regime_buff, application_id );
   begin
      Put_Line( "action is " & action'Img );
      if( action = error_check ) then
         if( Error_Count( regime_buff ) > 0 ) then
            Put_Line( "making error messages " );
            outs := TuS("Warning: there are errors on the page; you won't be able to run the model until these are corrected");
         end if;
      else
         block := Block_Type'Value( To_Upper(atype) );
         case block is
            when charge =>
               declare
                  target_id        : constant Unbounded_String := TuS(AWS.Parameters.Get( params, "target_id", 1 ));
                  charge_id        : constant Unbounded_String := TuS(AWS.Parameters.Get( params, "charge_id", 1 ));
                  target_buff      : Target_Buffer_Type := Get_Target( application_buff, target_id );
                  charge_buff      : Charges_Buffer_Type := Get_Charge( target_buff, charge_id );
                  new_charge       : Charges_Buffer_Type := 
                     Copy_Charge( Charges_Buffer_Session_Data.Get( session_id, mwcn.SESSION_CHARGE_COPY ), True );
               begin
                  new_charge.id := Tus( Utils.Random_String );
                  case action is
                     when insert_above => Load_Target( params, application_id, target_buff ); -- save everything first
                                          Insert_Or_Replace_Charge( target_buff, charge_id, before, new_charge );
                     when insert_below => Load_Target( params, application_id, target_buff );
                                          Insert_Or_Replace_Charge( target_buff, charge_id, after, new_charge );
                     when delete       => Delete_Charge( target_buff, charge_id );
                     when save         => Load_Charge( params, application_id, target_id, charge_buff );
                                          Insert_Or_Replace_Charge( target_buff, charge_id, replace, charge_buff );
                     when copy         => Load_Charge( params, application_id, target_id, charge_buff );
                                          Charges_Buffer_Session_Data.Set( session_id, mwcn.SESSION_CHARGE_COPY, charge_buff );
                                          Insert_Or_Replace_Charge( target_buff, charge_id, replace, charge_buff );
                     when error_check  => null;
                  end case;
                  Insert_Or_Replace_Target( application_buff, target_id, replace, target_buff );
                  Insert_Or_Replace_Application( regime_buff, application_id, replace, application_buff );
                  outs := Charges_To_HTML( application_id, target_id, target_buff.charges );
               end;
            when target =>
               declare
                  target_id        : constant Unbounded_String := TuS(AWS.Parameters.Get( params, "target_id", 1 ));
                  target_buff      : Target_Buffer_Type := Get_Target( application_buff, target_id );
                  new_target       : Target_Buffer_Type := 
                     Copy_Target( Target_Buffer_Session_Data.Get( session_id, mwcn.SESSION_TARGET_COPY ), True );
               begin
                  case action is
                     when insert_above => -- Load_Application( params, application_buff ); -- save everything surrounding first
                                          Insert_Or_Replace_Target( application_buff, target_id, before, new_target );
                     when insert_below => -- Load_Application( params, application_buff );
                                          Insert_Or_Replace_Target( application_buff, target_id, after, new_target );
                     when delete       => Delete_Target( application_buff, target_id );
                     when save         => Load_Target( params, application_id, target_buff );
                                          Insert_Or_Replace_Target( application_buff, target_id, replace, target_buff );
                     when copy         => Load_Target( params, application_id, target_buff );
                                          Target_Buffer_Session_Data.Set( session_id, mwcn.SESSION_TARGET_COPY, target_buff );
                                          Insert_Or_Replace_Target( application_buff, target_id, replace, target_buff );
                     when error_check  => null;
                  end case;
                  Insert_Or_Replace_Application( regime_buff, application_id, replace, application_buff );
                  outs := Targets_To_HTML( application_id, application_buff.targets );
               end;
            when application =>
               declare
                  new_application  : Application_Buffer_Type := 
                     Copy_Application( Application_Buffer_Session_Data.Get( session_id, mwcn.SESSION_APPLICATION_COPY ), True );
                  num_applications : constant Natural := Natural(  Application_Buffer_Package.Length( regime_buff.applications ));
                  target_id        : Unbounded_String;
               begin
                  Put_Line( "editing application; action is " & action'Img & "application_id = " & TS(application_id) );
                  case action is
                     when insert_above => Insert_Or_Replace_Application( regime_buff, application_id, before, new_application );
                                          target_id := new_application.id;
                     when insert_below => Insert_Or_Replace_Application( regime_buff, application_id, after, new_application );
                                          target_id := new_application.id;
                     when delete       => Delete_Application( regime_buff, application_id );
                                          target_id := Application_Buffer_Package.Element( regime_buff.applications, 1 ).id; 
                     when save         => Load_Regime( params, regime_buff ); -- FIXME: spartially redundant
                                          Load_Application( params, application_buff );
                                          target_id := application_buff.id;
                     when copy         => Load_Application( params, application_buff );
                                          Application_Buffer_Session_Data.Set( session_id, mwcn.SESSION_APPLICATION_COPY, application_buff );
                                          target_id := application_buff.id;
                     when error_check  => null;
                  end case;
                  Insert_Or_Replace_Application( regime_buff, application_id, replace, application_buff );
                  outs := Regime_To_HTML( regime_buff, target_id );
               end;
         end case;
         Costs_Model_Buffer_Session_Data.Set( session_id, mwcn.SESSION_COST_MODEL_BUFFER, regime_buff );
      end if;
      return AWS.Response.Build( "text/html", outs );
   end Ajax_Callback;
   
   procedure Rename_Working_Dir( 
      username : Unbounded_String;
      last_name : Unbounded_String; 
      new_name : Unbounded_String ) is
   use Ada.Directories;
      old_full_name : constant String := TS( Create_Dirname_For_Run( username, last_name ));
      new_full_name : constant String := TS( Create_Dirname_For_Run( username, new_name ));
   begin
      if((Length( last_name ) > 0 ) and then Exists( old_full_name )) then
         Rename( old_full_name, new_full_name );
      else
         Create_Directory( new_full_name );
      end if;
   end Rename_Working_Dir;

   function Input_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use type rs.State_Type;
   use Model.Charging.Buffer;
      params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      translations : Translate_Set;
      logresult : constant u.Login_Result := Handle_Login( request );
      session_id : constant AWS.Session.Id := AWS.Status.Session( request );
      run_state : rs.State_Type;
      job_is_running : Boolean;
      action  : constant String := AWS.Parameters.Get( params, "action", 1 );
      run_id  : constant String := AWS.Parameters.Get( params, "run_id", 1 );
      new_random_string : constant String := Utils.Random_String;
      run_settings :  rs.Settings_Rec;
      regime  : mc.Charging_Regime;
      dirname : Unbounded_String;
      application_buffer  : Application_Buffer_Type;
      regime_buff : Charging_Regime_Buffer;
      app_text : Unbounded_String;
      application_id : Unbounded_String;
      last_name : Unbounded_String;
      num_errors : Natural := 0;
      root : constant String := TS( mwcn.OSCR_Paths.root );
   begin
      Write( mwcm.logger,  "got action as = |" & action & "| " );
      Put_Line( "Input_Page_Callback entered" );
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      run_settings := Run_Settings_Session_Data.Get(
         session_id,
         mwcn.SESSION_RUN_SETTINGS );
      regime := Costs_Model_Session_Data.Get(
         session_id,
         mwcn.SESSION_COST_MODEL );
      regime_buff := Costs_Model_Buffer_Session_Data.Get(
         session_id,
         mwcn.SESSION_COST_MODEL_BUFFER );
      run_state := Run_State_Session_Data.Get(
         session_id,
         mwcn.SESSION_RUN_STATE );
      job_is_running := run_state /= rs.BLANK_STATE_TYPE;
      Write( mwcm.logger,  "job_is_running " & job_is_running'Img );
      Put_Line( "job_is_running " & job_is_running'Img );
      AWS.Session.Set( session_id, mwcn.SESSION_ABORTING, False );
      Put_Line( "got regime buffer "  );
      if( AWS.Parameters.Exist( params, "application_id" )) then
         application_id := TuS(AWS.Parameters.Get( params, "application_id", 1 ));
         application_buffer := Get_Application( regime_buff, application_id );
      else
         application_buffer := Application_Buffer_Package.Element( regime_buff.applications, 1 );
         application_id := application_buffer.id;
      end if;
      if( action = "Run" ) then
         Put_Line( "got application buffer id is |" & TS( application_id ));
         last_name := regime_buff.name.buffer;
         Load_Regime( params, regime_buff );
         num_errors := Error_Count( regime_buff );
         if( not job_is_running ) then -- FIXME: shouldn't need job_is_running since we're just queueing it here
            if( num_errors = 0 ) then 
               if( last_name /= regime_buff.name.buffer ) then
                  Rename_Working_Dir( logresult.user.username, last_name, regime_buff.name.buffer );
               end if;
               Insert( translations, Assoc( "MAIN-ERROR-MESSAGE", "" ));
               regime := Map_To( regime_buff );
               Write( mwcm.logger,  "running" );
               dirname := Create_Directory_For_Run( session_id, logresult.user, regime );
               Write( mwcm.logger,  "made dir " & TS(dirname) );
               job_queue.Enque( session_id, regime, run_settings, dirname );
               run_state.phase := rs.not_started;
               run_state.message := TuS( "Job is in the queue to run" );
               Run_State_Session_Data.Set( session_id, mwcn.SESSION_RUN_STATE, run_state );
               Write( mwcm.logger,  "enqueued" );
               job_is_running := True;
               Insert( translations, Assoc( "RUN_ID", new_random_string ));
               Costs_Model_Session_Data.Set(
                  session_id,
                  mwcn.SESSION_COST_MODEL, regime  );
               Costs_Model_Buffer_Session_Data.Set(
                  session_id,
                  mwcn.SESSION_COST_MODEL_BUFFER, regime_buff );
               mcw.Save( dirname, regime );
               mrsw.Save( dirname, run_settings );
            else
               Insert( translations, Assoc( "MAIN-ERROR-MESSAGE", "<div id='error_section' class='error_section'>There are " & 
                    Natural'Image( num_errors ) & 
                  " errors on this page, highlighted in red. Please correct these before trying to commit your changes</div>" ));
            end if;
         end if;
      elsif( action = "Save" ) then
         Put_Line( "got application buffer id is |" & TS( application_id ));
         last_name := regime_buff.name.buffer;            
         Load_Regime( params, regime_buff );            
         num_errors := Error_Count( regime_buff );
         if( num_errors = 0 ) then
            if( last_name /= regime_buff.name.buffer ) then
               Rename_Working_Dir( logresult.user.username, last_name, regime_buff.name.buffer );
            end if;
            Insert( translations, Assoc( "MAIN-ERROR-MESSAGE", "" ));
            regime := Map_To( regime_buff );
            dirname := Create_Directory_For_Run(  session_id, logresult.user, regime );
            Write( mwcm.logger,  "saving to " & TS( dirname ));
            mcw.Save( dirname, regime );
            mrsw.Save( dirname, run_settings ); 
            -- save run settings in case we've not saved before so the pair of files is always in the working dir
            Costs_Model_Session_Data.Set(
               session_id,
               mwcn.SESSION_COST_MODEL, regime  );
            Costs_Model_Buffer_Session_Data.Set(
               session_id,
               mwcn.SESSION_COST_MODEL_BUFFER, regime_buff );
         else
            Put_Line( "writing error message " );
            Insert( translations, Assoc( "MAIN-ERROR-MESSAGE", "<div id='error_section' class='error_section'>There are " & 
                  Natural'Image( num_errors ) & 
                  " errors on this page, highlighted in red. Please correct these before trying to run the model</div>" ));
         end if;
         
      elsif( action = "Abort" ) then
         Write( mwcm.logger,  "aborting" );
         AWS.Session.Set( session_id, mwcn.SESSION_ABORTING, True );
      end if;
      Insert( translations, Assoc( "ROOT", root )); 
      Insert( translations, Assoc( "JOB_IS_RUNNING", job_is_running ));
      Insert( translations, Assoc( "USERNAME", logresult.user.title ));
      Insert( translations, Assoc( "REGIME", Regime_To_HTML( regime_buff, application_id )));
      Insert( translations, Assoc( "RAND", Utils.Random_String ));
      return Web_Utils.Build_Input_Page(
         mwcn.OSCR_Paths.template_components_path & "input",
         translations );
   end Input_Page_Callback;

   function Run_Progress_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      session_id : constant AWS.Session.Id := AWS.Status.Session( request );
   begin
      return AWS.Response.Build( "text/html",
         Model.Web_Commons.Get_State_Of_Run_As_HTML( session_id ) );
   end Run_Progress_Callback;
 
   --
   -- delete everthing in the dir with filename in it if and only if the file is in the
   -- user's directory in the work directory.
   -- return false if the dir isn't deleted
   -- 
   function Paraniod_Delete_Directory_Tree( filename : String; username : Unbounded_String ) return Boolean is
      dir : Unbounded_String := TuS(Ada.Directories.Containing_Directory( filename ));
   begin
      Put_Line( "deleting: testing dir " & TS( dir ));
      if( not Ada.Directories.Exists( TS(dir) )) then
         --
         -- No such directory: bale out
         --
         return False;
      end if;
      if( Index( dir, TS(mwcn.OSCR_Paths.work_dir & Censor_String( username )) ) = 0 )then
         Put_Line( "Not deleting " & TS( dir ));
         --
         -- not in the user's workdir: bale out
         -- 
         return False;
      end if;
      if( dir = mwcn.OSCR_Paths.work_dir & "templates" )then
         --
         -- In templates directory: bale out
         --
         return False;
      end if;
      Put_Line( "Intending to Delete path " & TS( dir ));
      Ada.Directories.Delete_Tree( TS(dir) );
      return True;
   exception
      when Others => return False;
   end Paraniod_Delete_Directory_Tree;
 
   function Index_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use type rs.State_Type;
   
      function Extract_Action( params : AWS.Parameters.List ) return String is
      use mwcm.UK_Html_Utils;
      begin
         if( Contains_Value( params, "Edit" ) ) then 
            return "Edit";
         end if;
         if( Contains_Value( params, "Copy" ) ) then 
            return "Copy";
         end if;
         if( Contains_Value( params, "Delete" ) ) then 
            return "Delete";
         end if;
         return "";
      end Extract_Action;
   
      params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      translations : Translate_Set;
      logresult : constant u.Login_Result := Handle_Login( request );
      session_id : constant AWS.Session.Id := AWS.Status.Session( request );
      run_state : rs.State_Type;
      job_is_running : Boolean;
      action  : String := Extract_Action( params );
      deleted : Boolean;
      root : constant String := TS( mwcn.OSCR_Paths.root );
   begin
      Put_Line( "Index_Page_Callback: action |" & action & "| " );
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      run_state := Run_State_Session_Data.Get(
         session_id,
         mwcn.SESSION_RUN_STATE );
      job_is_running := run_state /= rs.BLANK_STATE_TYPE;
      if( action /= "" ) then 
         declare
            -- FIXME: needless generics
            filename : constant String := 
               mwcm.UK_Html_Utils.Reverse_Table_Lookup( params, action );
            template_dir : constant String := 
               Ada.Directories.Containing_Directory( filename );
         begin
            Put_Line( "filename = |" & filename & "| " );
            if ( action = "Copy" ) or ( action = "Edit" ) then
               declare
                  regime       : mc.Charging_Regime;
                  regime_buff  : mcb.Charging_Regime_Buffer;                  
                  dirname      : Unbounded_String;
                  run_settings : rs.Settings_Rec; 
               begin
                  mcw.Load( filename, regime );
                  regime_buff := mcb.Map_From( regime );
                  Put_Line( "opened file " & filename );
                  if( action = "Copy" ) then
                     regime_buff := mcb.Copy_Regime( regime_buff, True );                     
                     regime := mcb.Map_To( regime_buff );
                     dirname := 
                        Create_Directory_For_Run(  session_id, logresult.user, regime );
                     mcw.Save( dirname, regime );
                     mrsw.Load( TuS(template_dir), run_settings );
                     mrsw.Save( dirname, run_settings );
                  end if;
                  Costs_Model_Session_Data.Set( session_id, mwcn.SESSION_COST_MODEL, regime );
                  Costs_Model_Buffer_Session_Data.Set( session_id, mwcn.SESSION_COST_MODEL_BUFFER, regime_buff );
                  Run_Settings_Session_Data.Set( session_id, mwcn.SESSION_RUN_SETTINGS, run_settings );
               end;
               return AWS.Response.URL( Location => root & "input/" );
            elsif( action = "Delete" ) then
               deleted := Paraniod_Delete_Directory_Tree( filename, logresult.user.username );
               -- Ada.Directories.Delete_File( filename ); -- FIXME clean out the directory
               Put_Line( "filename = " & filename & " this filename " & TS(Create_Dirname_For_Run( session_id ) & mwcn.REGIME_FILENAME ));
               if( filename = TS(Create_Dirname_For_Run( session_id ) & mwcn.REGIME_FILENAME )) then -- deleting current; FIXME horrible test
                  declare
                     regime_buff   : constant mcb.Charging_Regime_Buffer := mcb.Make_Initialised_Regime;
                     regime        : constant mc.Charging_Regime := mcb.Map_To( regime_buff );   
                  begin
                     Costs_Model_Session_Data.Set( session_id, mwcn.SESSION_COST_MODEL, regime );
                     Costs_Model_Buffer_Session_Data.Set( session_id, mwcn.SESSION_COST_MODEL_BUFFER, regime_buff );
                  end;
               end if;
            end if;
         end;
      end if;
      --
      --
      --
      Insert( translations, Assoc( "ROOT", root ));
      Insert( translations, Assoc( "JOB_IS_RUNNING", job_is_running  ));
      Insert( translations, Assoc( "USERNAME", logresult.user.title ));
      --
      -- add lists of previous simulations 
      -- 
      Insert( translations, mcw.Make_File_Set( "USER", logresult.user.username ) ); 
      Insert( translations, mcw.Make_File_Set( "TEMPLATES", TuS( "templates" ) ) );
      Insert( translations, Assoc( "RAND", Utils.Random_String ));
      Insert( translations, Assoc( "BUILD-DATE", "$Date: 2009-06-18 16:45:32 +0100 (Thu, 18 Jun 2009) $" ));
      Insert( translations, Assoc( "REVISION", "$Revision: 7467 $" ));
      return Web_Utils.Build_Input_Page(
         mwcn.OSCR_Paths.template_components_path & "index",
         translations );
   end Index_Page_Callback;


end OSCR_Callbacks;
