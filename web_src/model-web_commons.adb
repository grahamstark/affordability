with Ada.Containers.Hashed_Maps;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Gnat.OS_Lib;

with AWS.Log;
with AWS.Parameters;  

with Data_Constants;
with Model.OSCR_Output.Formatters;
with Model.Run_Settings;
with Model.Charging.Buffer;
with Model.Web_Constants;
with Scotland_Specific_Constants;
with System;
with Templates_Parser; 
with Text_Utils;
with Text_Utils;
with Utils;
with Web_Utils;

package body Model.Web_Commons is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Text_Utils;
   use AWS.Log;
   
   package u renames OSCR_Users;
   package moof renames Model.OSCR_Output.Formatters;
   package mwcn renames Model.Web_Constants;
   package mcb  renames Model.Charging.Buffer;
   
   function Compare_State_Type( left, right : rs.State_Type ) return Boolean is
      use type rs.State_Type;
   begin
      return left = right;
   end Compare_State_Type;
   
   function Compare_Settings_Rec( left, right : rs.Settings_Rec ) return Boolean is
      use type rs.Settings_Rec;
   begin
      return left = right;
   end Compare_Settings_Rec;
    
   protected body Job_Queue_Type is
   
      procedure Deque( rc : out Regime_And_Control ) is
      begin
         rc := ( Model.Charging.BLANK_CHARGING_REGIME, rs.BLANK_SETTINGS_REC, AWS.Session.No_Session, TuS( "" ) );
         if( Size > 0 ) then
            rc := Job_Queue_Package.Element( queue, 1 );
            Job_Queue_Package.Delete( queue, 1 );
            job_count := job_count + 1;
         end if;
      end Deque;
      
      procedure Set_Max_Jobs_To_Run( n : Positive ) is
      begin
         max_jobs_to_run := n;
      end Set_Max_Jobs_To_Run;
 
      function Num_Jobs_Run return Natural is
      begin
         return job_count;
      end Num_Jobs_Run;
      
      function Size return Natural is 
      begin
         return Natural( Job_Queue_Package.Length( queue ));
      end Size;
      
      procedure Enque( 
         session_id : AWS.Session.Id; 
         regime : Model.Charging.Charging_Regime; 
         run_settings : rs.Settings_Rec; 
         working_dir : Unbounded_String ) is
         rc : Regime_And_Control := ( regime, run_settings, session_id, working_dir );
      begin
         Put_Line( "Enqueing job for directory " & TS( working_dir ));
         Job_Queue_Package.Append( queue, rc );
      end Enque;
      
      function Run_Enough_Jobs return Boolean is
      begin
         return ( max_jobs_to_run < Positive'Last ) and then ( job_count >= max_jobs_to_run );
      end Run_Enough_Jobs;
      
   end Job_Queue_Type;
   
  
   function Update_Run_State(
      run_id        : Unbounded_String;
      household     : Natural;
      year          : Data_Constants.Data_Years;
      other_counter : Natural;
      phase         : rs.Phase_Type;
      health        : rs.Health_Type := rs.normal
      ) return Boolean is
      use type rs.Phase_Type;
      use type rs.Health_Type;
      use Text_Utils;
      aborting : Boolean := False; 
      run_state : rs.State_Type;
      session_id : constant AWS.Session.Id := AWS.Session.Value( TS( run_id ));
   begin
      run_state := Run_State_Session_Data.Get( session_id, mwcn.SESSION_RUN_STATE );
      if( phase = rs.complete ) or ( health = rs.aborted ) then
         Run_State_Session_Data.Set( session_id, mwcn.SESSION_RUN_STATE, rs.BLANK_STATE_TYPE );
         if( health = rs.aborted ) then -- turn off aborted session switch
            -- FIXME get rid of aborting session switch and use the state in
            -- rs instead everyhere
            AWS.Session.Set( session_id, mwcn.SESSION_ABORTING, False );
         end if;
      else 
         run_state.household := household;
         run_state.year := year; 
         run_state.other_counter := other_counter;
         run_state.phase := phase;
         run_state.health := health;
         Run_State_Session_Data.Set( session_id, mwcn.SESSION_RUN_STATE, run_state );
      end if;
      aborting := AWS.Session.Get( session_id, mwcn.SESSION_ABORTING ) or ( health = rs.aborted );
      -- Put_Line( "got abort status as " & Boolean'Image(aborting) );
      return aborting;
   end Update_Run_State;
   
   function Run_Javacharts( 
      script : String; 
      run_directory : String; 
      logfile : String; 
      use_svg_graphics : Boolean ) return Integer is
   use Gnat.OS_Lib;
   use System;
      return_code : Integer;
      args : Argument_List( 1 .. 2 );
      success : Boolean := False;
   begin
      args( 1 ) := new String'( run_directory );
      if( use_svg_graphics ) then
         args( 2 ) := new String'( "1" );
      else
         args( 2 ) := new String'( "0" );
      end if;
      Write( logger,  "Run_Charts_Driver: args created; writing to dir " & run_directory );
      Put_Line( "Run_Charts_Driver: args created; writing to dir " & run_directory & " script is " & script );
      Spawn( script, args, logfile, success, return_code );
      Write( logger,  "spawned" );
      Put_Line( "spawned" );
      if( not success ) and then ( return_code = 0 ) then
         return_code := -1;
      end if;
      Free( args( 1 ) );
      Write( logger,  "args destroyed" );
      Put_Line(  "args destroyed" );
      return return_code;
   end Run_Javacharts;
   
   procedure Run_Charts_Driver( run_directory : String; user : OSCR_Users.User_Type ) is
   use Model.Web_Constants;
   use Text_Utils;
   use OSCR_Users;
   
      -- type String_Access is access String;
      return_code : Integer := 0;
      script : constant String := TS(OSCR_Paths.charts_driver_script);
      logfile   : constant String := TS(OSCR_Paths.log_file_dir) & "last_java_run_results.log";
   begin
      return_code := Run_Javacharts( script, run_directory, logfile, user.preferences( use_svg_graphics ));
      if( return_code /= 0 ) then
         Write( logger, "java failed: return code was " &  Integer'Image(return_code) );
         Put_Line( "java failed: return code was " &  Integer'Image(return_code) );
      end if;
      
   end Run_Charts_Driver;

   
   function Print_Run_State( 
      run_id      : Unbounded_String;
      household   : Natural;
      year        : Data_Constants.Data_Years;
      other_count : Natural;
      phase       : rs.Phase_Type ) return Boolean is
   use Text_Utils;
   begin
      Put_Line( "run state " & TS( run_id ) & " household " & household'Img &
                " year " & year'Img & " phase " & phase'Img );
      return False;
   end Print_Run_State;
   
   procedure Zip_Directories( output_directory : String ) is
      zip_file_name    : constant String := output_directory & DIR_SEPARATOR & "phunpack.zip";      
   begin
      Utils.Zip_Directory( output_directory, zip_file_name );                
   end Zip_Directories;

   task type Model_Runner_Task_Type is
      -- 
      -- For why this pragma:
      -- See: http://gcc.gnu.org/onlinedocs/gcc-3.3.6/gnat_rm/Storage_005fSize-Clauses.html
      -- and: http://coding.derkeiler.com/Archive/Ada/comp.lang.ada/2005-05/msg00281.html
      -- 
      pragma Storage_Size( Utils.Default_Stack_Size );

   end Model_Runner_Task_Type;
   
   runner : Model_Runner_Task_Type;
   someones_job_running : Boolean;
   
   
   task body Model_Runner_Task_Type is
      --
      -- FIXME: this is wrong because is presupposes that the user's session is
      -- still there when the job runs. We need need to store everything we need
      -- in the Regime_And_Control record and use it all from there.
      -- Not fixing it now, though.
      -- Also, we have out_dir from the session and   working_directory from the RandC record
      -- which are (almost) the same thing.
      -- 
   use Text_Utils;
      rc       : Regime_And_Control;
      i        : Positive := 1;
      root     : constant String := TS( mwcn.OSCR_Paths.root );
   begin
      loop
         if( job_queue.Size > 0 ) then
            job_queue.Deque( rc );
            someones_job_running := True;
            declare
               run_id_string : constant Unbounded_String := TuS(AWS.Session.Image( rc.session_id ));
               user       : constant u.User_Type :=  u.User_Session_Data.Get( rc.session_id, mwcn.SESSION_USER_ID );
               aborting   : Boolean := False;
               tables     : moo.Table_Set;
               out_dir    : constant String := AWS.Session.Get( rc.session_id, mwcn.SESSION_WORK_DIR );
               YEAR_DUMMY : constant Positive := Data_Constants.FIRST_AVAILABLE_DATA_YEAR;
            begin
               --
               -- FIXME TEMP CODE
               -- 
               -- rc.run_settings.inc_ctl.exclude_is_cases := False;
               -- rc.run_settings.inc_ctl.exclude_net_incomes_below_poverty_line := True;
               --
               Write( logger,  "job dequed run_settings.inc_ctl.aggregate_incomes_to = " &
                  rc.run_settings.inc_ctl.aggregate_incomes_to'Img );
               Put_Line( "job dequed with run_settings.inc_ctl.aggregate_incomes_to = " &
                  rc.run_settings.inc_ctl.aggregate_incomes_to'Img );
               tables := moog.Generate_Cost_Measures( 
                  mwcn.OSCR_Paths.datafile_directory,
                  run_id_string,
                  rc.regime,  
                  rc.run_settings,
                  Update_Run_State'Access );
               aborting := Update_Run_State( run_id_string, 0, YEAR_DUMMY, 0, rs.generating_output );
               if( not aborting ) then
                  Tables_Session_Data.Set( rc.session_id, mwcn.SESSION_OUTPUT_TABLES, tables );
                  moof.Make_HTML_And_Charts(
                     root,
                     rc.regime,
                     rc.working_directory,               
                     tables,
                     rc.run_settings,
                     user.title ); -- FIXME put this somewhere other than the f**ing buffer
                end if;
                aborting := Update_Run_State( run_id_string, 0, YEAR_DUMMY, 0, rs.generating_output );
                if ( not aborting ) then
                  if mwcn.OSCR_paths.create_zip_file_and_static_images then 
                     Write( logger, "starting  Run_Charts_Driver to dir " & out_dir );
                     Run_Charts_Driver( out_dir, user );
                     Zip_Directories( out_dir );  
                  end if;
                  aborting := Update_Run_State( run_id_string, 0, YEAR_DUMMY, 0, rs.complete );
                else
                  aborting := Update_Run_State( run_id_string, 0, YEAR_DUMMY, 0, rs.complete, rs.aborted );
                end if;
                -- always turn off the abort indicator, no matter how we got to here
                AWS.Session.Set( rc.session_id, mwcn.SESSION_ABORTING, False );
            end; -- decare
         end if;
         exit when job_queue.Run_Enough_Jobs;
         delay 1.0; 
         i := i + 1;
         if( i = 10 ) then
            Put_Line( "Processing Jobs: Queue size: " & Natural'Image(Job_Queue.Size) );
            i := 1;
         end if;
        -- end Process_Job_Queue;
     end loop;
     Write( logger,  "Exiting Model_Runner_Task_Type " );
   end Model_Runner_Task_Type;

   function Make_Household_Progress_Table( pct_done : Natural ) return Unbounded_String is
      use Text_Utils;
      
      s : Unbounded_String;
      pct_to_do : Natural;
   begin
      pct_to_do := 100 - pct_done;
      s := s &"<table width='100%' border='0' cellspacing='0'  class='progressBar'>";
      s := s &"<tr>";
      if( pct_done > 0 ) then
         s := s & "<td width='" & Format(pct_done) & "%' class='households_done'>&nbsp;</td>";
         s := s & "<td width='" & Format(pct_to_do) & "%' class='households_todo'>&nbsp;</td>";
      else
         s := s & "<td>&nbsp;</td>";
      end if;
      s := s & "</tr>";
      s := s & "</table>";
      return s;
   end Make_Household_Progress_Table;
   
   function Make_Years_Progress_Table( 
      this_year, start_year, end_year :  Data_Constants.Data_Years;
      phase :rs.Phase_Type ) return Unbounded_String is
      
      use type rs.Phase_Type;
      use Text_Utils;
      
      num_years : constant Rate := Rate( end_year - start_year + 1 );
      width : constant Positive := Positive( 100.0/num_years );
      width_str : constant String := Format( width );
      s : Unbounded_String := TuS("<table width='100%' cellspacing='1'><tr class='modulebox' >" );
      class : Unbounded_String := TuS( "module_done" );
    begin  
      if( phase = rs.generating_output ) then
         s := s & "<td class='module_active' width='20%'>Loading Results</td>";
      elsif( phase = rs.running ) then 
         for year in start_year .. end_year loop
            if( year = this_year ) then
               s := s & "<td width='" & width_str & "%' class='module_active'>" & Format(year) & "</td>";       
               class := TuS( "module_todo" );
            else
               s := s & "<td width='" & width_str & "%' class='" & class & "'>" & Format(year) & "</td>";
            end if;
         end loop;
      else
         s := s & "<td>&nbsp;</td>";
      end if;
      s := s  & "</tr></table>";
      return s;
    end Make_Years_Progress_Table;

   function Get_Percentage_Done( year : Data_Constants.Data_Years; done :Natural; r : rs.Settings_Rec ) return Natural is
      use Scotland_Specific_Constants;
      nhh : Rate;
   begin
      if( r.scotland_only ) then
         nhh := Rate( -- scotland - specific start stops
                   FILE_POSITIONS( year, end_pos ) - 
                   FILE_POSITIONS( year, start_pos ) + 1 );
      else
         nhh := Rate(Data_Constants.RAW_FRS_HOUSEHOLD_COUNTS( year ));
      end if;
      return Natural( 100.0*Rate( done ) / nhh ); 
   end Get_Percentage_Done;

   function Make_Progress_Table( run_state : rs.State_Type; run_settings : rs.Settings_Rec ) return Unbounded_String is
      use type rs.Phase_Type;
      use Text_Utils;
      
      s : Unbounded_String;
      pct_done : Natural;
      done_string : Unbounded_String := TuS( "" );
      years_table : Unbounded_String;
      households_table : Unbounded_String;
   begin   
      s := s & "<table width='98%' class='statusTable' >" & LINE_BREAK; 
      s := s &"     <tr>" & LINE_BREAK;
      case run_state.phase is
         when rs.not_started => 
               pct_done := 0;
               s := s & "<tr><td align='middle'><h3>Your job is in the queue and waiting to start</h3></td></tr>" & LINE_BREAK;
         when rs.run_starting => 
               pct_done := 0;
               s := s & "<tr><td>Starting Up</td></tr>" & LINE_BREAK;
         when  rs.running => 
               pct_done := Get_Percentage_Done( 
                  run_state.year, 
                  run_state.household, 
                  run_settings );
               households_table := Make_Household_Progress_Table( pct_done );
               years_table := Make_Years_Progress_Table( 
                  run_state.year, 
                  run_settings.start_year,
                  run_settings.end_year,
                  run_state.phase );
               s := s &"<tr><th>Data Year</th><th colspan='2'>Households Processed</th></tr>" & LINE_BREAK;
               s := s & "<tr>" ;
               s := s &"<td width='42%'>" & years_table & "</td>";
               s := s &"<td width='16%'>" & Format_With_Commas( run_state.household ) & " (" & 
                    Format( pct_done ) & "%) Completed </td>";
               s := s &"<td width='42%'>" & households_table & "</td>";
               s := s & "</tr>" & LINE_BREAK;
               s := s & "<tr><td colspan='3' align='center'><input id='Abort_Submit_Button' type='submit' name='action' value='Abort' /></td></tr>" & LINE_BREAK;
         when rs.generating_output => 
               pct_done := 100;
               s := s & "<tr><td align='center'><h4>Generating Output</h4> : Please Wait .. </td></tr>" & LINE_BREAK;
         when rs.complete =>  
               pct_done := 100;
               s := s & "<tr><td>Completed</td>" & LINE_BREAK;
      end case;
      s := s &"     </tr>" & LINE_BREAK;
      s := s &"</table>" & LINE_BREAK;
      return s;
   end Make_Progress_Table;
   
   function Get_State_Of_Run_As_HTML( session_id : AWS.Session.Id ) return Unbounded_String is
      use type rs.Phase_Type;
      use type rs.State_Type;
      use Text_Utils;
      run_state : rs.State_Type;  
      run_settings : constant rs.Settings_Rec := Run_Settings_Session_Data.Get( session_id, mwcn.SESSION_RUN_SETTINGS );
      s : Unbounded_String;
      aborting : constant Boolean := AWS.Session.Get( session_id, mwcn.SESSION_ABORTING );
   begin
      if( aborting ) then 
         run_state := rs.BLANK_STATE_TYPE;
         -- always turn the abort off
         Run_State_Session_Data.Set( session_id, mwcn.SESSION_RUN_STATE, run_state );
      end if;
      run_state := Run_State_Session_Data.Get( session_id, mwcn.SESSION_RUN_STATE );
      if( run_state /= rs.BLANK_STATE_TYPE ) then
         s := Make_Progress_Table( run_state, run_settings );
      else -- turn javascript off, and enable run button
         Write( logger,  "complete; sending switch off code " );
         s := s & "<script>" & LINE_BREAK; 
         s := s & "      updater.stop();"  & LINE_BREAK; 
         s := s & "      $( 'Run_Submit_Button' ).disabled=false" & LINE_BREAK;
         s := s & "</script>" & LINE_BREAK;
      end if;
      return s;
   end Get_State_Of_Run_As_HTML;   

end Model.Web_Commons;
