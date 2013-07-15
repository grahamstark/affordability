--
-- A bunch of OSCR - specific (I imagine) functions. Hence the Model parent.
-- Plus some odds n sods and constants for paths and the like.
-- 
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with AWS.Log;
with AWS.Parameters;
with AWS.Response;
with AWS.Session;
with AWS.Status;

with Model.Charging;
with Model.Charging.Buffer;
with Model.Run_Settings;
with Model.OSCR_Output.Generator;

with Data_Constants;
with HTML_Utils;
with T_Utils;
with Templates_Parser;
with Model.Web_Constants;
with OSCR_Users;


package Model.Web_Commons is

   package rs renames Model.Run_Settings;
   package moo renames Model.OSCR_Output;
   package moog renames Model.OSCR_Output.Generator;
   
   use Ada.Strings.Unbounded;

   package Costs_Model_Session_Data is new AWS.Session.Generic_Data(
      Model.Charging.Charging_Regime,
      Model.Charging.BLANK_CHARGING_REGIME
   );
   
   package Costs_Model_Buffer_Session_Data is new AWS.Session.Generic_Data(
      Model.Charging.Buffer.Charging_Regime_Buffer,
      Model.Charging.Buffer.BLANK_CHARGING_REGIME_BUFFER
   );
   
   package Charges_Buffer_Session_Data is new AWS.Session.Generic_Data(
      Model.Charging.Buffer.Charges_Buffer_Type,
      Model.Charging.Buffer.BLANK_CHARGES_BUFFER
   );
   
   package Target_Buffer_Session_Data is new AWS.Session.Generic_Data(
      Model.Charging.Buffer.Target_Buffer_Type,
      Model.Charging.Buffer.BLANK_TARGET_BUFFER
   );
   
   package Application_Buffer_Session_Data is new AWS.Session.Generic_Data(
      Model.Charging.Buffer.Application_Buffer_Type,
      Model.Charging.Buffer.BLANK_APPLICATION_BUFFER
   );
   
   package Run_Settings_Session_Data is new AWS.Session.Generic_Data(
      rs.Settings_Rec,
      rs.BLANK_SETTINGS_REC
   );

   package Run_State_Session_Data is new AWS.Session.Generic_Data(
      rs.State_Type,
      rs.BLANK_STATE_TYPE
   );
   
   package Tables_Session_Data is new AWS.Session.Generic_Data(
      moo.Table_Set,
      moo.BLANK_TABLE_SET 
   );
   

   package UK_HTML_Utils is new HTML_Utils( 
      Rate=>Rate, 
      Counter_Type => Counter_Type );
   --
   -- public for testing only
   -- 
   function Update_Run_State(
      run_id        : Unbounded_String;
      household     : Natural;
      year          : Data_Constants.Data_Years;
      other_counter : Natural;
      phase         : rs.Phase_Type;
      health        : rs.Health_Type := rs.normal ) return Boolean;
   
   --
   -- public for testing only
   -- 
   function Get_State_Of_Run_As_HTML( session_id : AWS.Session.Id ) return Unbounded_String;
   -- for testing only 
   function Make_Progress_Table( run_state : rs.State_Type; run_settings : rs.Settings_Rec ) return Unbounded_String;
   
   function Run_Javacharts( 
      script : String; 
      run_directory : String; 
      logfile : String; 
      use_svg_graphics : Boolean ) return Integer;
   
   --
   -- public for testing only
   -- 
   procedure Run_Charts_Driver( run_directory : String; user : OSCR_Users.User_Type );

   
   type Regime_And_Control is record
      regime : Model.Charging.Charging_Regime; 
      run_settings : rs.Settings_Rec;
      session_id   : AWS.Session.Id;
      working_directory : Unbounded_String; 
      -- working_directory is a duplicate of the string in the session as 
      -- SESSION_WORK_DIR, but is needed here because we might run the job 
      -- when the session isn't there any more, but the job is still queued.
   end record;
   
   package Job_Queue_Package is new Ada.Containers.Vectors(
      Index_Type => Positive,
      Element_Type => Regime_And_Control );      
   
   protected type Job_Queue_Type is
      procedure Deque( rc : out Regime_And_Control );
      function Size return Natural;
      function Num_Jobs_Run return Natural;
      procedure Set_Max_Jobs_To_Run( n : Positive );
      function Run_Enough_Jobs return Boolean; 
      procedure Enque( 
         session_id : AWS.Session.Id; 
         regime : Model.Charging.Charging_Regime; 
         run_settings : rs.Settings_Rec;
         working_dir  : Unbounded_String );
   private      
      queue : Job_Queue_Package.Vector; 
      job_count : Natural := 0;
      max_jobs_to_run : Natural := Natural'Last;
   end Job_Queue_Type;
   
   job_queue : Job_Queue_Type;
   
   logger : AWS.Log.Object;
   
end Model.Web_Commons;
