--
-- $Revision $
-- $Author $
-- $Date $
--
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Server;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Page_Server;
with AWS.Config; use AWS.Config;
with AWS.Config.Set; use AWS.Config.Set;
with AWS.Log;
with AWS.Mime;
with AWS.Services; use AWS.Services;

with Model.Web_Constants;
with Web_Utils;
with OSCR_Callbacks;
with Text_Utils;
with Model.Web_Commons;
with Model;

procedure OSCR_Server is
   my_server     : AWS.Server.HTTP;
   my_dispatcher : AWS.Services.Dispatchers.URI.Handler;
   my_config     : AWS.Config.Object := AWS.Config.Get_Current;
   
   package mwc renames Model.Web_Constants;
   package awsc renames AWS.Config; 
   use Text_Utils;
   use Ada.Strings.Unbounded;
   ROOT : constant String := TS( mwc.OSCR_Paths.root );
   STATIC_FILE_REGEP : constant String :=  
                                   root & ".*\.css|" & 
                                   root & ".*\.js|" &
                                   root & ".*\.png|" & 
                                   root & ".*\.html|" & 
                                   root & ".*\.gif|" &
                                   root & ".*\.pdf|" &
                                   root & ".*\.zip";
begin

   Model.UK_Logger.Set_Output( TS( mwc.OSCR_Paths.log_file_dir & mwc.MODEL_LOG_FILE ) );
   
   AWS.Log.start( 
      log            => Model.Web_Commons.logger, 
      split          => AWS.Log.Daily, 
      file_directory => TS( mwc.OSCR_Paths.log_file_dir ),
      filename_prefix=> "oscr_server_log" );
      
   -- add a mime type for SVG
   AWS.Mime.Add_Extension( "svg", mwc.MIME_TYPE_IMAGE_SVG );

   Ada.Text_IO.Put_Line
      ("Call me on port" &
       Positive'Image( mwc.OSCR_Paths.port ) &
       "press q to stop me ...");
   awsc.Set.Server_Name( my_config, "OSCR Server" );
   awsc.Set.Server_Port( my_config, mwc.OSCR_Paths.port );
   awsc.Set.WWW_Root( my_config, TS( mwc.OSCR_Paths.physical_server_root ) );
   awsc.Set.Max_Connection( my_config, 50 );
   awsc.Set.Session( my_config, true );
   awsc.Set.Session_Lifetime( Duration( 720_000 ) ); -- 20 hours
   awsc.Set.Max_Connection(my_config,  100 );
   awsc.Set.Accept_Queue_Size( my_config, 60 );
   awsc.Set.Free_Slots_Keep_Alive_Limit(my_config,  80 );
   
   Dispatchers.URI.Register_Regexp( my_dispatcher, STATIC_FILE_REGEP,
                                   OSCR_Callbacks.Serve_Static_Resource'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root,  OSCR_Callbacks.Index_Page_Callback'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "progress/*",  OSCR_Callbacks.Run_Progress_Callback'Access );   
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "input/*",  OSCR_Callbacks.Input_Page_Callback'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "run_settings/*",  OSCR_Callbacks.Run_Settings_Callback'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "ajax/*",  OSCR_Callbacks.Ajax_Callback'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "charts/*",  OSCR_Callbacks.Serve_File_Callback'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "save_file/*",  OSCR_Callbacks.Serve_File_Callback'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "output/*",  OSCR_Callbacks.Output_Callback'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "popup/*",  OSCR_Callbacks.Popup_Callback'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "logout/*",  OSCR_Callbacks.Logout_Callback'Access );
   Dispatchers.URI.Register_Regexp( my_dispatcher, root & "load_existing/*",  OSCR_Callbacks.Index_Page_Callback'Access );
   AWS.Server.Start( 
      my_server,
      Dispatcher => my_dispatcher,
      Config => my_config );
     
   AWS.Server.Wait( AWS.Server.forever );
   
   AWS.Server.Shutdown( my_server );
   
end OSCR_Server;
