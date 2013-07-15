--
-- $Revision $
-- $Author $
-- $Date $
--
with Ada.Text_IO;

with AWS.Server;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Page_Server;
with AWS.Config; use AWS.Config;
with AWS.Config.Set; use AWS.Config.Set;
with AWS.Log;
with AWS.Services; use AWS.Services;

with Ajax_Callback;
with Model.Web_Constants;
with Web_Utils;
with Text_Utils;

procedure Ajax_Test is
        MY_PORT       : constant := 9091;
        my_server     : AWS.Server.HTTP;
        my_dispatcher : AWS.Services.Dispatchers.URI.Handler;
        my_config     : AWS.Config.Object := AWS.Config.Get_Current;

begin
        -- aws.log.start( log=>la_log.logger, split=>aws.log.none, filename_prefix=>"sla_logger" );

        Ada.Text_IO.Put_Line
              ("Call me on port" &
                Positive'Image( MY_PORT ) &
                "press q to stop me ...");
        AWS.Config.Set.Server_Name( my_config, "OSCR Test Server" );
        AWS.Config.Set.Server_Port( my_config, MY_PORT );
        AWS.Config.Set.WWW_Root( my_config, Text_Utils.TS(Model.Web_Constants.OSCR_Paths.physical_server_root) );
        AWS.Config.Set.Max_Connection( my_config, 10 );
        AWS.Config.Set.Session( my_config, true );
        AWS.Config.Set.Session_Lifetime( Duration(72_000) ); -- 2 hours

        Dispatchers.URI.Register_Regexp( my_dispatcher, "/.*\.css|/.*\.js|/.*\.png|/.*\.html|/.*\.gif|/.*\.pdf",
                                         AWS.Services.Page_Server.callback'Access );
        Dispatchers.URI.Register( my_dispatcher, "/edit_rbs",  Ajax_Callback.Test_Callback'Access );
        
        Dispatchers.URI.Register( my_dispatcher, "/ajax_test/",  Ajax_Callback.Home_Page_Callback'Access );
        
        AWS.Server.Start( 
           my_server,
           Dispatcher => my_dispatcher,
           Config => my_config );
           
        AWS.Server.Wait( AWS.Server.forever );

        AWS.Server.Shutdown(my_server);
end Ajax_Test;
