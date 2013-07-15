with Ada.Strings.Unbounded;


package Model.Web_Constants is
   
   use Ada.Strings.Unbounded;

   DOMAIN : constant String := "OSCR Costs Model";
   --
   -- ?? not used ??
   HOME_PAGE : constant String := "/oscr/index.thtml";
   INPUT_PAGE : constant String := "/oscr/input.thtml";
   OUTPUT_PAGE : constant String := "/oscr/output.thtml";
   
   SESSION_USER_ID : constant String := "user";
   SESSION_RUN_STATE : constant String := "run-state";
   SESSION_RUN_SETTINGS : constant String := "run-settings";
   SESSION_OUTPUT_TABLES : constant String := "output-tables";
   SESSION_COST_MODEL : constant String := "cost-model";
   SESSION_ABORTING : constant String := "aborting";
   SESSION_WORK_DIR : constant String := "work-dir";
   SESSION_COST_MODEL_BUFFER : constant String := "model-buffer";
   SESSION_APPLICATION_COPY   : constant String := "application-copy";
   SESSION_TARGET_COPY   : constant String := "target-copy";
   SESSION_CHARGE_COPY   : constant String := "charge-copy";
   SESSION_WHICH_SLOT_RANGE : constant String := "which_slot_range";
   
   -- mime type for svg see: /etc/mime.types, and AWS.Mime.ads
   MIME_TYPE_IMAGE_SVG : constant String := "image/svg+xml";
   MIME_TYPE_TEXT_CSV  : constant String := "text/comma-separated-values";
   
   MODEL_LOG_FILE  : constant String := "model_log_file.log";
   REGIME_FILENAME : constant String := "regime.txt";
   RUN_SETTINGS_FILENAME : constant String := "run_settings.txt";
   
   
   type OSCR_Paths_Record is record
      physical_server_root     : Unbounded_String;
      charts_driver_script     : Unbounded_String;
      datafile_directory       : Unbounded_String;
      template_components_path : Unbounded_String;
      work_dir                 : Unbounded_String;
      log_file_dir             : Unbounded_String;
      root                     : Unbounded_String;
      port                     : Positive;
      create_zip_file_and_static_images : Boolean := False;
   end record;
  
   function OSCR_Paths return OSCR_Paths_Record;
   

end Model.Web_Constants;
