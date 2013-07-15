with Ada.Characters.Handling; 
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;

with Templates_Parser; 

with Base_Model_Types;
with Charging_Examples;
with Costs_Tabulator;
with Data_Constants;
with FRS_Enums;
with FRS_Utils;
with Format_Utils;
with Inequality_Generator;
with Model.Web_Constants;
with Scotland_Specific_Constants;
with Tabulator;
with Tabulator_Commons;
with Text_Utils;
with Utils;
with Web_Utils;

with Model;

with Model.Equivalence_Scales;
with Model.Household;
with Model.Income_Measures.Breakdown_Package;

with Model.Incomes;
with Model.Main_Examples;
with Model.Output.Complete;
with Model.Output.Legal_Aid;
with Model.Parameters.Complete;
with Model.Parameters.Legal_Aid;
with Model.Parameters.Means_Tested_Benefits;
with Model.Run_Settings;
with Model.Uprate;



package body Model.OSCR_Output.Formatters is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Text_Utils;

   package webc  renames Model.Web_Constants;
   package adir  renames Ada.Directories;
   package soc   renames Scotland_Specific_Constants;
   package mhh   renames Model.Household;
   package lio   renames Ada.Text_IO.Unbounded_IO;
   package ubstr renames Ada.Strings.Unbounded;
   package pars  renames Model.Parameters.Complete;
   package outp  renames Model.Output.Complete;
   package icp   renames mimt.Income_Measure_Package;
   package mc    renames Model.Charging;
   package ap    renames Model.Charging.Application_Package;
   package tp    renames Model.Charging.Target_Package;
   
   function To_CDA( label : String; it : Incomes_Table; print_header : Boolean := False ) return String is
      use Text_Utils;

      use mimt;
      
      s : Unbounded_String := To_Unbounded_String( "" ); 
      lim : Amount := 0.0;
   begin
      if( print_header ) then
         s := s & label & ",,";
         for i in Income_Brackets'First .. Income_Brackets'Last loop
            lim := lim + INCOME_INCREMENT;
            s := s & Format(lim);
            if( i <  Income_Brackets'Last ) then 
               s := s & ",";
            end if;        
         end loop;
         s := s & LINE_BREAK;
      end if;
      for im in mimt.Income_Measure_Type'First .. mimt.Income_Measure_Type'Last loop
         s := s & label & "," & im'Img & ",";
         for i in Income_Brackets'First .. Income_Brackets'Last loop
            lim := lim + INCOME_INCREMENT;
            s := s & Counter_Type'Image( it( i, im ) );
            if( i <  Income_Brackets'Last ) then s := s & ","; end if;        
         end loop;
         s := s & LINE_BREAK;
      end loop;
      s := s & LINE_BREAK;
      return To_String(s);
   end To_CDA;
    
   function BU_Complete_To_String( 
      bu      : mhh.Model_Benefit_Unit; 
      tbpos   : outp.Benefit_Unit_Result;
      incomes : mimt.One_Complete_Income_Measure_Output ) return String is
   use Text_Utils;
   use Ada.Strings.Unbounded;
   
      outs : Unbounded_String := To_Unbounded_String( "==== BENEFIT UNIT ===== " );
   begin      
      outs := outs & mhh.To_String( bu );
      outs := outs & " Tax Benefit Position " & LINE_BREAK;
      outs := outs & outp.To_String( bu, tbpos );
      outs := outs & " Incomes Position " & LINE_BREAK;
      outs := outs & mimt.To_String( incomes );
      return To_String(outs);
   end BU_Complete_To_String;
    
   function Make_Docbook_Regime_Section(
      output_directory : String;
      regime : Model.Charging.Charging_Regime;
      tables : Table_Set; 
      which_totals_slot : mimt.Slot_Range ) return Unbounded_String is
      use Templates_Parser;
      use Model.Income_Measure_Types;

      use Model.Charging;

      regime_dir : String := Web_Utils.To_Dir_Name( regime.name );
      individual_sections : Vector_Tag;      
      complete_section : Vector_Tag;      
      section_translations : Translate_Set;
      NUM_APPLICATIONS : constant mimt.Costs_Range := mimt.Costs_Range( ap.Length( regime.applications ));
      summary : Breakdown_By_Tenure.Summary_Info_Rec;
      single_section_str : Unbounded_String;
   begin
      
      Insert( section_translations, Assoc( "HEADER", regime.name )); 
      Insert( section_translations, Assoc( "DESCRIPTION", regime.description ));
      for application_no in 1 .. NUM_APPLICATIONS loop 
         declare
            application : Application_Type := 
               Application_Package.Element( regime.applications, application_no );
            application_name : String := Web_Utils.To_Dir_Name( application.name );
            
         begin
            summary := 
               Breakdown_By_Tenure.Make_Summary_Data( tables.tenure_tab, application_no, which_totals_slot );
            single_section_str := 
               Breakdown_By_Tenure.Make_Docbook_Section( 
                  chart_directory => output_directory, 
                  section_header => To_String( application.description ) , 
                  summary_info => summary.totals,
                  html_output_dir => output_directory );
            individual_sections := individual_sections & single_section_str;
         end;
      end loop;
      Insert( section_translations, Assoc( "SECTIONS", individual_sections ));
      return Web_Utils.Parse_Template( 
         webc.OSCR_Paths.template_components_path & "single_docbook_section", section_translations );      
   end Make_Docbook_Regime_Section;
   
   function Make_Charts_Control_File(
      output_directory : String;
      tables : Table_Set; 
      application_no : mimt.Costs_Range;
      which_totals_slot : mimt.Slot_Range ) return Unbounded_String is
      
      tenure_summary : constant Breakdown_By_Tenure.Summary_Info_Rec := 
         Breakdown_By_Tenure.Make_Summary_Data( tables.tenure_tab, application_no, which_totals_slot );
      region_summary : Breakdown_By_Region.Summary_Info_Rec:= 
         Breakdown_By_Region.Make_Summary_Data( tables.region_tab, application_no, which_totals_slot );
      economic_status_summary : Breakdown_By_Economic_status.Summary_Info_Rec  := 
         Breakdown_By_Economic_status.Make_Summary_Data( tables.economic_status_tab, application_no, which_totals_slot );
      disablement_status_summary : Breakdown_By_Disablement_status.Summary_Info_Rec := 
         Breakdown_By_Disablement_status.Make_Summary_Data( tables.disablement_status_tab, application_no, which_totals_slot );
      bu_type_summary : Breakdown_By_Bu_type.Summary_Info_Rec := 
         Breakdown_By_Bu_type.Make_Summary_Data( tables.bu_type_tab, application_no, which_totals_slot );
      age_range_summary : Breakdown_By_Age_Range_Of_Head.Summary_Info_Rec := 
         Breakdown_By_Age_Range_Of_Head.Make_Summary_Data( tables.age_range_tab, application_no, which_totals_slot );
      charts : Unbounded_String;
   begin      
       charts := charts & Breakdown_By_Tenure.Write_Chart_Block( output_directory, "tenure", tenure_summary );
       charts := charts & Breakdown_By_Region.Write_Chart_Block( output_directory,"regions", region_summary );
       charts := charts & Breakdown_By_Economic_Status.Write_Chart_Block( output_directory,"economic_status", economic_status_summary );
       charts := charts & Breakdown_By_Disablement_status.Write_Chart_Block( output_directory,"disablement_status", disablement_status_summary );
       charts := charts & Breakdown_By_Bu_type.Write_Chart_Block( output_directory,"bu_type", bu_type_summary );
       charts := charts & Breakdown_By_Age_Range_Of_Head.Write_Chart_Block( output_directory, "age_of_head", age_range_summary );
       return charts;       
   end Make_Charts_Control_File;
   
   function Make_Submenu( root: String; regime : mc.Charging_Regime; this_application : Unbounded_String ) return Unbounded_String is
   use Model.Income_Measure_Types;
   use Unbounded_String_Vector_Package;
   use Text_Utils;
   use mc;
      application_names : Unbounded_String_List;
      application_ids : Unbounded_String_List;
      NUM_APPLICATIONS : constant Costs_Range := Costs_Range( ap.Length( regime.applications ));
      outs : Unbounded_String;
      application : Application_Type; 
   begin
      for application_no in 1 .. NUM_APPLICATIONS loop
         application := ap.Element( regime.applications, application_no );
         Append( application_names, application.name );
         Append( application_ids, TuS(Web_Utils.To_Dir_Name( application.name) ));
         
      end loop;
      outs := Web_Utils.Make_Menu( root & "output", "application", application_names, application_ids, this_application, 0 );
      return outs;
   end Make_Submenu;

   function Make_HTML(
      root           : String;
      regime         : Model.Charging.Charging_Regime;
      application_no : mimt.Costs_Range; 
      relative_dir   : String;
      tables         : Table_Set;
      settings       : rs.Settings_Rec;
      user_title : Unbounded_String ) return Unbounded_String is
      
      use Model.Charging;
      use Model.Income_Measure_Types;
      use Templates_Parser;
      use ubstr;
      
      which_totals_slot : mimt.Slot_Range := settings.inc_ctl.summary_slot; 
      
      tenure_summary : constant Breakdown_By_Tenure.Summary_Info_Rec := 
         Breakdown_By_Tenure.Make_Summary_Data( tables.tenure_tab, application_no, which_totals_slot );
      region_summary : Breakdown_By_Region.Summary_Info_Rec:= 
         Breakdown_By_Region.Make_Summary_Data( tables.region_tab, application_no, which_totals_slot );
      economic_status_summary : Breakdown_By_Economic_status.Summary_Info_Rec  := 
         Breakdown_By_Economic_status.Make_Summary_Data( tables.economic_status_tab, application_no, which_totals_slot );
      disablement_status_summary : Breakdown_By_Disablement_status.Summary_Info_Rec := 
         Breakdown_By_Disablement_status.Make_Summary_Data( tables.disablement_status_tab, application_no, which_totals_slot );
      bu_type_summary : Breakdown_By_Bu_type.Summary_Info_Rec := 
         Breakdown_By_Bu_type.Make_Summary_Data( tables.bu_type_tab, application_no, which_totals_slot );
      age_range_summary : Breakdown_By_Age_Range_Of_Head.Summary_Info_Rec := 
         Breakdown_By_Age_Range_Of_Head.Make_Summary_Data( tables.age_range_tab, application_no, which_totals_slot );
      
      application        : constant Application_Type := Application_Package.Element( regime.applications, application_no );
      translations       : Translate_Set;
      blocks             : Templates_Parser.Vector_Tag;
      population_summary : Unbounded_String;
      html               : Unbounded_String;
      menu               : Unbounded_String := Make_Submenu( root, regime, application.name );
      title              : String := To_String( regime.name & " : " & application.name );
      regime_id          : String := Censor_String( To_String(regime.name) );
      application_id     : String := Censor_String( To_String(application.name) );
      regime_text        : String := To_String(regime.name);
      application_text   : String := To_String(application.name);
      
   begin      
       html := To_Unbounded_String( "" );
       population_summary := Breakdown_By_Tenure.Make_Single_Table(
            root,
            regime_text,
            application_text,
            regime_id,
            application_id,
            "All of Scotland", "tenure", tenure_summary.totals, 
            FRS_Enums.Tenure_Type'First, 
            True ); 
        
       Insert( translations, Assoc ("AFFORDABLE-RESIDUAL-INCOME-POPN", 
         Format_With_Commas( 100.0 - tenure_summary.totals.unaffordable( residual_income_level ), False )));
       Insert(translations, Assoc ("UNAFFORDABLE-COST-DISPOSABLE-RATIO-POPN",
         Format_With_Commas( tenure_summary.totals.unaffordable( cost_disposable_ratio ), False ))); -- FIXME fwc only becuase only one to supress the decimal
       Insert( translations, Assoc ( "ALL-POPULATION", To_String( population_summary ) ));
       Insert( translations, Assoc ( "TITLE", title ));
       Insert( translations, Assoc ( "RELATIVE_DIR", relative_dir ));
       Insert( translations, Assoc ( "REGIME", regime_id ));
       Insert( translations, Assoc ( "APPLICATION", application_id ));
       Insert( translations, Assoc ( "USERNAME", user_title ));
       Insert( translations, Assoc ( "ROOT", root ));
       blocks := blocks & Breakdown_By_Tenure.To_HTML(
            root,
            regime_text,
            application_text,
            regime_id,
            application_id,
            tenure_summary,
            "Breakdown by Tenure", 
            "tenure",
            False );
       -- blocks := blocks & Breakdown_By_Region.To_HTML( 
            -- relative_dir,
            -- region_summary,
            -- "Breakdown by Region", 
            -- "region",
            -- False );
       blocks := blocks & Breakdown_By_Economic_Status.To_HTML(
            root,
            regime_text,
            application_text,
            regime_id,
            application_id,
            economic_status_summary,
            "Breakdown by Economic Status", 
            "economic_status",
            False );
       blocks := blocks & Breakdown_By_Disablement_Status.To_HTML( 
            root,
            regime_text,
            application_text,
            regime_id,
            application_id,
            disablement_status_summary,
            "Breakdown by Disablement Status", 
            "disablement_status",
            False );
       blocks := blocks & Breakdown_By_Bu_type.To_HTML( 
            root,
            regime_text,
            application_text,
            regime_id,
            application_id,
            bu_type_summary,
            "Breakdown by Benefit Unit Type", 
            "bu_type",
            False );
       blocks := blocks & Breakdown_By_Age_Range_Of_Head.To_HTML(
            root,
            regime_text,
            application_text,
            regime_id,
            application_id,
            age_range_summary,
            "Breakdown by Age Range of Head", 
            "age_of_head",
            False );
       Insert( translations, Assoc ( "TABLE-BLOCKS", blocks ));
       Insert( translations, Assoc ( "REGIME_DESCRIPTION", regime.description ) );
       Insert( translations, Assoc ( "APPLICATION_DESCRIPTION", application.description ) );
       Insert( translations, Assoc ( "MENU", menu ) );
       Insert( translations, Assoc ( "REGIME_NAME", regime.name ) );
       Insert( translations, Assoc ( "APPLICATION_NAME", application.name ) );
       Insert( translations, Assoc ( "SLOT-RANGE", mimt.Pretty_Print( which_totals_slot )) );
       html := Web_Utils.Parse_Template( webc.OSCR_Paths.template_components_path & "output", translations );
       return html;
   end Make_HTML;
   
   
   procedure Make_HTML_And_Charts(
      root   : String;
      regime : Model.Charging.Charging_Regime;
      output_directory : Unbounded_String;
      tables : Table_Set;
      settings : rs.Settings_Rec;
      user_title : Unbounded_String ) is
      
      use Model.Charging;
      use Model.Income_Measure_Types;
      which_totals_slot : mimt.Slot_Range := settings.inc_ctl.summary_slot; 
      num_applications : constant Costs_Range := Costs_Range(ap.Length( regime.applications ));
      outfile : File_Type;
   begin
       Each_Application:
       for application_no in 1 .. num_applications loop
          declare
            application       : constant Application_Type := Application_Package.Element( regime.applications, application_no );
            relative_dir      : constant String := Web_Utils.To_Dir_Name( application.name );
            regime_dir        : constant String := To_String(output_directory) & relative_dir & DIR_SEPARATOR;
            cda_file_name     : constant String := regime_dir & "output_dump.csv";
            html_file_name    : constant String := regime_dir & "output.html";
            charts_file_name  : constant String := regime_dir & "oscr_charts_driver.txt";
            docbook_file_name : constant String := regime_dir & "docbook_insert.xml";
            html      : Unbounded_String; --  
            charts    : Unbounded_String := Make_Charts_Control_File( regime_dir, tables, application_no, which_totals_slot );
            docbook   : Unbounded_String;            
          begin
            html := Make_HTML( root, regime, application_no, relative_dir, tables, settings, user_title );
            Utils.Make_Directory_Path( regime_dir );
            Create( outfile, Out_File, html_file_name );
            Put_Line( outfile, To_String( html ));
            Close( outfile );
            Create( outfile, Out_File, charts_file_name );
            Put_Line( outfile, To_String( charts ));
            Close( outfile );
            --
            -- note this puts a duplicate file in each application direcory: all we have time for
            -- since all the applications are in each table set
            -- 
            Create( outfile, Out_File, cda_file_name );
            Put_Line( outfile, To_String( regime.name ) );
            Put_Line( outfile, "TENURE" );
            Put_Line( outfile,  Breakdown_By_Tenure.To_CDA( tables.tenure_tab, num_applications, settings ));
            -- Put_Line( outfile, "REGION" );
            -- Put_Line( outfile, Breakdown_By_Region.To_CDA( tables.region_tab, num_applications, settings ));
            Put_Line( outfile, "Economic_status" );
            Put_Line( outfile, Breakdown_By_Economic_status.To_CDA( tables.economic_status_tab, num_applications, settings ));
            Put_Line( outfile, "Disablement_status");
            Put_Line( outfile, Breakdown_By_Disablement_status.To_CDA( tables.disablement_status_tab, num_applications, settings ));
            Put_Line( outfile, "BU Type");
            Put_Line( outfile, Breakdown_By_Bu_type.To_CDA( tables.bu_type_tab, num_applications, settings ));
            Put_Line( outfile, "Age Range of Head" );
            Put_Line( outfile, Breakdown_By_Age_Range_Of_Head.To_CDA( tables.age_range_tab, num_applications, settings  ));
            new_line( outfile );
            Close( outfile );
            --
            -- docbook skeleton
            --
            Create( outfile, Out_File, docbook_file_name );
            docbook := Make_Docbook_Regime_Section( regime_dir, regime, tables, settings.inc_ctl.summary_slot );
            Put_Line( outfile, To_String( docbook ));
            Close( outfile );
          end;
      end loop Each_Application;
   end Make_HTML_And_Charts;

end Model.OSCR_Output.Formatters;
