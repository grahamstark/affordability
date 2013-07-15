--
--  $Author: graham_s $
--  $Date: 2008-12-23 11:25:13 +0000 (Tue, 23 Dec 2008) $
--  $Revision: 6564 $
--
pragma License( Modified_GPL );

with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Characters.Handling; 

with Templates_Parser; 

with Base_Model_Types;
with Charging_Examples;
with Data_Constants;
with FRS_Enums;
with FRS_Utils;
with Scotland_Specific_Constants;
with Costs_Tabulator;

with Model;
with Model.Calculations.Complete;
with Model.Calculations.Legal_Aid;
with Model.Calculations.Means_Tested_Benefits;
with Model.Calculations.Non_Means_Tested_Benefits;
with Model.Calculations;

with Model.Income_Measures;
with Model.Income_Measures.Breakdown_Package;
with Model.Incomes;
with Model.Equivalence_Scales;
with Model.Household;
with Model.Output.Legal_Aid;
with Model.Output.Complete;
with Model.Parameters.Complete;
with Model.Parameters.Legal_Aid;
with Model.Parameters.Means_Tested_Benefits;
with Model.Run_Settings;
with Model.Uprate;
with Model.Web_Commons;
with Model.Main_Examples;

with Financial_Functions;
with Format_Utils;
with HTML_Utils;
with Inequality_Generator;
with Tabulator;
with Tabulator_Commons;
with Tax_Utils;
with Text_Utils;
with Optimiser;
with Piecewise_Linear_Generator;
with Text_Utils;

package body Model.OSCR_Output.Generator is

   -- use Ada.Exceptions;
   -- use Base_Model_Types;
   use Ada.Text_IO;
   use Data_Constants;
   
   package adir  renames Ada.Directories;
   package rs    renames Model.Run_Settings;
   package soc   renames Scotland_Specific_Constants;
   package mhh   renames Model.Household;
   package lio   renames Ada.Text_IO.Unbounded_IO;
   package ubstr renames Ada.Strings.Unbounded;
   package calcs renames Model.Calculations.Complete;
   package pars  renames Model.Parameters.Complete;
   package outp  renames Model.Output.Complete;
   package mtb   renames Model.Calculations.Means_Tested_Benefits;
   package incm  renames Model.Income_Measures;
   
   package icp   renames incm.Income_Measure_Package;
   package ap    renames Model.Charging.Application_Package;
   package tp    renames Model.Charging.Target_Package;
   package mc renames Model.Charging.Charging_Regime_Package;
   
   type Incomes_Table is array( incm.Income_Brackets,  incm.Income_Measure_Type ) of Counter_Type;
   
   type Aggregate_Cell_Breakdown is record
      v1 : Counter_Type := 0.0;
      v2 : Counter_Type := 0.0;
   end record;
   
   type Example is record
      hhref : Positive;
      year  : Data_Constants.Data_Years;      
   end record;
   
   procedure Increment_Cell_Breakdown( 
      current_breakdown : in out Aggregate_Cell_Breakdown; 
      new_breakdown : Aggregate_Cell_Breakdown;
      weight        : Counter_Type ) is
   begin
      current_breakdown.v1 := current_breakdown.v1 + ( new_breakdown.v1 * weight );
      current_breakdown.v2 := current_breakdown.v2 + ( new_breakdown.v2 * weight );
   end Increment_Cell_Breakdown;

   
   function To_Dir_Name( u : ubstr.Unbounded_String ) return String is
   begin
      return Ada.Characters.Handling.To_Lower(
         ubstr.To_String( 
            Text_Utils.Censor_String( u )
         )
      );
   end To_Dir_Name;
   
   function Regimes_To_Menu( 
      regimes : Model.Charging.Charging_Regime_List; 
      this_regime : ubstr.Unbounded_String; 
      this_application : ubstr.Unbounded_String ) return String is
      
      use ubstr;
      use Text_Utils;
      use Model.Charging;
      use Model.Income_Measures;
      
      function tbs( s : String ) return Unbounded_String is
         us : Unbounded_String;
      begin 
         return To_Unbounded_String( s & LINE_BREAK );
      end tbs;
      
      function Make_Submenu( regime : Charging_Regime ) return Unbounded_String is
         outs : Unbounded_String;
         application : Application_Type;
         application_name : String := To_Dir_Name( application.name );
         NUM_APPLICATIONS : constant Costs_Range := Costs_Range( ap.Length( regime.applications ));
      begin
         outs := tbs( "<ul id='snavlist'>" );
         Each_Charge:
         for application_no in 1 .. num_applications loop
            declare
                application : Application_Type := ap.Element( regime.applications, application_no );
                application_name : String := To_Dir_Name( application.name );
                regime_name : String := To_Dir_Name( regime.name );                
                relative_dir : String := "/oscr/regime_" &  regime_name & "_application_" & application_name & "/oscr_output.html";               
            begin
               if( this_application /= application.name ) then
                  outs := outs & "     <li>" & LINE_BREAK;
                  outs := outs & "              <a class='navlist' href='" & relative_dir & "'>" & application.name & "</a>" & LINE_BREAK;
                  outs := outs & "     </li>" & LINE_BREAK;         
               else
                  outs := outs & "     <li>" & LINE_BREAK;
                  outs := outs & "              <span class='modelsNav current'>" & application.name & "</span>" & LINE_BREAK;
                  outs := outs & "     </li>" & LINE_BREAK;
               end if;
            end;
         end loop Each_Charge;
         outs := outs & "</ul>" & LINE_BREAK;   
         return outs;
      end Make_Submenu;
      
      NUM_CHARGING_REGIMES : constant Positive := Positive( mc.Length( regimes ) );
      outs : ubstr.Unbounded_String;
      submenu : Unbounded_String;            
   begin
      outs := tbs(   "<div id='nav'>" );
      outs := outs & "  <ul id='navlist'>" & LINE_BREAK;
      Each_Regime: 
      for regime_no in 1 .. NUM_CHARGING_REGIMES loop
         declare
            regime : Charging_Regime := mc.Element( regimes, regime_no );
            regime_name  : String := To_Dir_Name( regime.name );
            application : Application_Type := ap.Element( regime.applications, 1 );
            application_name : String := To_Dir_Name( application.name );            
            relative_dir : String := "/oscr/regime_" &  regime_name & "_application_" & application_name & "/oscr_output.html";               
         begin
            regime := mc.Element( regimes, regime_no );
            if( regime.name /= this_regime ) then
               outs := outs & "     <li>" & LINE_BREAK;
               outs := outs & "              <a class='navlist' href='" & relative_dir & "'>" &  regime.name & "</a>" & LINE_BREAK;
               outs := outs & "     </li>" & LINE_BREAK;         
            else
               outs := outs & "     <li>" & LINE_BREAK;
               outs := outs & "              <span class='modelsNav current'>" & regime.name & "</span>" & LINE_BREAK;
               outs := outs & "     </li>" & LINE_BREAK;
               submenu := Make_Submenu( regime );               
            end if;
         end;
      end loop Each_Regime;
      
      outs := outs & "  </ul>" & LINE_BREAK;
      outs := outs & submenu;
      outs := outs & "</div> " & LINE_BREAK;
      outs := outs & "</div> " & LINE_BREAK;      
      return To_String( outs );
   end Regimes_To_Menu;
   
   procedure Increment( it : in out Incomes_Table; incs : incm.Income_Measure_Array; gf : Rate ) is
      i : Positive;
   begin
      for im in incm.Income_Measure_Type'First .. incm.Income_Measure_Type'Last loop
         i := incm.Get_Income_Range( incs( im ) );
         it( i, im ) := it( i, im ) + Counter_Type( gf );
      end loop;
   end Increment;
   
   function To_CDA( label : String; it : Incomes_Table; print_header : Boolean := False ) return String is
      use Text_Utils;
      use Ada.Strings.Unbounded;
      use incm;
      
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
      for im in incm.Income_Measure_Type'First .. incm.Income_Measure_Type'Last loop
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
   
   
   package Inequality_Package is new Inequality_Generator( Rate=>Rate, Amount=>Amount );

   
   procedure Generate_Income_Measures( control : Model.Run_Settings.Settings_Rec  ) is
      
   use FRS_Enums;
   use Model.Income_Measures;
   
      grossing_factor : Rate := 0.0;
      income_measures  : incm.Household_Incomes_Result;
      settings   : rs.Settings_Rec;
      hh         : mhh.Household_Rec;
      num_years  : constant Rate := Rate(settings.end_year - settings.start_year + 1);
      hh_file    : mhh.hh_io.File_Type;
      start_hh   : Natural := 0;
      end_hh     : Natural := 0;
      results    : outp.Household_Result;
      params     : pars.Parameters := pars.Get_2007_8_System;
      table1     : Incomes_Table := ( others=>(others=>0.0) );
      ten_tables : array( Tenure_Type ) of Incomes_Table := ( others=>(others=>(others=>0.0) ));
      reg_tables : array( Regional_Stratifier ) of Incomes_Table := ( others=>(others=>(others=>0.0) ));
      ec_tables  : array( Benefit_Unit_Economic_Status  ) of Incomes_Table := ( others=>(others=>(others=>0.0) ));
      dis_tables : array( BU_Disabled_Indicator ) of Incomes_Table := ( others=>(others=>(others=>0.0) ));
      bu_tables  : array( HBAI_Benefit_Unit_Type ) of Incomes_Table := ( others=>(others=>(others=>0.0) ));
      age_tables : array( Age_Group ) of Incomes_Table := ( others=>(others=>(others=>0.0) ));
      n          : Positive := 1;
      sz         : Natural := 0;

      quant_list : Inequality_Package.Quantile_List.Vector;
      ineq  : Inequality_Package.Inequality_Array;
      bins  :  Inequality_Package.Quantile_List.Vector;
      income : Amount;
      
      incomes_list : Income_Measure_List.Vector;
      outfile : File_Type;
    begin
    
      Create( outfile, Append_File, ubstr.To_String(control.save_file_name) );
    
      if( not control.inc_ctl.include_child_care_costs_in_allowances ) then
         Model.Parameters.Legal_Aid.Turn_Off_Child_Allowances( params.legal_aid );
      end if;

      Years:
      for year in  settings.start_year .. settings.end_year loop
         mhh.Initialise ( hh_file, year, sz, False );
         pars.To_Annual( params );
         if control.SCOTLAND_ONLY then
            start_hh := soc.FILE_POSITIONS( year, soc.start_pos );
            end_hh   := soc.FILE_POSITIONS( year, soc.end_pos );
         else
            start_hh := 1;
            end_hh   := sz;
         end if;
         
         Households:
         for href in  start_hh .. end_hh loop
            hh := mhh.Load (hh_file, href);
            mhh.Annualise( hh );
            mhh.Uprate_Household( hh );
            log( Model.runner, "year " & year'Img & " on household " & Format(hh.sernum) & " hhref " & href'Img );
            grossing_factor := hh.grossing_factor / num_years;
            results := calcs.Calculate( hh, params, settings );
            incm.Calculate_Income_Measures( hh, results, income_measures, settings, Grossing_Factor );
            
            Benefit_Units:
            for buno in 1 .. hh.num_benefit_units loop
               if( n < 20 ) then
                  put_line( "Household " & mhh.To_String( hh ) );
                  put_line( "Results " & outp.To_String( hh.benefit_units( buno ), results.benefit_units( buno ) ));
                  put_line( "Income Measures" & incm.To_String( income_measures.benefit_units(buno) ));
               elsif ( n = 20 ) then
                  Remove_Target( Model.legal_aid_calcs );
                  Remove_Target( Model.income_calcs );
                  Remove_Target( Model.charging_model );
               end if;
               n := n + 1;
               Increment( 
                  table1, 
                  income_measures.benefit_units(buno).incomes, 
                  grossing_factor );
               Increment( 
                  ten_tables( hh.tenure ), 
                  income_measures.benefit_units(buno).incomes, 
                  grossing_factor ); 
               Increment( 
                  reg_tables( hh.regional_stratifier ), 
                  income_measures.benefit_units(buno).incomes, 
                  grossing_factor );
               Increment( 
                  ec_tables( hh.benefit_units(buno).economic_status ), 
                  income_measures.benefit_units(buno).incomes, 
                  grossing_factor );
               Increment( 
                  dis_tables( hh.benefit_units(buno).disablement_status ), 
                  income_measures.benefit_units(buno).incomes, 
                  grossing_factor );
               Increment( 
                  bu_tables( hh.benefit_units(buno).bu_type ), 
                  income_measures.benefit_units(buno).incomes, 
                  grossing_factor );
               Increment( 
                  age_tables( hh.benefit_units(buno).age_range_of_head ), 
                  income_measures.benefit_units(buno).incomes, 
                  grossing_factor );
               income := income_measures.benefit_units(buno).incomes( Net );
               if( income > 0.0 ) then
                  Inequality_Package.Quantile_List.append(
                     quant_list, 
                     (population=>Grossing_Factor, 
                     income=> income ));  
               elsif ( buno = 1 ) then
                  put_line( "**!! Negative or zero "& Net'Img & " for hh ref " & href'Img & " bu " & buno'img & " year " & year'Img );
                  put_line( mhh.To_String( hh ) );
               end if;  
               Add_Observation( income_measures.benefit_units(buno), incomes_list );
   
            end loop Benefit_Units;
            results := outp.New_Output;
            income_measures := incm.New_Output;
         end loop Households;
         mhh.hh_io.Close( hh_file );
      end loop Years;
      put_line( To_CDA( "ALL", table1, True ));
      put_line( outfile, "TENURE" );
      for t in Tenure_Type'First .. Tenure_Type'Last loop
         put_line( outfile, To_CDA( Tenure_Type'Image(t), ten_tables( t ), t = Tenure_Type'First ) );
      end loop;
      put_line( outfile, "REGIONS" );
      for t in Scotland_Specific_Constants.Scottish_Regional_Stratifier loop
         put_line( outfile, To_CDA( Regional_Stratifier'Image(t), reg_tables( t ),t=Scotland_Specific_Constants.Scottish_Regional_Stratifier'First ) );
      end loop;
      put_line( outfile, "Benefit_Unit_Economic_Status" );
      for t in Benefit_Unit_Economic_Status'First .. Benefit_Unit_Economic_Status'Last loop
         put_line( outfile, To_CDA( Benefit_Unit_Economic_Status'Image(t), ec_tables( t ), t=Benefit_Unit_Economic_Status'First ) );
      end loop;
      put_line( outfile, "BU_Disabled_Indicator" );
      for t in BU_Disabled_Indicator'First .. BU_Disabled_Indicator'Last loop
         put_line( outfile, To_CDA( BU_Disabled_Indicator'Image(t), dis_tables( t ),t=BU_Disabled_Indicator'First ) );
      end loop;
      put_line( outfile, "HBAI_Benefit_Unit_Type" );
      for t in HBAI_Benefit_Unit_Type'First .. HBAI_Benefit_Unit_Type'Last loop
         put_line( To_CDA( HBAI_Benefit_Unit_Type'Image(t), bu_tables( t ),t=HBAI_Benefit_Unit_Type'First ) );
      end loop;
      put_line( outfile, "Age_Group" );
      for t in Age_Group'First .. Age_Group'Last loop
         put_line( outfile, To_CDA( Age_Group'Image(t), age_tables( t ),t=Age_Group'First ));
      end loop;
      --
      -- inequality
      --
      Inequality_Package.Sort_By_Income( quant_list );
      bins := Inequality_Package.Binify( quant_list, 10 );
      n :=  Positive(Inequality_Package.Quantile_List.Length( bins ));
      for p in 1 .. n loop
          put_line( outfile, Positive'Image(p) & " = " & Inequality_Package.To_String( Inequality_Package.Quantile_List.element( bins, p ) ));
      end loop;
      ineq := Inequality_Package.generate( quant_list );
      put_line( outfile, "Inequality Measures for Net Incomes excl. housing costs" );
      put_line( outfile, Inequality_Package.To_String( ineq ) );
      Close( outfile );
   end Generate_Income_Measures;
   
   function BU_Complete_To_String( 
      bu      : mhh.Model_Benefit_Unit; 
      tbpos   : outp.Benefit_Unit_Result;
      incomes : incm.One_Complete_Income_Measure_Output ) return String is
   use Text_Utils;
   use Ada.Strings.Unbounded;
   
      outs : Unbounded_String := To_Unbounded_String( "==== BENEFIT UNIT ===== " );
   begin      
      outs := outs & mhh.To_String( bu );
      outs := outs & " Tax Benefit Position " & LINE_BREAK;
      outs := outs & outp.To_String( bu, tbpos );
      outs := outs & " Incomes Position " & LINE_BREAK;
      outs := outs & incm.To_String( incomes );
      return To_String(outs);
   end BU_Complete_To_String;
   
   package Breakdown_By_Tenure is new incm.Breakdown_Package( Breakdown_Var => FRS_Enums.Tenure_Type, Get_Breakdown_Description => FRS_Enums.Pretty_Print ); 
   package Breakdown_By_Region is new incm.Breakdown_Package( Breakdown_Var => Scotland_Specific_Constants.Scottish_Regional_Stratifier, Get_Breakdown_Description => FRS_Enums.Pretty_Print );  
   package Breakdown_By_Economic_status is new incm.Breakdown_Package( Breakdown_Var => FRS_Enums.Benefit_Unit_Economic_Status, Get_Breakdown_Description => FRS_Enums.Pretty_Print ); 
   package Breakdown_By_Disablement_status is new incm.Breakdown_Package( Breakdown_Var => FRS_Enums.BU_Disabled_Indicator, Get_Breakdown_Description => FRS_Enums.Pretty_Print ); 
   package Breakdown_By_Bu_type is new incm.Breakdown_Package( Breakdown_Var => FRS_Enums.HBAI_Benefit_Unit_Type, Get_Breakdown_Description => FRS_Enums.Pretty_Print ); 
   package Breakdown_By_Age_Range_Of_Head is new incm.Breakdown_Package( Breakdown_Var => FRS_Enums.Age_Group, Get_Breakdown_Description => FRS_Enums.Pretty_Print );

   
   type Table_Set is record
      tenure_tab : Breakdown_By_Tenure.Table_Rec;
      region_tab : Breakdown_By_Region.Table_Rec;
      economic_status_tab : Breakdown_By_Economic_status.Table_Rec;
      disablement_status_tab : Breakdown_By_Disablement_status.Table_Rec;
      bu_type_tab : Breakdown_By_Bu_type.Table_Rec;
      age_range_tab : Breakdown_By_Age_Range_Of_Head.Table_Rec;
   end record;
   
   package Table_Set_Package is new Ada.Containers.Vectors( Element_Type => Table_Set, Index_Type => Positive );
   subtype Table_Set_List is Table_Set_Package.Vector;
   package ts renames Table_Set_Package;
   
   function Make_Ready_Reckoner_Section(
      regime : Model.Charging.Charging_Regime;
      tables : Table_Set ) return String is
      use Templates_Parser;
      use Model.Income_Measures;
      use Ada.Strings.Unbounded;
      use Model.Web_Commons;
      use Model.Charging;

      complete_tables :  Unbounded_String;
   begin
      for affordability_type in Affordability_Measure_Type loop
         declare
            section_translations : Translate_Set;
            rows : Vector_Tag;      
            NUM_APPLICATIONS : constant Costs_Range := Costs_Range( ap.Length( regime.applications ));
            summary : Breakdown_By_Tenure.Summary_Info_Rec;
            single_table_str : Unbounded_String;
         begin
            Insert( section_translations, Assoc( "WHICH-MEASURE",
               Model.Income_Measures.Affordability_Measure_Type_Name( affordability_type )));
            Insert( section_translations, Assoc( "HEADER-ROW",
               Breakdown_By_Tenure.Make_RR_Row( affordability_type )));
            for application_no in 1 .. NUM_APPLICATIONS loop
               declare
                  application : Application_Type := 
                     Application_Package.Element( regime.applications, application_no );
                  application_name : String := To_Dir_Name( application.name );
                  charge : Amount := Model.Main_Examples.CHARGES( application_no );
               begin
                  summary := 
                     Breakdown_By_Tenure.Make_Summary_Data( tables.tenure_tab, application_no );
                  rows := rows & Breakdown_By_Tenure.Make_RR_Row( charge, summary.totals.graph_data( affordability_type ) ); 
               end;
            end loop;
            Insert( section_translations, Assoc( "ROWS", rows ));
            single_table_str := Parse( 
               Model.Web_Commons.TEMPLATE_COMPONENTS_PATH & "ready_reckoner_table.thtml", section_translations );
            complete_tables := complete_tables & single_table_str;
         end;
      end loop;
      return To_String( complete_tables );
   end Make_Ready_Reckoner_Section;
   
   
   function Make_Docbook_Regime_Section(
      regime : Model.Charging.Charging_Regime;
      tables : Table_Set ) return String is
      use Templates_Parser;
      use Model.Income_Measures;
      use Ada.Strings.Unbounded;
      use Model.Web_Commons;
      use Model.Charging;

      regime_dir : String := To_Dir_Name( regime.name );
      individual_sections : Vector_Tag;      
      complete_section : Vector_Tag;      
      section_translations : Translate_Set;
      NUM_APPLICATIONS : constant Costs_Range := Costs_Range( ap.Length( regime.applications ));
      summary : Breakdown_By_Tenure.Summary_Info_Rec;
      single_section_str : Unbounded_String;
   begin
      
      Insert( section_translations, Assoc( "HEADER", regime.name )); 
      Insert( section_translations, Assoc( "DESCRIPTION", regime.description ));
      for application_no in 1 .. NUM_APPLICATIONS loop 
         declare
            application : Application_Type := 
               Application_Package.Element( regime.applications, application_no );
            application_name : String := To_Dir_Name( application.name );
            html_output_dir : String := "regime_" &  regime_dir & "_application_" & application_name;
            full_dir : String := "/var/www/oscr/" & html_output_dir;
         begin
            summary := 
               Breakdown_By_Tenure.Make_Summary_Data( tables.tenure_tab, application_no );
            single_section_str := 
               Breakdown_By_Tenure.Make_Docbook_Section( 
                  chart_directory => full_dir, 
                  section_header => To_String( application.description ) , 
                  summary_info => summary.totals,
                  html_output_dir => html_output_dir );
            individual_sections := individual_sections & single_section_str;
         end;
      end loop;
      Insert( section_translations, Assoc( "SECTIONS", individual_sections ));
      return Parse( 
         Model.Web_Commons.TEMPLATE_COMPONENTS_PATH & "single_docbook_section.thtml", section_translations );      
   end Make_Docbook_Regime_Section;
   
   function Make_Docbook_Complete( 
      regimes     : Model.Charging.Charging_Regime_List;
      table_list : Table_Set_List ) return String is
      use Ada.Strings.Unbounded;
      NUM_CHARGING_REGIMES : constant Positive := Positive( mc.Length( regimes ) );
      regime  : Model.Charging.Charging_Regime;
      outs   : Unbounded_String;
   begin
      for regime_no in 2 .. NUM_CHARGING_REGIMES loop -- RR is 1, so skip it
            outs := outs & Make_Docbook_Regime_Section(
               mc.Element( regimes, regime_no ),
               ts.Element( table_list, regime_no ));
      end loop;
      return To_String( outs );
   end Make_Docbook_Complete;
   
   procedure Make_HTML_And_Charts(
      regime_name,
      application_name,
      regime_description,
      application_description,
      relative_dir     : String;
      output_directory : String;
      tables : Table_Set; 
      control : Model.Run_Settings.Settings_Rec; 
      application_no : Model.Income_Measures.Costs_Range; 
      title : String; 
      id : String;
      html : out ubstr.Unbounded_String;
      charts : out ubstr.Unbounded_String;
      menu  : String ) is
      
      use Templates_Parser;
      use type Templates_Parser.Vector_Tag;
      use ubstr;
      use Model.Income_Measures;
  
      subtype OSCR_TABLE_SIZE is Integer range 1 .. 100;
      subtype OSCR_Translate_Table is Templates_Parser.Translate_Table( OSCR_TABLE_SIZE );
      
      html_table_bocks : Templates_Parser.Vector_Tag;
      
      tenure_summary : constant Breakdown_By_Tenure.Summary_Info_Rec := 
         Breakdown_By_Tenure.Make_Summary_Data( tables.tenure_tab, application_no );
      region_summary : Breakdown_By_Region.Summary_Info_Rec:= 
         Breakdown_By_Region.Make_Summary_Data( tables.region_tab, application_no );
      economic_status_summary : Breakdown_By_Economic_status.Summary_Info_Rec  := 
         Breakdown_By_Economic_status.Make_Summary_Data( tables.economic_status_tab, application_no );
      disablement_status_summary : Breakdown_By_Disablement_status.Summary_Info_Rec := 
         Breakdown_By_Disablement_status.Make_Summary_Data( tables.disablement_status_tab, application_no );
      bu_type_summary : Breakdown_By_Bu_type.Summary_Info_Rec := 
         Breakdown_By_Bu_type.Make_Summary_Data( tables.bu_type_tab, application_no );
      age_range_summary : Breakdown_By_Age_Range_Of_Head.Summary_Info_Rec := 
         Breakdown_By_Age_Range_Of_Head.Make_Summary_Data( tables.age_range_tab, application_no );
      
      translations :  OSCR_Translate_Table;
      blocks : Templates_Parser.Vector_Tag;
      
      s2 : Unbounded_String;
      
   begin      
       charts := To_Unbounded_String( "" );
       html := To_Unbounded_String( "" );
       s2 := Breakdown_By_Tenure.Make_Single_Table(
         relative_dir,
         "All of Scotland", "tenure", tenure_summary.totals, 
         FRS_Enums.Tenure_Type'First, 
         True ); 
        
       translations( 1 ) := Templates_Parser.Assoc ("AFFORDABLE-RESIDUAL-INCOME-POPN", 
         Format_With_Commas( 100.0 - tenure_summary.totals.unaffordable( residual_income_level ), False ));
       translations( 2 ) := Templates_Parser.Assoc ("UNAFFORDABLE-COST-DISPOSABLE-RATIO-POPN",
         Format_With_Commas( tenure_summary.totals.unaffordable( cost_disposable_ratio ), False )); -- FIXME fwc only becuase only one to supress the decimal
       translations( 3 ) := Templates_Parser.Assoc ( "ALL-POPULATION", To_String( s2 ) );
       translations( 4 ) := Templates_Parser.Assoc ( "TITLE", title );
       translations( 5 ) := Templates_Parser.Assoc ( "RELATIVE_DIR", relative_dir );
       blocks := blocks & Breakdown_By_Tenure.To_HTML( 
            relative_dir,
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
            relative_dir,
            economic_status_summary,
            "Breakdown by Economic Status", 
            "economic_status",
            False );
       blocks := blocks & Breakdown_By_Disablement_Status.To_HTML( 
            relative_dir,
            disablement_status_summary,
            "Breakdown by Disablement Status", 
            "disablement_status",
            False );
       blocks := blocks & Breakdown_By_Bu_type.To_HTML( 
            relative_dir,
            bu_type_summary,
            "Breakdown by Benefit Unit Type", 
            "bu_type",
            False );
       blocks := blocks & Breakdown_By_Age_Range_Of_Head.To_HTML(
            relative_dir,
            age_range_summary,
            "Breakdown by Age Range of Head", 
            "age_of_head",
            False );
       translations( 6 ) := Templates_Parser.Assoc ( "TABLE-BLOCKS", blocks );
       translations( 7 ) := Templates_Parser.Assoc ( "REGIME_DESCRIPTION", regime_description );
       translations( 8 ) := Templates_Parser.Assoc ( "APPLICATION_DESCRIPTION", application_description );
       translations( 9 ) := Templates_Parser.Assoc ( "MENU", menu );
       translations( 10 ) := Templates_Parser.Assoc ( "REGIME_NAME", regime_name );
       translations( 11 ) := Templates_Parser.Assoc ( "APPLICATION_NAME", application_name );
       html := Templates_Parser.Parse( Model.Web_Commons.TEMPLATE_COMPONENTS_PATH & "output" & ".thtml", translations );
       
       charts := charts & Breakdown_By_Tenure.Write_Chart_Block( output_directory, "tenure", tenure_summary );
       charts := charts & Breakdown_By_Region.Write_Chart_Block( output_directory,"regions", region_summary );
       charts := charts & Breakdown_By_Economic_Status.Write_Chart_Block( output_directory,"economic_status", economic_status_summary );
       charts := charts & Breakdown_By_Disablement_status.Write_Chart_Block( output_directory,"disablement_status", disablement_status_summary );
       charts := charts & Breakdown_By_Bu_type.Write_Chart_Block( output_directory,"bu_type", bu_type_summary );
       charts := charts & Breakdown_By_Age_Range_Of_Head.Write_Chart_Block( output_directory, "age_of_head", age_range_summary );
       
   end Make_HTML_And_Charts;
   
   
   procedure Generate_Cost_Measures( 
      regimes  : Model.Charging.Charging_Regime_List; 
      control  : Model.Run_Settings.Settings_Rec;
      observer : Model.Run_Settings.Run_Observer_Access ) is
      -- Model.Charging.Charging_Regime
      use FRS_Enums;
      use Model.Income_Measures;
      use Model.Charging;
      use Model.Household;
      use Amount_Package;
      tables : Table_Set_List;
      these_tables : Table_Set;
      
      num_applications : Costs_Range;
      NUM_CHARGING_REGIMES : constant Positive := Positive( mc.Length( regimes ) );
      
      grossing_factor : Amount := 0.0;
      income_measures  : incm.Household_Incomes_Result;
      settings   : rs.Settings_Rec;
      hh         : Household_Rec;
      num_years  : constant Rate := Rate(settings.end_year - settings.start_year + 1);
      hh_file    : hh_io.File_Type;
      start_hh   : Natural := 0;
      end_hh     : Natural := 0;
      results    : outp.Household_Result;
      params     : pars.Parameters := pars.Get_2007_8_System;
      n          : Positive := 1;
      sz         : Natural := 0;
      incomes_list : Income_Measure_List.Vector;
      costs : Amount_List;
      i : Costs_Range;
      disposable_income : Amount := 0.0;
      outfile : File_Type;
      htmlfile : File_Type;
      below_is_counts : array( 1..2) of Rate := ( 0.0, 0.0 );
      regime  : Model.Charging.Charging_Regime;
    begin
      Put_Line( "Generate_Cost_Measures entered " );
      observer( control.run_id, 0, FIRST_AVAILABLE_DATA_YEAR, 0, rs.run_starting );
      for sys_no in 1 .. NUM_CHARGING_REGIMES loop
         ts.Append( tables, these_tables );
      end loop;
      
      if( not control.inc_ctl.include_child_care_costs_in_allowances ) then
         put_line( "turning off child care costs" );
         Model.Parameters.Legal_Aid.Turn_Off_Child_Allowances( params.legal_aid );
      end if;
      pars.To_Annual( params );
      Years:
      for year in  settings.start_year .. settings.end_year loop
         Initialise ( hh_file, year, sz, False );
         if control.SCOTLAND_ONLY then
            start_hh := soc.FILE_POSITIONS( year, soc.start_pos );
            end_hh   := soc.FILE_POSITIONS( year, soc.end_pos );
         else
            start_hh := 1;
            end_hh   := sz;
         end if;
         Households:
         for href in  start_hh .. end_hh loop
            
            hh := Load( hh_file, href );
            if(( href MOD 10 ) = 0) then
               observer( control.run_id, href, year, 0, rs.running );
            end if;
            if (control.SCOTLAND_ONLY)  and then ( hh.standard_region /= Scotland ) then
               put_line( "scottish household in middle of scottish bock " & Format(hh.sernum) & " hhref " & Format( href ));
            else
               put_line( "on houshold " & href'Img & " year " & year'Img );
               Annualise( hh );
               Uprate_Household( hh );
               log( Model.runner, "year " & year'Img & " on household " & Format(hh.sernum) & " hhref " & href'Img );
               grossing_factor := hh.grossing_factor / num_years;
               results := calcs.Calculate( hh, params, settings );
                  
               Charging_Regimes: for charging_regime_no in 1 .. NUM_CHARGING_REGIMES loop
                  regime := mc.Element( regimes, charging_regime_no );
                  num_applications := Costs_Range( ap.Length( regime.applications ));
                  these_tables := ts.Element( tables, charging_regime_no );
                  for buno in 1 .. hh.num_benefit_units loop
                     costs := Get_Charge_For_Family( hh.benefit_units(buno), regime );
                     for i in 1 .. num_applications loop
                        income_measures.benefit_units( buno ).costs( i ) := Element( costs, Integer(i) );
                     end loop;
                     income_measures.benefit_units( buno ).num_cost_measures := num_applications;
                  end loop;
                  incm.Calculate_Income_Measures( hh, results, income_measures, settings, Grossing_Factor );
                  if( settings.inc_ctl.aggregate_incomes_to = benefit_unit_level ) then
                     Benefit_Units:
                     for buno in 1 .. hh.num_benefit_units loop
                        disposable_income := income_measures.benefit_units( buno ).affordabilities( 1 )( residual_income_level );
                        n := n + 1;
                        if ((income_measures.benefit_units( buno ).incomes( net ) <= income_measures.benefit_units( buno ).poverty_line) and 
                            ( income_measures.benefit_units( buno ).costs( 1 ) > 0.0 ))then  
                           if( income_measures.benefit_units( buno ).on_income_support ) then
                              below_is_counts(1) := below_is_counts(1) + grossing_factor;
                           else
                              below_is_counts(2) := below_is_counts(2) + grossing_factor;                     
                           end if;
                        end if;
                        Breakdown_By_Tenure.Increment( hh.tenure,these_tables.tenure_tab, income_measures.benefit_units(buno), control ); 
                        log( Model.runner, "incrementing hh.regional_stratifier = " & hh.regional_stratifier'Img );
                        if( hh.regional_stratifier /= MISSING ) then
                           Breakdown_By_Region.Increment( hh.regional_stratifier,these_tables.region_tab, income_measures.benefit_units(buno), control );
                        end if;
                        Breakdown_By_Economic_status.Increment( hh.benefit_units(buno).economic_status, these_tables.economic_status_tab, income_measures.benefit_units(buno), control);
                        Breakdown_By_Disablement_status.Increment(  hh.benefit_units(buno).disablement_status, these_tables.disablement_status_tab, income_measures.benefit_units(buno), control );
                        Breakdown_By_Bu_type.Increment( hh.benefit_units(buno).bu_type, these_tables.bu_type_tab, income_measures.benefit_units(buno), control );
                        Breakdown_By_Age_Range_Of_Head.Increment( hh.benefit_units(buno).age_range_of_head, these_tables.age_range_tab, income_measures.benefit_units(buno), control );   
                        -- Add_Observation( income_measures.benefit_units(buno), incomes_list );
                     end loop Benefit_Units;
                  elsif( settings.inc_ctl.aggregate_incomes_to = household_level ) then
                        Breakdown_By_Tenure.Increment( hh.tenure, these_tables.tenure_tab, income_measures.aggregate, control );
                        if( hh.regional_stratifier /= MISSING ) then
                           Breakdown_By_Region.Increment( hh.regional_stratifier,these_tables.region_tab, income_measures.aggregate, control );
                        end if;
                        Breakdown_By_Economic_status.Increment( hh.benefit_units(1).economic_status, these_tables.economic_status_tab, income_measures.aggregate, control);
                        Breakdown_By_Disablement_status.Increment(  hh.benefit_units(1).disablement_status, these_tables.disablement_status_tab, income_measures.aggregate, control );
                        Breakdown_By_Bu_type.Increment( hh.benefit_units(1).bu_type, these_tables.bu_type_tab, income_measures.aggregate, control );
                        Breakdown_By_Age_Range_Of_Head.Increment( hh.benefit_units(1).age_range_of_head, these_tables.age_range_tab, income_measures.aggregate, control );   
                        -- Add_Observation( income_measures.aggregate, incomes_list );
                  end if;
                  ts.Replace_Element( tables, charging_regime_no, these_tables );                             
                  income_measures := incm.New_Output;
               end loop Charging_Regimes;
               
               results := outp.New_Output;
            end if;
         end loop Households;
         hh_io.Close( hh_file );
      end loop Years;
      observer( control.run_id, 0, FIRST_AVAILABLE_DATA_YEAR, 0, rs.generating_output );

      Each_Regime: 
      for regime_no in 1 .. NUM_CHARGING_REGIMES loop
         regime := mc.Element( regimes, regime_no );
         num_applications := Costs_Range( ap.Length( regime.applications ));
         these_tables := ts.Element( tables, regime_no );   
         
         Each_Charge:
         for application_no in 1 .. num_applications loop
            declare
               application : Application_Type := Application_Package.Element( regime.applications, application_no );
               application_name : String := To_Dir_Name( application.name );
               regime_name : String := To_Dir_Name( regime.name );
               relative_dir : String := "regime_" &  regime_name & "_application_" & application_name;
               full_dir : String := "/var/www/oscr/" & relative_dir;
               cda_file_name : String := full_dir & "/" & ubstr.To_String(control.save_file_name);
               html_filename : String := full_dir & "/oscr_output.html";
               charts_filename : String := full_dir & "/oscr_charts_driver.txt";
               id : String := "id_"&Format( regime_no );
               html, charts  : ubstr.Unbounded_String;
               menu : String := Regimes_To_Menu( regimes, regime.name, application.name ); 
            begin
               if( not adir.Exists( full_dir )) then 
                  adir.Create_Directory( full_dir );
               end if;
               Make_HTML_And_Charts( 
                  ubstr.To_String( regime.name ),
                  ubstr.To_String( application.name ),
                  ubstr.To_String( regime.description ), 
                  ubstr.To_String( application.description ), 
                  relative_dir, 
                  full_dir, 
                  these_tables, 
                  control, 
                  application_no, 
                  regime_name, 
                  id, 
                  html, 
                  charts,
                  menu );
               Create( outfile, Append_File, html_filename );
               Put_Line( outfile, ubstr.To_String( html ));
               Close( outfile );
               Create( outfile, Append_File, charts_filename );
               Put_Line( outfile, ubstr.To_String( charts ));
               Close( outfile );
               --
               -- note this puts a duplicate file in each application direcory: all we have time for
               -- since all the applications are in each table set
               -- 
               Create( outfile, Append_File, cda_file_name );
               put_line( outfile, ubstr.To_String(regime.name) );
               put_line( outfile, "TENURE" );
               put_line( outfile,  Breakdown_By_Tenure.To_CDA( these_tables.tenure_tab, num_applications, control ));
               -- put_line( outfile, "REGION" );
               -- put_line( outfile, Breakdown_By_Region.To_CDA( these_tables.region_tab, num_applications, control ));
               put_line( outfile, "Economic_status" );
               put_line( outfile, Breakdown_By_Economic_status.To_CDA( these_tables.economic_status_tab, num_applications, control ));
               put_line( outfile, "Disablement_status");
               put_line( outfile, Breakdown_By_Disablement_status.To_CDA( these_tables.disablement_status_tab, num_applications, control ));
               put_line( outfile, "BU Type");
               put_line( outfile, Breakdown_By_Bu_type.To_CDA( these_tables.bu_type_tab, num_applications, control ));
               put_line( outfile, "Age Range of Head" );
               put_line( outfile, Breakdown_By_Age_Range_Of_Head.To_CDA( these_tables.age_range_tab, num_applications, control  ));
               new_line( outfile );
               Close( outfile );               
            end;
         end loop Each_Charge;
      end loop Each_Regime;
      Create( outfile, Append_File, "docbook_insert.xml" );
      Put_Line( outfile, Make_Docbook_Complete( regimes, tables ));
      Close( outfile );
      
      Create( outfile, Append_File, "ready_reckoner_insert.xml" );
      Put_Line( outfile, 
         Make_Ready_Reckoner_Section( 
            mc.Element( regimes, 1 ), 
            ts.Element( tables, 1 )));
      Close( outfile );
      observer( control.run_id, 0, FIRST_AVAILABLE_DATA_YEAR, 0, rs.complete );

   end Generate_Cost_Measures;
 
end Model.OSCR_Output.Generator;
