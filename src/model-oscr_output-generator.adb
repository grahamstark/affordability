--
--  $Author: graham_s $
--  $Date: 2010-02-11 19:10:25 +0000 (Thu, 11 Feb 2010) $
--  $Revision: 8611 $
--
pragma License( Modified_GPL );

with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Characters.Handling; 

with Base_Model_Types;
with Charging_Examples;
with Data_Constants;
with FRS_Enums;
with FRS_Utils;
with Scotland_Specific_Constants;

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

with Format_Utils;
with HTML_Utils;
with Inequality_Generator;
with Tabulator;
with Tabulator_Commons;
with Text_Utils;

package body Model.OSCR_Output.Generator is

   -- use Ada.Exceptions;
   -- use Base_Model_Types;
   use Ada.Text_IO;
   -- use Data_Constants;
   
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
   package mimt  renames Model.Income_Measure_Types;
   package icp   renames mimt.Income_Measure_Package;
   package ap    renames Model.Charging.Application_Package;
   package tp    renames Model.Charging.Target_Package;
   package mc renames Model.Charging.Charging_Regime_Package;
   
   procedure Increment_Cell_Breakdown( 
      current_breakdown : in out Aggregate_Cell_Breakdown; 
      new_breakdown : Aggregate_Cell_Breakdown;
      weight        : Counter_Type ) is
   begin
      current_breakdown.v1 := current_breakdown.v1 + ( new_breakdown.v1 * weight );
      current_breakdown.v2 := current_breakdown.v2 + ( new_breakdown.v2 * weight );
   end Increment_Cell_Breakdown;

  
   procedure Increment( it : in out Incomes_Table; incs : mimt.Income_Measure_Array; gf : Rate ) is
      i : Positive;
   begin
      for im in mimt.Income_Measure_Type'First .. mimt.Income_Measure_Type'Last loop
         i := mimt.Get_Income_Range( incs( im ) );
         it( i, im ) := it( i, im ) + Counter_Type( gf );
      end loop;
   end Increment;
   
   function Get_Parameters  return pars.Parameters is
      params : pars.Parameters := pars.Get_2007_8_System;
      use Model.Incomes;
   begin
      --
      -- turn disability benefits on (FIXME can't remember why we did this..)
      --
      params.legal_aid.incomesList( disability_living_allowance ) := 1.0;
      params.legal_aid.incomesList( attendance_allowance ) := 1.0;
      params.legal_aid.incomesList( constantattendance_allowance ) := 1.0;
      params.legal_aid.incomesList( social_fund  ) := 1.0;
      params.legal_aid.gross_incomesList( disability_living_allowance ) := 1.0;
      params.legal_aid.gross_incomesList( attendance_allowance ) := 1.0;
      params.legal_aid.gross_incomesList( constantattendance_allowance ) := 1.0;
      params.legal_aid.gross_incomesList( social_fund  ) := 1.0;
      return params;
   end Get_Parameters;
   
   function Generate_Cost_Measures(
      data_path : Unbounded_String;
      run_id    : Unbounded_String;
      regime    : Model.Charging.Charging_Regime; 
      control   : Model.Run_Settings.Settings_Rec;
      observer  : Model.Run_Settings.Run_Observer_Access ) return Table_Set is
      
      use FRS_Enums;
      use Model.Charging;
      use Model.Household;
      use Amount_Package;
      use mimt;
      
      all_tables : Table_Set;
      
      NUM_APPLICATIONS : constant Costs_Range := Costs_Range( ap.Length( regime.applications ));
      grossing_factor : Amount := 0.0;
      income_measures  : mimt.Household_Incomes_Result;
      hh         : Household_Rec;
      num_years  : constant Rate := Rate(control.end_year - control.start_year + 1);
      hh_file    : hh_io.File_Type;
      start_hh   : Natural := 0;
      end_hh     : Natural := 0;
      results    : outp.Household_Result;
      params     : pars.Parameters := Get_Parameters;
      n          : Positive := 1;
      sz         : Natural := 0;
      costs : Amount_List;
      i : Costs_Range;
      disposable_income : Amount := 0.0;
      count_on_income_support : Amount := 0.0;
      outfile : File_Type;
      htmlfile : File_Type;
      below_is_counts : array( 1..2) of Rate := ( 0.0, 0.0 );
      households_processed : Natural := 0;
      aborting : Boolean := False;
    begin
    --
    -- TEMP TEMP TEMP
    -- 
      aborting := observer( run_id, 0, Data_Constants.FIRST_AVAILABLE_DATA_YEAR, 0, rs.run_starting );
      if( aborting ) then
         Put_Line( "aborting" );
         return all_tables;
      end if;
      if( not control.inc_ctl.include_child_care_costs_in_allowances ) then
         put_line( "turning off child care costs" );
         Model.Parameters.Legal_Aid.Turn_Off_Child_Allowances( params.legal_aid );
      end if;
      pars.To_Annual( params );
      Years:
      for year in  control.start_year .. control.end_year loop
         Initialise ( data_path, hh_file, year, sz, False );
         if control.SCOTLAND_ONLY then
            start_hh := soc.FILE_POSITIONS( year, soc.start_pos );
            end_hh   := soc.FILE_POSITIONS( year, soc.end_pos );
         else
            start_hh := 1;
            end_hh   := sz;
         end if;
         households_processed := 0;
         Households:
         for href in  start_hh .. end_hh loop
            hh := Load( hh_file, href );
            households_processed := households_processed + 1;
            if(( households_processed MOD 10 ) = 0) then
               aborting := observer( run_id, households_processed, year, 0, rs.running );
               if( aborting ) then
                  hh_io.Close( hh_file );         
                  return all_tables;
               end if;
            end if;
            if (control.SCOTLAND_ONLY)  and then ( hh.standard_region /= Scotland ) then
               Put_Line( "scottish household in middle of scottish bock " & Data_Constants.Format( hh.sernum ) & " hhref " & Format( href ));
            else
               if( ( href mod 100 ) = 0 ) then
                  Put_Line( "on household " & href'Img & " year " & year'Img );
               end if;
               Annualise( hh );
               Uprate_Household( hh );
               log( Model.runner, "year " & year'Img & " on household " & Data_Constants.Format(hh.sernum) & " hhref " & href'Img );
               grossing_factor := hh.grossing_factor / num_years;
               results := calcs.Calculate( hh, params, control );
                  
               for buno in 1 .. hh.num_benefit_units loop
                  costs := Get_Charge_For_Family( hh.benefit_units(buno), regime );
                  for i in 1 .. NUM_APPLICATIONS loop
                     income_measures.benefit_units( buno ).costs( i ) := Element( costs, Integer(i) );
                  end loop;
                  income_measures.benefit_units( buno ).num_cost_measures := NUM_APPLICATIONS;
               end loop;
               Put_Line( "control.inc_ctl.aggregate_incomes_to " & control.inc_ctl.aggregate_incomes_to'Img );
               
               incm.Calculate_Income_Measures( hh, results, income_measures, control, Grossing_Factor );
               if( control.inc_ctl.aggregate_incomes_to = benefit_unit_level ) then
                  Put_Line( "aggregating to bu level" );
                  Benefit_Units:
                  for buno in 1 .. hh.num_benefit_units loop
                     disposable_income := income_measures.benefit_units( buno ).affordabilities( 1 )( residual_income_level );
                     n := n + 1;
                     if( income_measures.benefit_units( buno ).on_income_support ) then
                        count_on_income_support := count_on_income_support + grossing_factor;
                     end if;
                     if ((income_measures.benefit_units( buno ).incomes( net ) <= income_measures.benefit_units( buno ).poverty_line) and 
                         ( income_measures.benefit_units( buno ).costs( 1 ) > 0.0 ))then  
                        if( income_measures.benefit_units( buno ).on_income_support ) then
                           below_is_counts(1) := below_is_counts(1) + grossing_factor;
                        else
                           below_is_counts(2) := below_is_counts(2) + grossing_factor;                     
                        end if;
                     end if;
                     Breakdown_By_Tenure.Increment( hh.tenure,all_tables.tenure_tab, income_measures.benefit_units(buno), control ); 
                     log( Model.runner, "incrementing hh.regional_stratifier = " & hh.regional_stratifier'Img );
                     if( hh.regional_stratifier /= MISSING ) then
                        Breakdown_By_Region.Increment( hh.regional_stratifier,all_tables.region_tab, income_measures.benefit_units(buno), control );
                     end if;
                     Breakdown_By_Economic_status.Increment( hh.benefit_units(buno).economic_status, all_tables.economic_status_tab, income_measures.benefit_units(buno), control);
                     Breakdown_By_Disablement_status.Increment(  hh.benefit_units(buno).disablement_status, all_tables.disablement_status_tab, income_measures.benefit_units(buno), control );
                     Breakdown_By_Bu_type.Increment( hh.benefit_units(buno).bu_type, all_tables.bu_type_tab, income_measures.benefit_units(buno), control );
                     Breakdown_By_Age_Range_Of_Head.Increment( hh.benefit_units(buno).age_range_of_head, all_tables.age_range_tab, income_measures.benefit_units(buno), control );   
                  end loop Benefit_Units;
               elsif( control.inc_ctl.aggregate_incomes_to = household_level ) then
                     Put_Line( "aggregating to household level" );
                     Breakdown_By_Tenure.Increment( hh.tenure, all_tables.tenure_tab, income_measures.aggregate, control );
                     if( hh.regional_stratifier /= MISSING ) then
                        Breakdown_By_Region.Increment( hh.regional_stratifier,all_tables.region_tab, income_measures.aggregate, control );
                     end if;
                     Breakdown_By_Economic_status.Increment( hh.benefit_units(1).economic_status, all_tables.economic_status_tab, income_measures.aggregate, control);
                     Breakdown_By_Disablement_status.Increment(  hh.benefit_units(1).disablement_status, all_tables.disablement_status_tab, income_measures.aggregate, control );
                     Breakdown_By_Bu_type.Increment( hh.benefit_units(1).bu_type, all_tables.bu_type_tab, income_measures.aggregate, control );
                     Breakdown_By_Age_Range_Of_Head.Increment( hh.benefit_units(1).age_range_of_head, all_tables.age_range_tab, income_measures.aggregate, control );   
               end if;
               income_measures := mimt.New_Output;
            end if;
         end loop Households;
         hh_io.Close( hh_file );
      end loop Years;
      return all_tables;
   end Generate_Cost_Measures;
 
end Model.OSCR_Output.Generator;
