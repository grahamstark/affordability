--
--  $Author: graham_s $
--  $Date: 2010-06-05 20:45:46 +0100 (Sat, 05 Jun 2010) $
--  $Revision: 8919 $
--

pragma License( Modified_GPL );

with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;

with Gnat.OS_Lib;

with AUnit.Assertions;             
with AWS.Session;

with Base_Model_Types;
with Charging_Examples;
with Costs_Tabulator;
with Data_Constants;
with FRS_Enums;
with FRS_Utils;
with Financial_Functions;
with Format_Utils;
with HTML_Utils;
with Inequality_Generator;
with OSCR_Callbacks;
with Optimiser;
with Piecewise_Linear_Generator;
with Private_School_Examples;
with Scotland_Specific_Constants;
with T_Utils;
with Tabulator;
with Tabulator_Commons;
with Tax_Utils;
with Templates_Parser; 
with Test_Households;
with Text_Utils;
with Utils;
with Web_Utils;

with Model.Calculations.Complete;
with Model.Calculations.Legal_Aid;
with Model.Calculations.Means_Tested_Benefits;
with Model.Calculations.Non_Means_Tested_Benefits;
with Model.Calculations;
with Model.Charging.IO;
with Model.Charging;
with Model.Charging.Buffer;
with Model.Equivalence_Scales;
with Model.Household;
with Model.Income_Measures;
with Model.Income_Measure_Types;
with Model.Incomes;
with Model.Main_Examples;
with Model.OSCR_Output.Formatters;
with Model.OSCR_Output.Generator;
with Model.OSCR_Output.Formatters;
with Model.OSCR_Output;
with Model.Output.Complete;
with Model.Output.Legal_Aid;
with Model.Parameters.Complete;
with Model.Parameters.Legal_Aid;
with Model.Parameters.Means_Tested_Benefits;
with Model.Run_Settings;
with Model.Run_Settings.IO;
with Model.Uprate;
with Model.Web_Commons;
with Model.Web_Constants;
with Model;

--  with User_IO;
package body OSCR_Tests is


   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use type Utils.Error_Type;
   use Ada.Exceptions;
   use Base_Model_Types;
   use Ada.Text_IO;
   use Text_Utils;
   use AUnit;
   
   package rs renames Model.Run_Settings;
   package soc renames Scotland_Specific_Constants;
   package mhh renames Model.Household;

   package lio renames Ada.Text_IO.Unbounded_IO;
   package ubstr renames Ada.Strings.Unbounded;
   package UK_Format_Utils is new Format_Utils( Counter_Type => Counter_Type, Float_Type => Rate );
   package UK_HTML_Utils is new HTML_Utils( Counter_Type => Counter_Type, Rate => Rate );
   package logger renames Model.UK_Logger;
   package calcs renames Model.Calculations.Complete;
   package pars  renames Model.Parameters.Complete;
   package outp  renames Model.Output.Complete;
   package mtb   renames Model.Calculations.Means_Tested_Benefits;
   package incmt  renames Model.Income_Measure_Types;
   package incm  renames Model.Income_Measures;
   package icp renames incmt.Income_Measure_Package;
   package ap renames Model.Charging.Application_Package;
   package tp renames Model.Charging.Target_Package;
   package cp renames Model.Charging.Charges_Package;
   package moo renames Model.OSCR_Output;
   package moog renames Model.OSCR_Output.Generator;
   package moof renames Model.OSCR_Output.Formatters;
   package rp renames Model.Charging.Charging_Regime_Package;
   package mco renames Model.Charging.IO;
   package mwcn renames Model.Web_Constants;
   package mwc renames Model.Web_Commons;   --
   --
   --
   function Safe_Delete_Directory_Tree( filename : String; username : Unbounded_String ) return Boolean is
      dir : Unbounded_String := TuS(Ada.Directories.Containing_Directory( filename ));
   begin
      Put_Line( "deleting: testing dir " & TS( dir ));
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
      if( not Ada.Directories.Exists( TS(dir) )) then
         --
         -- No such directory: bale out
         --
         return False;
      end if;
      Put_Line( "Intending to Delete path " & TS( dir ));
      return True;
      -- Ada.Directories.Delete_Tree( TS(dir) );
      exception
         when Others => return False;
   end Safe_Delete_Directory_Tree;
 
   
   
   
   subtype OSCR_TABLE_SIZE is Integer range 1 .. 4000;
   
   type Parser_Tag_Array is
     array (1 .. 30) of Templates_Parser.Tag;

   subtype OSCR_Translate_Table is Templates_Parser.Translate_Table( OSCR_TABLE_SIZE );
   
   type Summary_Info is record
      residual_unaffordable : Amount := 0.0;
      disposable_unaffordable  : Amount := 0.0;
      gross_unaffordable  : Amount := 0.0;
   end record;
   
   procedure Test_Delete_Tree( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin
      Assert( Safe_Delete_Directory_Tree( "/home/graham_s/VirtualWorlds/projects/oscr/model/workdir/graham_s/medical/regime.txt", TuS( "graham_s" )), 
         "should delete /home/graham_s/VirtualWorlds/projects/oscr/model/workdir/graham_s/medical/regime.txt");
      Assert( not Safe_Delete_Directory_Tree( "/home/graham_s/VirtualWorlds/projects/oscr/model/workdir/graham_s/xxmedical/regime.txt", TuS( "graham_s" )), 
         "shouldn't delete /home/graham_s/VirtualWorlds/projects/oscr/model/workdir/graham_s/medical/regime.txt" );
      Assert( not Safe_Delete_Directory_Tree( "/zzhome/graham_s/VirtualWorlds/projects/oscr/model/workdir/graham_s/medical/regime.txt", TuS( "graham_s" )),
         " shouldn't delete /zzhome/graham_s/VirtualWorlds/projects/oscr/model/workdir/graham_s/medical/regime.txt" );
   end Test_Delete_Tree;
   
   procedure Test_Dump_Examples( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.Charging;
   use Charging_Regime_Package;
   use Text_Utils;
      all_charges : Charging_Regime_List := Model.Main_Examples.Make_All_Regimes;
      n : constant Positive := Positive(Length( all_charges )); 
      f : File_Type;
   begin
      for i in 1 .. n loop
         declare
            regime : Charging_Regime := Element( all_charges, i );
            out_name : String := "tmp/" & TS(Censor_String( regime.name )) & ".txt";
         begin
            Create( f, out_file, out_name );
            Model.Charging.IO.Write( f,regime );
            Close( f );
         end;
      end loop;
   end Test_Dump_Examples;
   
   procedure Test_Search_Directories( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Ada.Calendar;
   use Ada.Directories;
   
   procedure Walk (Dir_Name : String; File_Pattern : String ) is
      
      procedure Print (Item : Directory_Entry_Type) is
      begin
         Ada.Text_IO.Put_Line( "walking " & Full_Name (Item) );
      end Print;
      
      procedure Walk (Item : Directory_Entry_Type) is
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Walk (Full_Name (Item), File_Pattern);
         end if;
      --exception
      --   when O => null;
      end Walk;
      
   begin
      Search( Dir_Name, File_Pattern, (others => True), Print'Access );
      Search( Dir_Name, "", (Directory => True, others => False), Walk'Access );
   end Walk;
  
      directory_entry : Directory_Entry_Type;
      FILES_FILTER    : constant Filter_Type := (Ordinary_File => True, others => True );
      mtime           : Time;
      file            : File_Type;
      search          : Search_Type;
      n               : Natural := 0;
      users_work_directory  : constant String := TS( Model.Web_Constants.OSCR_Paths.work_dir & 
                           Censor_String( "graham_s" ) & DIR_SEPARATOR );
    begin
      Start_Search( 
         search, 
         users_work_directory, 
         Model.Web_Constants.REGIME_FILENAME, 
         FILES_FILTER );
      n := 0;
      while More_Entries( search ) loop
         n := n + 1;
         Get_Next_Entry( search, directory_entry );
         mtime       := Modification_Time( directory_entry );
         Put_Line( "entry " & Natural'Image(n) & " = " & Full_Name( directory_entry ) );
      end loop;
      Walk( users_work_directory, Model.Web_Constants.REGIME_FILENAME );
   end Test_Search_Directories;
   
   
   procedure Test_Charge_Buffers( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.Charging.Buffer;
   use Model.Charging;
      
      childcare : Model.Charging.Charging_Regime := Charging_Examples.Construct_Kindergarden;
      childcare_buff : constant Model.Charging.Buffer.Charging_Regime_Buffer := Map_From( childcare );
      application_buff : constant Application_Buffer_Type := Application_Buffer_Package.Element( childcare_buff.applications, 1 );
      target_buff : constant Target_Buffer_Type := Targets_Buffer_Package.Element( application_buff.targets, 1 );
      buff : Charging_Regime_Buffer;
      regime1 : Charging_Regime := Charging_Examples.Construct_Kindergarden;
      regime2 : Charging_Regime;
      errs : Natural;
      charges_ajax : Unbounded_String := Make_Ajax_Call_Charge( insert_below, TuS( "app1" ), TuS( "tar1" ), TuS( "car1" ) );
      targets_ajax : Unbounded_String := Make_Ajax_Call_Target( insert_above, TuS( "app1" ), TuS( "tar1" ) );
      applications_ajax : Unbounded_String := Make_Ajax_Call_Application( insert_above, TuS( "regime22" ), TuS( "app1" ) );
      f : File_Type;
      html : Unbounded_String;
      charge_buff : Charges_Buffer_Type;
   begin
      Create( f, out_file, "regime1.txt" );
      Model.Charging.IO.Write( f, regime1 );
      Close( f );
      buff := Map_From( regime1 );
      errs := Error_Count( buff );
      Assert( errs = 0, " should have zero errors " & errs'Img );
      Put_Line( "charges_ajax = " & TS( charges_ajax ));
      Put_Line( "targets_ajax = " & TS( targets_ajax ));
      Put_Line( "applications_ajax = " & TS( applications_ajax ));
      regime2 := Map_To( buff );
      Create( f, out_file, "regime2.txt" );
      Model.Charging.IO.Write( f, regime2 );
      Close( f );
      charge_buff.id := TuS( Utils.Random_String );
      charge_buff.charge_amount.buffer := TuS("12345.0");
      charge_buff.charge_amount.error := Utils.Out_Of_Range_Error;
      charge_buff.charge_amount.error_message := TuS("Too Small");
      
      
      html := Charge_To_HTML( TuS( "app1" ), TuS( "tar1" ), charge_buff, 2 );
      html := Target_To_HTML( TuS( "app1" ), target_buff, 3 );
      html := Application_To_HTML( TuS( "XXX"), application_buff, 3 );
      Put_Line( " target html = " & ts( html )); 
   end Test_Charge_Buffers;
   
   procedure Test_Run_Program( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Gnat.OS_Lib;
      args : Argument_List( 1 .. 3 );
      java : constant String := "/home/graham_s/VirtualWorlds/projects/oscr/model/graphics/javachart/oscr/scripts/run_svg_charts.sh";
      outfile : constant String := "/home/graham_s/tmp/java.dump";
      success : Boolean := False;
      return_code : Integer;
   begin
      args(1) := new String'("/var/www/oscr/regime_golf_application_everyone_8_/"  );  
      args(2) := new String'("1");
      args(3) := new String'("/var/www/oscr/regime_golf_application_everyone_8_/oscr_charts_driver.txt");
      Put_Line( "args created" );
      Spawn( java, args, outfile, success, return_code );
      Put_Line( "spawned" );
      
      assert( success, "success was " &  success'Img );
      assert( return_code = 0, "return code was " &  return_code'Img );
      Free( args(1) );
      Free( args(2) );
      Free( args(3) );
      Put_Line( "args destroyed" );
      
   end Test_Run_Program;
   
   procedure Test_Make_Directory_Path( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin   
      Utils.Make_Directory_Path( "/home/graham_s/tmp/1/2/3/4" );
      Put_Line( "u made /home/graham_s/tmp/1/2/3/4 #1" );
      Utils.Make_Directory_Path( "/home/graham_s/tmp/1/2/3/4" );
      Put_Line( "u made /home/graham_s/tmp/1/2/3/4 #2" );
      Utils.Make_Directory_Path( "/home/graham_s/tmp/1/2/3/4" );
      Put_Line( "u made /home/graham_s/tmp/1/2/3/4 #3" );   
      Ada.Directories.Create_Path( "/home/graham_s/tmp/1/2/3/4" );
      Put_Line( "a made /home/graham_s/tmp/1/2/3/4 #1" );   
      Ada.Directories.Create_Path( "/home/graham_s/tmp/1/2/3/4" );
      Put_Line( "a made /home/graham_s/tmp/1/2/3/4 #2" );   
      Ada.Directories.Create_Path( "/home/graham_s/tmp/1/2/3/4" );
      Put_Line( "a made /home/graham_s/tmp/1/2/3/4 #2" );         
   end Test_Make_Directory_Path;
   
   procedure Test_Integers( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      i : Integer;
   begin
      Put_Line( "Integer'Last = " & Integer'Last'Img );
      Put_Line( "Integer'Size = " & i'Size'Img );
   end Test_Integers;
   
   procedure Test_Create_Template( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use ubstr;
      use Templates_Parser;
      use type Templates_Parser.Vector_Tag;
      sm : Summary_Info;
      s1 : Unbounded_String;
      s2 : Unbounded_String;
      translations :  OSCR_Translate_Table;
      blocks : Templates_Parser.Vector_Tag;
      
    begin
   
      -- s2 := Make_Single_Table( "All of Scotland", "run_250", sm ); 
       -- 
       -- translations( 1 ) :=
         -- Templates_Parser.Assoc ("UNAFFORDABLE-BASE", "45%" );
       -- translations( 2 ) :=
         -- Templates_Parser.Assoc ("ABOVE_POVERTY", "66%" );
       -- translations( 3 ) :=
         -- Templates_Parser.Assoc ( "ALL-SCOTLAND", To_String( s2 ) );
       
      s1 := Templates_Parser.Parse( TS( Model.Web_Constants.OSCR_Paths.template_components_path) & "output" & ".thtml", translations );
      Put_Line( To_String( s1 ) );
   end Test_Create_Template;
   
   procedure Test_Bounds( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      type TT is array(1..3) of Integer;
      A : TT := (1,2,3);
      i : integer := 3;
   begin
      i := i + 2;
      put_line( A(i)'Img ); --- check for bounds overflow
   end Test_Bounds;
   
   procedure Test_Serialise_Run_Settings( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.Run_Settings;
      icr1 : Incomes_Control_Record;
      icr2 : Incomes_Control_Record;
      f : File_Type;
   begin
      Create( f, out_file, "tmp/runsettings.txt" );
      icr1.aggregate_incomes_to := Model.Household.household_level;
      Model.Run_Settings.IO.Write( f, icr1 );
      Close( f );
      Open( f, in_file, "tmp/runsettings.txt" );
      icr2 := Model.Run_Settings.IO.Read( f );
      Close( f );
      Assert( icr1 = icr2, " Test_Serialise_Run_Settings; read record <> written record " );
   end Test_Serialise_Run_Settings;
   
   procedure Test_Serialise_Charging_Regime( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.Charging;
      childcare   : Charging_Regime := Charging_Examples.Construct_Kindergarden;
      childcare2  : Charging_Regime;
      all_charges : Charging_Regime_List := Model.Main_Examples.Make_All_Regimes;
      f : File_Type;
      charge1, charge2 : Charges_Type;
   begin
      Create( f, out_file, "tmp/childcare.txt" );
      Model.Charging.IO.Write( f, childcare );
      close( f );
      Open( f, in_file, "tmp/childcare.txt" );
      childcare2 := Model.Charging.IO.Read( f );
      close( f );
      Create( f, out_file, "tmp/childcare2.txt" );
      Model.Charging.IO.Write( f, childcare2 );
      close( f );
      if( charge1 /= charge2 ) then
         put_line( "oops" );
      end if;      
   end Test_Serialise_Charging_Regime;
   
   package Incomes_By_Employment_Aggregation_Package is new Costs_Tabulator( 
      Data_Type => Amount, 
      Breakdown_Range => FRS_Enums.ILO_Employment_Status, 
      Values_Range => Model.Incomes.Income_Items,
      Values_Array => Model.Incomes.Incomes_Array );
   package Housing_Costs_Aggregation_Package is new Costs_Tabulator(
      Data_Type => Amount, 
      Breakdown_Range => FRS_Enums.Tenure_Type, 
      Values_Range => mhh.Housing_Costs_Elements,
      Values_Array => mhh.Housing_Costs_Elements_Array );
   
      
      
   type Gross_Type is ( Grossed, Ungrossed );
   
   type Age_Agg_Table is array( Gross_Type, mhh.Aggregation_Level, FRS_Enums.Census_Age_Group ) of Amount;
   type Tenure_Agg_Table is array( Gross_Type, mhh.Aggregation_Level, FRS_Enums.Tenure_Type ) of Amount;
   type Household_Sum is array( Gross_Type, mhh.Aggregation_Level ) of Amount;


   procedure Test_Make_Progress_Tables( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.Run_Settings;
      control : Settings_Rec;
      prog : State_Type;
      table : Unbounded_String;
   begin
      prog.household := 1000;
      prog.year := 2004;
      for p in Phase_Type loop
         prog.phase := p;
         table := Model.Web_Commons.Make_Progress_Table( prog, control );
         put_line( "progress for 1000 households and phase " & p'Img & " = " & TS(table) );
      end loop;
   end Test_Make_Progress_Tables;
   
   procedure Test_Complete_HHlds(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Ada.Text_IO;
      use Data_Constants;
      use FRS_Enums;
      use mhh;
      use UK_Format_Utils;
      
      hh_file    : hh_io.File_Type;   
      settings   : rs.Settings_Rec;
      hh         : mhh.Household_Rec;
      params     : pars.Parameters := pars.Get_2007_8_System; 
      results    : outp.Household_Result;
      regime     : constant Model.Charging.Charging_Regime := Private_School_Examples.Construct_Edinburgh_B_49;
      costs      : Amount_List;
      cost       : Amount;
   begin
      logger.Add_Target( Model.charging_model );
      hh := mhh.Get_One_Model_Household(
         Model.Web_Constants.OSCR_Paths.datafile_directory,
         22291, 2003, True, True );
      for buno in 1 .. hh.num_benefit_units loop
         costs := Model.Charging.Get_Charge_For_Family( hh.benefit_units(buno), regime );
         cost := Amount_Package.Element( costs, 1 );
         if( cost > 0.0 ) and 
           ( hh.benefit_units(buno).bu_type = single_without_children ) and 
           ( hh.benefit_units(buno).adults( head ).age > 18 ) then
            Put_Line( mhh.To_String( hh.benefit_units(buno) ) );
            Put_line( "cost was " & Format(cost) );
         end if;
      end loop;
      logger.Remove_Target( Model.charging_model );
   end Test_Complete_HHlds;
   
   procedure Test_FRS_Totals(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Ada.Text_IO;
      use Data_Constants;
      use FRS_Enums;
      use mhh;
      use UK_Format_Utils;
      
      popn_total : Household_Sum := ( others => (others=>0.0) );
      age_totals : Age_Agg_Table := ( Others=>(others=>(others=>0.0)));
      ten_total  : Tenure_Agg_Table := ( Others=>(others=>(others=>0.0)));
      grossing_factor : Amount := 0.0;
      hh_file    : hh_io.File_Type;   
      start_hh   : Natural := 0;
      end_hh     : Natural := 0;
      settings   : rs.Settings_Rec;
      num_years  : constant Rate := Rate(settings.end_year - settings.start_year + 1);
      hh         : mhh.Household_Rec;
      num_hhlds  : Positive;
      age_group  : Census_Age_Group;
      
      hcosts     : Housing_Costs_Aggregation_Package.Table_Type;
      incomes    : Incomes_By_Employment_Aggregation_Package.Table_Type;
      households_read : Natural := 0;
   begin
      Years:
      for year in  settings.start_year .. settings.end_year loop
         Initialise ( Model.Web_Constants.OSCR_Paths.datafile_directory, 
            hh_file, year, num_hhlds, False );
         if settings.SCOTLAND_ONLY then
            start_hh := soc.FILE_POSITIONS( year, soc.start_pos );
            end_hh   := soc.FILE_POSITIONS( year, soc.end_pos );
         else
            start_hh := 1;
            end_hh   := num_hhlds;
         end if;
         Households:
         for href in  start_hh .. end_hh loop
            hh := Load( hh_file, href );
            if (settings.SCOTLAND_ONLY)  and then ( hh.standard_region /= Scotland ) then
               put_line( "scottish household in middle of scottish bock " & Format(hh.sernum) & " hhref " & Format( href ));
            else
               put_line( "on houshold " & href'Img & " year " & year'Img );
               Annualise( hh );
               Uprate_Household( hh );
               households_read := households_read + 1; 
               Put_Line( "year " & year'Img & " on household " & Format(hh.sernum) & " hhref " & href'Img );
               grossing_factor := hh.grossing_factor / num_years;
               
               Housing_Costs_Aggregation_Package.Add_Observation( 
                  hcosts, hh.tenure, grossing_factor, 
                  Map_From_Housing_Record_To_Array( hh.housing_costs ));
               
               declare
                  bu : mhh.Model_Benefit_Unit := hh.benefit_units( 1 );
               begin
                  age_group := Map_From_Age( bu.adults( head ).age ); 
                              
                  age_totals( Grossed, mhh.household_level ,age_group ) :=
                     age_totals( Grossed, mhh.household_level ,age_group ) + grossing_factor;
                  age_totals( Ungrossed, mhh.household_level ,age_group ) :=
                     age_totals( Ungrossed, mhh.household_level ,age_group ) + 1.0;
                  popn_total( Grossed, mhh.household_level ) :=
                     popn_total( Grossed, mhh.household_level ) + grossing_factor;
                  popn_total( Ungrossed, mhh.household_level ) :=
                     popn_total( Ungrossed, mhh.household_level ) + 1.0;
               end;
               Benefit_Units:
               for buno in 1 .. hh.num_benefit_units loop
                  declare
                     bu : mhh.Model_Benefit_Unit := hh.benefit_units( buno ); 
                  begin
                     age_group := Map_From_Age( bu.adults( head ).age );
                     age_totals( Grossed, mhh.benefit_unit_level, age_group ) :=
                        age_totals( Grossed, mhh.benefit_unit_level, age_group ) + grossing_factor;
                     age_totals( Ungrossed, mhh.benefit_unit_level, age_group ) :=
                        age_totals( Ungrossed, mhh.benefit_unit_level, age_group ) + 1.0;
                     popn_total( Grossed, mhh.benefit_unit_level ) :=
                        popn_total( Grossed, mhh.benefit_unit_level ) + grossing_factor;
                     popn_total( Ungrossed, mhh.benefit_unit_level ) :=
                        popn_total( Ungrossed, mhh.benefit_unit_level ) + 1.0;
                     Adults:
                     for adno in head .. hh.benefit_units( buno ).last_adult loop
                        age_group := Map_From_Age( bu.adults( adno ).age ); 
                        age_totals( Grossed, mhh.individual_level, age_group ) :=
                           age_totals( Grossed, mhh.individual_level, age_group ) + grossing_factor;
                        age_totals( Ungrossed, mhh.individual_level, age_group ) :=
                           age_totals( Ungrossed, mhh.individual_level, age_group ) + 1.0;
                        popn_total( Grossed, mhh.individual_level ) :=
                           popn_total( Grossed, mhh.individual_level ) + grossing_factor;
                        popn_total( Ungrossed, mhh.individual_level ) :=
                           popn_total( Ungrossed, mhh.individual_level ) + 1.0;
                           
                        Incomes_By_Employment_Aggregation_Package.Add_Observation( 
                           incomes, bu.adults( adno ).ilo_employment, grossing_factor, 
                           bu.adults( adno ).incomes );
                        if( households_read < 20 ) then
                           for i in bu.adults( adno ).incomes'Range loop
                              put_line( i'Img & bu.adults( adno ).incomes(i)'Img );
                           end loop;
                        end if;
   -- type Tenure_Agg_Table is array( hh.Aggregation_Level, FRS_Enums.Tenure, Gross_Type ) of Amount;
   -- type Household_Sum is array( hh.Aggregation_Level ) of Amount;
                        
                     end loop Adults;
                     Children:
                     for chno in 1 .. bu.num_children loop
                        age_group := Map_From_Age( bu.children( chno ).age ); 
                        age_totals( Grossed, mhh.individual_level, age_group ) :=
                           age_totals( Grossed, mhh.individual_level, age_group ) + grossing_factor;
                        age_totals( Ungrossed, mhh.individual_level, age_group ) :=
                           age_totals( Ungrossed, mhh.individual_level, age_group ) + 1.0;
                        popn_total( Grossed, mhh.individual_level ) :=
                           popn_total( Grossed, mhh.individual_level ) + grossing_factor;
                        popn_total( Ungrossed, mhh.individual_level ) :=
                           popn_total( Ungrossed, mhh.individual_level ) + 1.0;
                     end loop Children;
                  end;
               end loop Benefit_Units;
            end if;
         end loop Households;
         hh_io.Close( hh_file );
      end loop Years;
      --      
      --  main output
      --
      put_line( "Age Distribution" );
      for g in Gross_Type loop
         put_line( "Gross Type " & Gross_Type'Image( g ) & " =========== " );
         for ag in mhh.Aggregation_Level loop
            put_line( "aggregation level " & mhh.Aggregation_Level'Image( ag ) & " =========== " );
            for age in Census_Age_Group loop
               put_line( Pretty_Print( age ) & "  :  " & Format_With_Commas( age_totals( g, ag, age )));
            end loop;
         end loop;
      end loop;
      new_line;
      put_line( "Population Totals" );
      for g in Gross_Type loop
         put_line( "Gross Type " & Gross_Type'Image( g ) & " =========== " );
         for ag in mhh.Aggregation_Level loop
            put_line( Format_With_Commas( popn_total( g, ag ) ));
         end loop;
         new_line;
      end loop;
      new_line;
      put_line( 
         Housing_Costs_Aggregation_Package.To_Delimited( hcosts ) );
      new_line;
      put_line( 
         Incomes_By_Employment_Aggregation_Package.To_Delimited( incomes ) );
      new_line;
   end Test_FRS_Totals;

   
   procedure Test_Aggregation( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   use FRS_Enums;
   use incmt;
   use UK_Format_Utils;
      v1 : One_Complete_Income_Measure_Output;
      v2 : One_Complete_Income_Measure_Output;
   begin
      v1.poverty_line := 10.0;
      v2.poverty_line := 12.0;
      v1.region := fife_central_lothian;
      v1.num_cost_measures := 2;
      v1.costs(1) := 100.0;
      v1.costs(2) := 100.0;
      v2.costs(1) := 1.0;
      v2.costs(2) := 2.0;
      v1.incomes( gross ) := 10.0;
      v2.incomes( gross ) := 1.0;
      v1.incomes( net ) := 100.0;
      v2.incomes( net ) := 10.0;
      v1.incomes( disposable ) := 1000.0;
      v2.incomes( disposable ) := 100.0;
      incmt.Aggregate( v1, v2 );
      Assert( v1.incomes( disposable ) = 1100.0, " v1.incomes( disposable ) should be 1,100 was " & format(v1.incomes( disposable )));
      Assert( v1.incomes( net ) = 110.0, " v1.incomes( net ) should be 110 was " & format(v1.incomes( net )));
      Assert( v1.incomes( gross ) = 11.0, " v1.incomes( gross ) should be 11 was " & format(v1.incomes( gross )));
      Assert( v1.poverty_line = 22.0, " v1.poverty_line should be 22 was " & format(v1.poverty_line));
      Assert( v1.costs(1) = 101.0, " v1.costs(1) should be 200 was " & format(v1.costs(1)));
      Assert( v1.costs(2) = 102.0, " v1.costs(2) should be 3 was " & format(v1.costs(2)));
      Assert( v1.region = fife_central_lothian, "Should be fife_central_lothian was " &  v1.region'Img );
   end Test_Aggregation;
  
   procedure Test_OSCR_OutputGenerator( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      regimes  : Model.Charging.Charging_Regime_List :=  Model.Main_Examples.Make_All_Regimes;
      regime   : Model.Charging.Charging_Regime;
      control  : rs.Settings_Rec;
      tables   : moo.Table_Set;
      tablestr : Unbounded_String;
      rc       : Integer;
   begin
      control.save_file_name := ubstr.To_Unbounded_String( "oscr_detailed.csv" );
      regime := rp.Element( regimes, 2 );
      tables := moog.Generate_Cost_Measures( 
         Model.Web_Constants.OSCR_Paths.datafile_directory,
         TuS( "TEST" ),
         regime, 
         control, 
         Model.Web_Commons.Update_Run_State'Access );
      moof.Make_HTML_And_Charts(
         "/oscr",
         regime, 
         TuS("/home/graham_s/VirtualWorlds/projects/oscr/model/tmp/"), 
         tables, 
         control, 
         TuS("Username Goes Here") );
      rc := mwc.Run_Javacharts( 
         script => "/home/graham_s/VirtualWorlds/projects/oscr/model/graphics/javachart/oscr/scripts/charts_generator_unix.sh",
         run_directory => "/home/graham_s/VirtualWorlds/projects/oscr/model/tmp/", 
         logfile => "logs/test_javachar_runner.log",
         use_svg_graphics => True );
      Assert( rc = 0, "return code should be " & rc'Img );
   end Test_OSCR_OutputGenerator;
   
   task type Simple_Task_Type is
      entry Say_Hello;
   end Simple_Task_Type;
   
   n : Natural := 0;
   
   task body Simple_Task_Type is
   begin
      loop
         accept Say_Hello do
            Put_Line( "eheheheheheheheheheheh " & n'Img );
            n := n + 1;
            delay 10.0;
         end Say_Hello;
      end loop;
   end Simple_Task_Type;
   
   
   procedure Test_Say_Hello( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      runner : Simple_Task_Type;
   begin
      for i in 1 .. 100 loop
         runner.Say_Hello;
      end loop;
   end Test_Say_Hello;
   
   procedure Test_Session_Ids( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use type AWS.Session.Id;
      session_id : AWS.Session.Id := AWS.Session.Value( "SID-1" );
   begin
      Assert( AWS.Session.Value( AWS.Session.Image( session_id )) = session_id, " not symetric " );
   end Test_Session_Ids;
   
   procedure Run_Task is
      regimes : Model.Charging.Charging_Regime_List := Model.Main_Examples.Make_All_Regimes;
      control :  rs.Settings_Rec;
      regime  : Model.Charging.Charging_Regime;
      jobs_in_queue : Natural;
      session_id : AWS.Session.Id := AWS.Session.Value( "TEST" );
      use Model.Web_Commons;
      dir : Unbounded_String := TuS("/home/graham_s/tmp/");
   begin
      Put_Line( "Run_Task: entered " );
      regime := rp.Element( regimes, 2 );
      job_queue.Set_Max_Jobs_To_Run( 2 );
      job_queue.Enque( session_id, regime, control, dir );
      jobs_in_queue := job_queue.size;
      job_queue.Enque( session_id, regime, control, dir );
      Put_Line( "Run_Task: exiting" );
   end Run_Task;
   
   procedure Test_Make_Task( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin
      Put_Line( "Test_Make_Task: entered " );
      Run_Task;
      Put_Line( "Test_Make_Task: returned " );
   end Test_Make_Task;

   
   procedure Test_Get_Scottish_Households( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.Household;
      use Scotland_Specific_Constants;
      use FRS_Enums;
      Scotland_Start_Stop : File_Positions_Array := 
         (2003 => ( 0, 0 ),
          2004 => ( 0, 0 ),
          2005 => ( 0, 0 ));
      settings   : rs.Settings_Rec;
      hh         : mhh.Household_Rec;
      start_hh   : Natural := 1;
      end_hh     : Natural := 0;
      hh_file    : mhh.hh_io.File_Type;
      on_scotland : Boolean := false;
      
   begin
      Years:
      for year in  settings.start_year .. settings.end_year loop
         mhh.Initialise( 
            Model.Web_Constants.OSCR_Paths.datafile_directory, 
            hh_file, 
            year, 
            end_hh, 
            False );
         on_scotland := False;
         for href in  start_hh .. end_hh loop
            hh := mhh.Load( hh_file, href );
            put_line( " " & href'Img & " region " & hh.standard_region'Img );
            if( hh.standard_region = Scotland ) then
               if( not on_scotland ) then
                  Scotland_Start_Stop( year, start_pos ) := href;
                  on_scotland := True;
               end if;
            end if;
            if( on_scotland ) and ( hh.standard_region /= Scotland ) then
               Scotland_Start_Stop( year, end_pos ) := href;               
               on_scotland := False;
            end if;             
         end loop;
         hh_io.Close( hh_file );
       end loop Years;
       for year in  settings.start_year .. settings.end_year loop
         put_line( year'Img & " => ( " & Scotland_Start_Stop( year, start_pos )'Img & ", " & Scotland_Start_Stop( year, end_pos )'Img & ", " );
       end loop;
   end Test_Get_Scottish_Households;
   
   procedure Test_Targetting( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.Charging;
   use FRS_Enums;
   use Model.Incomes;
   use Model.Household;
   -- use Model.Incomes.Incomes_Package.Set_Ops;
   use Model.Incomes.Broad_Benefits_Package.Set_Ops;
   use Gender_Package.Set_Ops;
      target : Target_Type;
      adult  : Model_Adult;
   begin
      put_line( "Test_Targetting entered " );
      target.min_age := 60;
      target.max_age := 70;
      
      adult.sex := female;
      
      adult.age := 60;
      assert( Hits_Target( adult, target ), " 60 year old should hit " );
      
      adult.age := 20;
      adult.incomes( child_benefit ) := 100.0;
      
      Assert( not Hits_Target( adult, target ), " 20 year old should miss " );
      put_line( "Test_Targetting test#1 passed" );
      Include( target.genders, male );
      
      Assert( not Is_Empty( target.genders ), " genders should have 1 element " );
      Assert( not Contains( target.genders, female ), " shouldn't contain female");
      Assert( Contains( target.genders, male ), " should contain male");
      
      Assert( not Hits_Target( adult, target ), " male target should miss " );
      put_line( "Test_Targetting test#2#3#4 passed" );
      Include( target.genders, female );
      adult.age := 65;
      Assert( Hits_Target( adult, target ), "female target should hit(1) " );
      
      Exclude( target.genders, female );
      Assert( not Hits_Target( adult, target ), "female target should miss(2) " );
      Include( target.genders, female );
      Include( target.benefits, child_benefit );
      Assert( Hits_Target( adult, target ), "female target should hit (child benefit)" );
      put_line( "Test_Targetting tests passed" );

   end Test_Targetting;
   
   
   procedure Test_Charging_Regime( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   
   use Model.Household;
   use Model.Charging;
   use FRS_Enums;
   use Amount_Package;
   
      cost  : Amount;
      costs : Amount_List;
      cost_expensive : Amount;
      cost_cheap : Amount;
      child  : Model_Child;
      bu     : Model_Benefit_Unit;
      target : Target_Type;
      charge : Charges_Type;
      childcare : Model.Charging.Charging_Regime := Charging_Examples.Construct_Kindergarden;
      -- childcare_expensive : Model.Charging.Charging_Regime := Charging_Examples.Construct_Kindergarden_Expensive;
   begin
      put_line( "starting Test_Charging_Regime" );
      put_line( To_String( childcare ) );
      child.age := 5;
      child.sex := female;
      -- a little local target
      target.name := ubstr.To_Unbounded_String("Extended School Day; Care Activities");
      target.min_age := 2;
      target.max_age := 8;
      charge.name := ubstr.To_Unbounded_String("8am-9:15am");
      charge.frequency := 1.0;
      charge.period := school_day;
      charge.charge_amount := 3.50;
      cp.Append( target.charges, charge );
      --
      cost := Charge_Person( child, target );
      Assert( cost = 955.5, "Costs should be 995.50 for 5 yo for 3.50 per day 273 days was" & Amount'Image( cost ) );
      put_line( " Test_Charging_Regime test #1 " );
      child.age := 12;
      cost := Charge_Person( child, target );
      Assert( cost = 0.0, "Costs should be 0 for 12 yo" & Amount'Image( cost ) );
      put_line( " Test_Charging_Regime test #2 " );
      bu.last_adult := head;
      bu.adults( head ).age := 30;
      bu.adults( head ).sex := female;
      bu.num_children := 1;
      bu.children(1).age := 3; -- rosebloom
      bu.children(1).sex := male;
      --  cost should be 900*3 + (21.0 per day * 273.0)  = 8433
      
      costs := Get_Charge_For_Family( bu, childcare );
      cost_expensive := Element( costs, 1 );
      cost_cheap := Element( costs, 2 );
      Assert( cost_expensive = 9273.0, " annual cost should be 9273.0 but was " & Amount'Image( cost_expensive ));
      put_line( " Test_Charging_Regime test #3 " );
      Assert( cost_cheap =  1305.0, " cheap cost should be 435 was " & Amount'Image( cost_cheap ) );
      put_line( " Test_Charging_Regime test #4 " );
      bu.num_children := 2;
      bu.children(2).age := 6; -- rosebloom
      bu.children(2).sex := male;
      costs := Get_Charge_For_Family( bu, childcare );
      cost_expensive := Element( costs, 1 );
      cost_cheap := Element( costs, 2 );
      
      Assert( cost_expensive = 19_362.0," annual cost should be 19_362.0 but was " & Amount'Image( cost_expensive ));
      put_line( " Test_Charging_Regime test #5 " );

      Assert( cost_cheap =  1305.0, " cheap cost should be unchanged at 1305 was " & Amount'Image( cost_cheap ));
      put_line( " Test_Charging_Regime test #6 " );
   end Test_Charging_Regime;
   
   procedure Test_Golf( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   
   use Model.Household;
   use Model.Charging;
   use FRS_Enums;
   use Amount_Package;
   
      child  : Model_Child;
      bu     : Model_Benefit_Unit;
      target : Target_Type;
      charge : Charges_Type;
      golf   : Model.Charging.Charging_Regime := Charging_Examples.Construct_Golf_Club;
      costs  : Amount_List;
      cost_expensive : Amount;
      cost_cheap : Amount;
   begin
      --
      -- golf
      --
      bu.last_adult := head;
      bu.adults( head ).age := 30;
      bu.adults( head ).sex := female;
      put_line( To_String( golf ) );
      bu.num_children := 0;
      costs := Get_Charge_For_Family( bu, golf );
      cost_cheap := Element( costs, 2 );
      cost_expensive := Element( costs, 1 );
      Assert( cost_expensive =  90.0, " golf single adult should be 90 was " & Amount'Image( cost_expensive ));
      put_line( " Test_Charging_Regime test #7 " );
      
      bu.num_children := 2;
      bu.children(1).age := 15; -- junior
      bu.children(1).sex := male;
      bu.children(2).age := 6; -- too young
      bu.children(2).sex := male;
      costs := Get_Charge_For_Family( bu, golf );
      cost_cheap := Element( costs, 2 );
      cost_expensive := Element( costs, 1 );
      
      Assert( cost_expensive = 100.0, " golf single adult + 1 child > 8 child should be 100 was " & Amount'Image( cost_expensive ));
      put_line( " Test_Charging_Regime test #8 " );

      bu.adults( head ).ilo_employment := student;
      costs := Get_Charge_For_Family( bu, golf );
      cost_cheap := Element( costs, 2 );
      cost_expensive := Element( costs, 1 );
      
      Assert( cost_expensive = 70.0, " golf single student adult + 1 child > 8 child should be 70 was " & Amount'Image( cost_expensive ));
      put_line( " Test_Charging_Regime test #9 passed" );

   end Test_Golf;
   
   procedure Test_Charging(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.Charging;
      ct : Charges_Type;
      amt : Amount;
   begin
      ct.name := ubstr.To_Unbounded_String( "t1" );
      ct.charge_amount := 10_000.0;
      ct.interest_rate := 10.0;
      ct.period := annual;
      ct.frequency := 2.0; -- 2 years
      amt := Annualised_Equivalent( ct );
      put_line( " amt " & Amount'Image( amt ) );
      assert( Nearly_Equal(amt, 5537.40 ), " loan over year should be 5537.40 but was " & amt'Img );
      ct.name := ubstr.To_Unbounded_String( "t1" );
      ct.charge_amount := 4.50;
      ct.period := daily;
      ct.frequency := 365.0; -- once a year
      amt := Annualised_Equivalent( ct );
      put_line( " amt (should be 4.50)" & Amount'Image( amt ) );
      assert( amt = 4.50, " loan over year should be 4.50 but was " & amt'Img );
      ct.period := monthly;
      ct.frequency := 3.0; -- once every 3 months
      amt := Annualised_Equivalent( ct );
      put_line( " amt (should be 18)" & Amount'Image( amt ) );
      assert( amt = 18.0, " loan over year should be 18. but was " & amt'Img );
   end Test_Charging;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests( t : in out Test_Case ) is
   begin 
      Register_Routine (T, Test_Serialise_Run_Settings'Access, "Test Serialise Run Settings");
      -- Register_Routine (T, Test_Delete_Tree'Access, "Test Delete Tree" );
      --Register_Routine (T, Test_Search_Directories'Access, "Test_Search");
      Register_Routine (T, Test_Dump_Examples'Access, "Test Dump Examples " );
      Register_Routine (T, Test_Charge_Buffers'Access, "Test_Charge_Buffers");
      -- Register_Routine (T, Test_Make_Directory_Path'Access, "Test_Make_Directory_Path");
      -- Register_Routine (T, Test_Run_Program'Access, "Test Run Program");
      -- Register_Routine (T, Test_Make_Task'Access, "Test Make Task");
      -- Register_Routine (T, Test_Session_Ids'Access, "Test Session Ids " );
      -- Register_Routine (T, Test_Make_Progress_Tables'Access, "Test Make Tables");
      Register_Routine (T, Test_OSCR_OutputGenerator'Access, "Test_OSCR_OutputGenerator");
      Register_Routine (T, Test_Integers'Access, "Test_Integer");
      -- Register_Routine (T, Test_Say_Hello'Access, "Test Say Hello Task");
      Register_Routine (T, Test_Serialise_Charging_Regime'Access, "Test Serialise");
      -- Register_Routine (T, Test_Bounds'Access, "Test Bounds");
      -- Register_Routine( T, Test_Create_Template'Access, "Test Create_Template");
      Register_Routine( T, Test_Aggregation'Access, "Test Aggregation");
      Register_Routine( T, Test_Charging_Regime'Access, "Test Charging Regime");
      Register_Routine( T, Test_Charging'Access, "Test Charging");
      Register_Routine( T, Test_Targetting'Access, "Test Targetting");
      Register_Routine( T, Test_Golf'Access, "Test Golf");
      Register_Routine( T, Test_Get_Scottish_Households'Access, "Test Scottish");
      Register_Routine( T, Test_FRS_Totals'Access, "Test FRS Totals");
      -- Register_Routine( T, Test_OSCR_Output.Generator'Access, "Test_OSCR_Output.Generator" );
      Register_Routine( T, Test_Complete_HHlds'Access, "Test Complete HHLDS" );
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   
   begin
      null;
      -- logger.Add_Target( Model.runner );
      -- logger.Add_Target( Model.legal_aid_calcs );
      -- logger.Add_Target( Model.income_calcs );
      -- logger.Add_Target( Model.Model.Household );
      -- logger.Add_Target( Model.model_uprate );
   end Set_Up;

   ----------
   -- Name --
   ----------
   function Name ( T : Test_Case ) return Message_String is
   begin
      return Format( " OSCR Tests." );
   end Name;

end OSCR_Tests;
