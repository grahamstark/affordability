--
--  $Author: graham_s $
--  $Date: 2009-06-18 09:57:13 +0100 (Thu, 18 Jun 2009) $
--  $Revision: 7463 $
--
--
-- 

pragma License( Modified_GPL );

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with FRS_Enums;
with Model.Incomes;
with Model.Household;
with Model.Parameters.Complete;
with Model.Output.Complete;
with Model.Run_Settings;
with T_Utils;
with Data_Constants;


generic

   type Breakdown_Var is (<>);
   with function Get_Breakdown_Description( bd : Breakdown_Var ) return String;
   
package Model.Income_Measures.Breakdown_Package is

   use Ada.Strings.Unbounded;

   type Example_Type is record
      hhref : Positive;
      year  : Data_Constants.Data_Years;
      buno  : Positive;
   end record;
   --
   -- not used yet but should be...
   --
   package Examples_List_Package is new Ada.Containers.Vectors( Element_Type => Example_Type, Index_Type => Positive );
   subtype Examples_List is Examples_List_Package.Vector;
   
   type Cell_Rec is
      record      
         count     : Amount := 0.0;
         examples  : Examples_List; 
       end record;

   type Cell_Array is array( Breakdown_Var ) of Basic_Cell;
   type Col_Totals_Array is  array( Breakdown_Var ) of Col_Totals_Type;
   type Table_Rec is record
      cells  : Cell_Array := (others=>(others=>(others=>(others=>0.0))));
      row_totals : Basic_Cell := ( others=>(others=>(others=>0.0)));
      col_totals : Col_Totals_Array := ( others=>(others=>(others=>0.0)));
      overall_totals : Col_Totals_Type := ( others=>(others=>0.0));
   end record;
   
   EMPTY_TABLE : constant Table_Rec := (
      cells  => (others=>(others=>(others=>(others=>0.0)))),
      row_totals => ( others=>(others=>(others=>0.0))),
      col_totals => ( others=>(others=>(others=>0.0))),
      overall_totals => ( others=>(others=>0.0)) );
   
   function To_Percent( 
      table : Table_Rec; 
      num_cost_measures : Costs_Range;
      run_settings    : settings.Settings_Rec ) return Table_Rec;
      
   type Graph_Data_Array is array( Affordability_Measure_Type ) of Basic_Breakdown;           
   type Affordability_Summary_Array is array ( Affordability_Measure_Type ) of Amount;
   
   type One_Summary_Info_Rec is record
      unaffordable :   Affordability_Summary_Array := ( others => 0.0 );
      population   : Amount := 0.0;
      graph_data   : Graph_Data_Array := ( others=>( others=>0.0));
   end record;
   
   type Chart_Types is ( bar, pie, radar ); 
   
   type Summary_Info_Array is array( Breakdown_Var ) of One_Summary_Info_Rec;
   
   type Summary_Info_Rec is record
      totals : One_Summary_Info_Rec;
      sm : Summary_Info_Array;
   end record;
   
   function Make_Summary_Data( table : Table_Rec; which_charge : Costs_Range; which_totals_slot : Slot_Range ) return Summary_Info_Rec;
   
   
   
   function Get_Percentage_Below_Poverty_Line( 
      table : Table_Rec; 
      which_charge :Costs_Range;
      which_breakdown : Breakdown_Var := Breakdown_Var'First;
      is_total : Boolean := True ) return Amount;
      
   function Get_Baseline_Unaffordable( 
      table : Table_Rec;  
      which_charge :Costs_Range; 
      measure : Affordability_Measure_Type;
      which_totals_slot : Slot_Range;       
      which_breakdown    : Breakdown_Var := Breakdown_Var'First;
      is_total : Boolean := True ) return Amount; 

   procedure Increment( breakdown : Breakdown_Var; 
                          table : in out Table_Rec; 
                          output : One_Complete_Income_Measure_Output;
                          run_settings    : settings.Settings_Rec );
                          
   function To_CDA( table : Table_Rec; num_cost_measures : Costs_Range;
                    run_settings : settings.Settings_Rec;
                    to_percent   : Boolean := false ) return String;

   function Make_Single_Table(    
                    root             : String;
                    regime_text      : String;
                    application_text : String;
                    regime_id        : String;
                    application_id   : String;
                    header           : String; 
                    id               : String; 
                    summary_infos    : One_Summary_Info_Rec; 
                    breakdown        : Breakdown_Var;
                    is_total         : Boolean ) return Ada.Strings.Unbounded.Unbounded_String; -- FIXME why not just String??
                    
   function To_HTML(
                    root : String;
                    regime_text : String;
                    application_text : String;
                    regime_id : String;
                    application_id : String;
                    sinfo : Summary_Info_Rec;
                    section_title : String; 
                    id : String;
                    to_percent   : Boolean := false ) return Unbounded_String;
                    
   function Create_One_Chart_Data(
      filename : String; 
      title : String;
      subtitle : String;
      data  : Basic_Breakdown;
      measure : Affordability_Measure_Type; -- residual_income_level, cost_disposable_ratio, cost_gross_ratio
      ctype : Chart_Types;
      is_thumbnail : Boolean ) return String;
      
   function Write_Chart_Block( 
      directory  : String;
      block_name : String; 
      sinfo : Summary_Info_Rec ) return String;
      
   function Make_Docbook_Section(
      chart_directory     : String;
      section_header : String;
      summary_info : One_Summary_Info_Rec;
      html_output_dir : String ) return Ada.Strings.Unbounded.Unbounded_String;
      
   function Make_RR_Row( cost : Amount; data  : Basic_Breakdown ) return String;
   function Make_RR_Row( measure : Affordability_Measure_Type ) return String;
      
end Model.Income_Measures.Breakdown_Package;
