with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Data_Constants;
with FRS_Enums;
with Scotland_Specific_Constants;
with Utils;

with Model.Charging;
with Model.Household; 
with Model.Income_Measures;
with Model.Income_Measure_Types;
with Model.Income_Measures.Breakdown_Package;
with Model.Output.Complete;
with Model.Run_Settings;


package Model.OSCR_Output is

   use Ada.Strings.Unbounded;
   
   package mimt  renames Model.Income_Measure_Types;
   package mim renames Model.Income_Measures;

   package Breakdown_By_Tenure is new mim.Breakdown_Package( Breakdown_Var => FRS_Enums.Tenure_Type, Get_Breakdown_Description => FRS_Enums.Pretty_Print ); 
   package Breakdown_By_Region is new mim.Breakdown_Package( Breakdown_Var => Scotland_Specific_Constants.Scottish_Regional_Stratifier, Get_Breakdown_Description => FRS_Enums.Pretty_Print );  
   package Breakdown_By_Economic_status is new mim.Breakdown_Package( Breakdown_Var => FRS_Enums.Benefit_Unit_Economic_Status, Get_Breakdown_Description => FRS_Enums.Pretty_Print ); 
   package Breakdown_By_Disablement_status is new mim.Breakdown_Package( Breakdown_Var => FRS_Enums.BU_Disabled_Indicator, Get_Breakdown_Description => FRS_Enums.Pretty_Print ); 
   package Breakdown_By_Bu_type is new mim.Breakdown_Package( Breakdown_Var => FRS_Enums.HBAI_Benefit_Unit_Type, Get_Breakdown_Description => FRS_Enums.Pretty_Print ); 
   package Breakdown_By_Age_Range_Of_Head is new mim.Breakdown_Package( Breakdown_Var => FRS_Enums.Age_Group, Get_Breakdown_Description => FRS_Enums.Pretty_Print );

   type Table_Set is record
      tenure_tab : Breakdown_By_Tenure.Table_Rec;
      region_tab : Breakdown_By_Region.Table_Rec;
      economic_status_tab : Breakdown_By_Economic_status.Table_Rec;
      disablement_status_tab : Breakdown_By_Disablement_status.Table_Rec;
      bu_type_tab : Breakdown_By_Bu_type.Table_Rec;
      age_range_tab : Breakdown_By_Age_Range_Of_Head.Table_Rec;
   end record;
   
   BLANK_TABLE_SET : constant Table_Set := (
      tenure_tab => Breakdown_By_Tenure.EMPTY_TABLE,
      region_tab => Breakdown_By_Region.EMPTY_TABLE,
      economic_status_tab => Breakdown_By_Economic_status.EMPTY_TABLE,
      disablement_status_tab => Breakdown_By_Disablement_status.EMPTY_TABLE,
      bu_type_tab => Breakdown_By_Bu_type.EMPTY_TABLE,
      age_range_tab => Breakdown_By_Age_Range_Of_Head.EMPTY_TABLE
   );
   
   --
   -- ?? no longer used ??
   --
   package Table_Set_Package is new Ada.Containers.Vectors( Element_Type => Table_Set, Index_Type => Positive );
   subtype Table_Set_List is Table_Set_Package.Vector;
   package ts renames Table_Set_Package;
   
   type Incomes_Table is array( mimt.Income_Brackets,  mimt.Income_Measure_Type ) of Counter_Type;
   
   type Aggregate_Cell_Breakdown is record
      v1 : Counter_Type := 0.0;
      v2 : Counter_Type := 0.0;
   end record;
   
   type Example is record
      hhref : Positive;
      year  : Data_Constants.Data_Years;      
   end record;


end Model.OSCR_Output;
