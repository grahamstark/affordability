with Model.Charging;
with Ada.Text_IO;
with Model.Run_Settings;
with Model.Income_Measures;
with Model.Output.Complete;
with Model.Household; 
with Data_Constants;
with Ada.Strings.Unbounded;
with Utils;

package Model.OSCR_Statistics_Generator is

   use Ada.Strings.Unbounded;

   procedure Generate_Income_Measures( control : Model.Run_Settings.Settings_Rec );
   
   procedure Generate_Cost_Measures( 
      regimes  : Model.Charging.Charging_Regime_List; 
      control  : Model.Run_Settings.Settings_Rec;
      observer : Model.Run_Settings.Run_Observer_Access );
      
   function BU_Complete_To_String( 
      bu      : Model.Household.Model_Benefit_Unit; 
      tbpos   : Model.Output.Complete.Benefit_Unit_Result;
      incomes : Model.Income_Measures.One_Complete_Income_Measure_Output ) return String;

      
      


end Model.OSCR_Statistics_Generator;
