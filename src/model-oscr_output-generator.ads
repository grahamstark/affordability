with Ada.Strings.Unbounded;
with Model.Charging;
with Model.Run_Settings;

package Model.OSCR_Output.Generator is

   use Ada.Strings.Unbounded;
   
   function Generate_Cost_Measures( 
      data_path : Unbounded_String;
      run_id    : Unbounded_String;
      regime    : Model.Charging.Charging_Regime; 
      control   : Model.Run_Settings.Settings_Rec;
      observer  : Model.Run_Settings.Run_Observer_Access ) return Table_Set;

end Model.OSCR_Output.Generator;
