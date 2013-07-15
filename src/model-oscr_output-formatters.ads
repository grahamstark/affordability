with Ada.Strings.Unbounded;
with Model.Run_Settings;
with Model.Charging;
with Model.Income_Measures;

package Model.OSCR_Output.Formatters is

   use Ada.Strings.Unbounded;
   package rs    renames Model.Run_Settings;
   package mimt   renames Model.Income_Measure_Types;  

   procedure Make_HTML_And_Charts(
      root   : String;
      regime : Model.Charging.Charging_Regime;
      output_directory : Unbounded_String;
      tables : Table_Set;
      settings : rs.Settings_Rec;
      user_title : Unbounded_String );
   --
   -- testing only
   --
   function Make_HTML(
      root           : String;
      regime         : Model.Charging.Charging_Regime;
      application_no : mimt.Costs_Range; 
      relative_dir   : String;
      tables         : Table_Set;
      settings       : rs.Settings_Rec;
      user_title     : Unbounded_String ) return Unbounded_String;
    
end Model.OSCR_Output.Formatters;
