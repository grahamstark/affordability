with Model.Charging;

package Model.Main_Examples is

      CHARGES : constant Amount_Array := (
         1_500.0,
         3_500.0,
         5_000.0,
         7_500.0,
         9_000.0,
         12_000.0,
         15_000.0,
         18_000.0,
         21_000.0,
         24_000.0,
         27_000.0 );


   function Make_All_Regimes return Model.Charging.Charging_Regime_List;
   
end Model.Main_Examples;
