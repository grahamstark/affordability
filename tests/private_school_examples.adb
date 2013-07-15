with Ada.Strings.Unbounded;

with FRS_Enums;
with Model.Incomes;
with Model.Household;

package body Private_School_Examples is

   use Model.Charging;
   use FRS_Enums;
   use Model.Incomes;
   use Model.Incomes.Incomes_Package.Set_Ops;
   use Model.Household;
   use Ada.Strings.Unbounded;
   
   package ap renames Model.Charging.Application_Package;
   package tp renames Model.Charging.Target_Package;
   package cp renames Model.Charging.Charges_Package;
   package rp renames Model.Charging.Charging_Regime_Package;
   package ep renames Model.Household.Employment_Package.Set_Ops;
   package sp renames FRS_Enums.Gender_Package.Set_Ops;


   --
   -- Edinburgh_1
   --

   function Construct_Edinburgh_1_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "1_Edinburgh : Co-ed 5-12 Yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 5 - 12 - high cost" );
      target.min_age := 5;
      target.max_age := 12;
      target.isExclusive := True;      
            -- Edinburgh : ages 5 - 12 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 5 - 12 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3600.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_1_High_Cost;


   function Construct_Edinburgh_1_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "1_Edinburgh : Co-ed 5-12 Yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 5 - 12 - low cost" );
      target.min_age := 5;
      target.max_age := 12;
      target.isExclusive := True;      
            -- Edinburgh : ages 5 - 12 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 5 - 12 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3600.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_1_Low_Cost;



   function Construct_Edinburgh_1 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_1" );
      ap.Append( regime.applications, Construct_Edinburgh_1_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_1_Low_Cost );
      return regime;
   end Construct_Edinburgh_1;

   --
   -- Elgin_2
   --

   function Construct_Elgin_2_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "2_Elgin : Co-ed 2-8 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Elgin : ages 2 - 8 - high cost" );
      target.min_age := 2;
      target.max_age := 8;
      target.isExclusive := True;      
            -- Elgin : ages 2 - 8 - high cost
      charge.name := To_Unbounded_String( "Elgin : ages 2 - 8 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3960.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Elgin_2_High_Cost;


   function Construct_Elgin_2_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "2_Elgin : Co-ed 2-8 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Elgin : ages 2 - 8 - low cost" );
      target.min_age := 2;
      target.max_age := 8;
      target.isExclusive := True;      
            -- Elgin : ages 2 - 8 - low cost
      charge.name := To_Unbounded_String( "Elgin : ages 2 - 8 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3960.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Elgin_2_Low_Cost;



   function Construct_Elgin_2 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Elgin_2" );
      ap.Append( regime.applications, Construct_Elgin_2_High_Cost );
      ap.Append( regime.applications, Construct_Elgin_2_Low_Cost );
      return regime;
   end Construct_Elgin_2;

   --
   -- Glasgow_3
   --

   function Construct_Glasgow_3_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "3_Glasgow : Co-ed 3-12 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 3 - 12 - high cost" );
      target.min_age := 3;
      target.max_age := 12;
      target.isExclusive := True;      
            -- Glasgow : ages 3 - 12 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 3 - 12 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4380.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_3_High_Cost;


   function Construct_Glasgow_3_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "3_Glasgow : Co-ed 3-12 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 3 - 12 - low cost" );
      target.min_age := 3;
      target.max_age := 12;
      target.isExclusive := True;      
            -- Glasgow : ages 3 - 12 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 3 - 12 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4380.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_3_Low_Cost;



   function Construct_Glasgow_3 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Glasgow_3" );
      ap.Append( regime.applications, Construct_Glasgow_3_High_Cost );
      ap.Append( regime.applications, Construct_Glasgow_3_Low_Cost );
      return regime;
   end Construct_Glasgow_3;

   --
   -- Edinburgh_4
   --

   function Construct_Edinburgh_4_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "High Cost" );
      app.description := To_Unbounded_String( "high cost assumption: all people in ages 3-18 are eligible." );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3090.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6093.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_4_High_Cost;


   function Construct_Edinburgh_4_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "Low Cost" );
      app.name := To_Unbounded_String( "Low cost assumption: maximum of 1 person per family is eligible" );
      app.max_children := 1;
      app.max_people := 1;
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3090.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6093.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_4_Low_Cost;



   function Construct_Edinburgh_4 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Private School" );
      regime.description := To_Unbounded_String( "Private High School: Co-ed 3-18 yrs" );
      ap.Append( regime.applications, Construct_Edinburgh_4_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_4_Low_Cost );
      return regime;
   end Construct_Edinburgh_4;

   --
   -- East_Lothian_5
   --

   function Construct_East_Lothian_5_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "5_East Lothian : Co-ed 4-11 : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "East Lothian : ages 4 - 11 - high cost" );
      target.min_age := 4;
      target.max_age := 11;
      target.isExclusive := True;      
            -- East Lothian : ages 4 - 11 - high cost
      charge.name := To_Unbounded_String( "East Lothian : ages 4 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6230.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_East_Lothian_5_High_Cost;


   function Construct_East_Lothian_5_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "5_East Lothian : Co-ed 4-11 : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "East Lothian : ages 4 - 11 - low cost" );
      target.min_age := 4;
      target.max_age := 11;
      target.isExclusive := True;      
            -- East Lothian : ages 4 - 11 - low cost
      charge.name := To_Unbounded_String( "East Lothian : ages 4 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6230.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_East_Lothian_5_Low_Cost;



   function Construct_East_Lothian_5 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "East_Lothian_5" );
      ap.Append( regime.applications, Construct_East_Lothian_5_High_Cost );
      ap.Append( regime.applications, Construct_East_Lothian_5_Low_Cost );
      return regime;
   end Construct_East_Lothian_5;

   --
   -- Edinburgh_6
   --

   function Construct_Edinburgh_6_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "6_Edinburgh : Co-ed 5-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3150.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6264.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_6_High_Cost;


   function Construct_Edinburgh_6_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "6_Edinburgh : Co-ed 5-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3150.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6264.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_6_Low_Cost;



   function Construct_Edinburgh_6 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_6" );
      ap.Append( regime.applications, Construct_Edinburgh_6_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_6_Low_Cost );
      return regime;
   end Construct_Edinburgh_6;

   --
   -- Edinburgh_B_7
   --

   function Construct_Edinburgh_B_7_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "7_Edinburgh (B) : Co-ed 3-12 : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 3 - 12 - high cost" );
      target.min_age := 3;
      target.max_age := 12;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 3 - 12 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 3 - 12 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 14085.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_7_High_Cost;


   function Construct_Edinburgh_B_7_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "7_Edinburgh (B) : Co-ed 3-12 : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 3 - 12 - low cost" );
      target.min_age := 3;
      target.max_age := 12;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 3 - 12 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 3 - 12 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6663.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_7_Low_Cost;



   function Construct_Edinburgh_B_7 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_B_7" );
      ap.Append( regime.applications, Construct_Edinburgh_B_7_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_B_7_Low_Cost );
      return regime;
   end Construct_Edinburgh_B_7;

   --
   -- Aberdeen_8
   --

   function Construct_Aberdeen_8_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "8_Aberdeen : Co-ed 3-16 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Aberdeen : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4650.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 16 - high cost" );
      target.min_age := 12;
      target.max_age := 16;
      target.isExclusive := True;      
            -- Aberdeen : ages 12 - 16 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 16 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6969.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_8_High_Cost;


   function Construct_Aberdeen_8_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "8_Aberdeen : Co-ed 3-16 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Aberdeen : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4650.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 16 - low cost" );
      target.min_age := 12;
      target.max_age := 16;
      target.isExclusive := True;      
            -- Aberdeen : ages 12 - 16 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 16 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6969.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_8_Low_Cost;



   function Construct_Aberdeen_8 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Aberdeen_8" );
      ap.Append( regime.applications, Construct_Aberdeen_8_High_Cost );
      ap.Append( regime.applications, Construct_Aberdeen_8_Low_Cost );
      return regime;
   end Construct_Aberdeen_8;

   function Construct_Glasgow_9_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "9_Glasgow : Girls 4-18 Boys 4-11 : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 4 - 11 - low cost" );
      target.min_age := 4;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Glasgow : ages 4 - 11 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 4 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5850.0;
      
      cp.Append( target.charges, charge ); 
      
      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      sp.Include( target.genders, female );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7200.0;
      
      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_9_Low_Cost;


   function Construct_Glasgow_9_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "9_Glasgow : Girls 4-18 Boys 4-11 : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 4 - 11 - high cost" );
      target.min_age := 4;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Glasgow : ages 4 - 11 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 4 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7200.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
            tp.Append( app.targets, target );
      cp.Clear( target.charges );
      sp.Include( target.genders, female );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7200.0;
      
      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_9_High_Cost;

   function Construct_Glasgow_9 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Glasgow_9" );
      ap.Append( regime.applications, Construct_Glasgow_9_High_Cost );
      ap.Append( regime.applications, Construct_Glasgow_9_Low_Cost );
      return regime;
   end Construct_Glasgow_9;

   --
   -- St_Andrews_10
   --

   function Construct_St_Andrews_10_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "10_St Andrews : Co-ed 3-12 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "St Andrews : ages 3 - 12 - high cost" );
      target.min_age := 3;
      target.max_age := 12;
      target.isExclusive := True;      
            -- St Andrews : ages 3 - 12 - high cost
      charge.name := To_Unbounded_String( "St Andrews : ages 3 - 12 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7227.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_St_Andrews_10_High_Cost;


   function Construct_St_Andrews_10_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "10_St Andrews : Co-ed 3-12 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "St Andrews : ages 3 - 12 - low cost" );
      target.min_age := 3;
      target.max_age := 12;
      target.isExclusive := True;      
            -- St Andrews : ages 3 - 12 - low cost
      charge.name := To_Unbounded_String( "St Andrews : ages 3 - 12 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7227.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_St_Andrews_10_Low_Cost;



   function Construct_St_Andrews_10 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "St_Andrews_10" );
      ap.Append( regime.applications, Construct_St_Andrews_10_High_Cost );
      ap.Append( regime.applications, Construct_St_Andrews_10_Low_Cost );
      return regime;
   end Construct_St_Andrews_10;

   --
   -- Edinburgh_B_11
   --

   function Construct_Edinburgh_B_11_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "11_Edinburgh (B) : 2-18 yrs Girls : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5655.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 18090.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_11_High_Cost;


   function Construct_Edinburgh_B_11_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "11_Edinburgh (B) : 2-18 yrs Girls : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5655.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7290.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_11_Low_Cost;



   function Construct_Edinburgh_B_11 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_B_11" );
      ap.Append( regime.applications, Construct_Edinburgh_B_11_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_B_11_Low_Cost );
      return regime;
   end Construct_Edinburgh_B_11;

   --
   -- Edinburgh_12
   --

   function Construct_Edinburgh_12_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "12_Edinburgh : Co-ed 3-11 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7500.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_12_High_Cost;


   function Construct_Edinburgh_12_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "12_Edinburgh : Co-ed 3-11 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7500.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_12_Low_Cost;



   function Construct_Edinburgh_12 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_12" );
      ap.Append( regime.applications, Construct_Edinburgh_12_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_12_Low_Cost );
      return regime;
   end Construct_Edinburgh_12;

   --
   -- Helensburgh_B_13
   --

   function Construct_Helensburgh_B_13_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "13_Helensburgh (B) : Co-ed 3-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Helensburgh (B) : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Helensburgh (B) : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Helensburgh (B) : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3810.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Helensburgh (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Helensburgh (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Helensburgh (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 16635.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Helensburgh_B_13_High_Cost;


   function Construct_Helensburgh_B_13_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "13_Helensburgh (B) : Co-ed 3-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Helensburgh (B) : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Helensburgh (B) : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Helensburgh (B) : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3810.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Helensburgh (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Helensburgh (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Helensburgh (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7770.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Helensburgh_B_13_Low_Cost;



   function Construct_Helensburgh_B_13 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Helensburgh_B_13" );
      ap.Append( regime.applications, Construct_Helensburgh_B_13_High_Cost );
      ap.Append( regime.applications, Construct_Helensburgh_B_13_Low_Cost );
      return regime;
   end Construct_Helensburgh_B_13;

   --
   -- Perth_14
   --

   function Construct_Perth_14_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "14_Perth : Co-ed 3-13 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Perth : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Perth : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Perth : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7800.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Perth : ages 12 - 13 - high cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Perth : ages 12 - 13 - high cost
      charge.name := To_Unbounded_String( "Perth : ages 12 - 13 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7800.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Perth_14_High_Cost;


   function Construct_Perth_14_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "14_Perth : Co-ed 3-13 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Perth : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Perth : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Perth : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7800.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Perth : ages 12 - 13 - low cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Perth : ages 12 - 13 - low cost
      charge.name := To_Unbounded_String( "Perth : ages 12 - 13 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7800.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Perth_14_Low_Cost;



   function Construct_Perth_14 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Perth_14" );
      ap.Append( regime.applications, Construct_Perth_14_High_Cost );
      ap.Append( regime.applications, Construct_Perth_14_Low_Cost );
      return regime;
   end Construct_Perth_14;

   --
   -- Edinburgh_B_15
   --

   function Construct_Edinburgh_B_15_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "15_Edinburgh (B) : 12-18 Boys : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7833.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15627.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_15_High_Cost;


   function Construct_Edinburgh_B_15_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "15_Edinburgh (B) : 12-18 Boys : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7833.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7833.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_15_Low_Cost;



   function Construct_Edinburgh_B_15 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_B_15" );
      ap.Append( regime.applications, Construct_Edinburgh_B_15_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_B_15_Low_Cost );
      return regime;
   end Construct_Edinburgh_B_15;

   --
   -- Edinburgh_B_16
   --

   function Construct_Edinburgh_B_16_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "16_Edinburgh (B) : 12-18 Girls : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7883.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 14085.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_16_High_Cost;


   function Construct_Edinburgh_B_16_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "16_Edinburgh (B) : 12-18 Girls : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7883.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7833.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_16_Low_Cost;



   function Construct_Edinburgh_B_16 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_B_16" );
      ap.Append( regime.applications, Construct_Edinburgh_B_16_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_B_16_Low_Cost );
      return regime;
   end Construct_Edinburgh_B_16;

   --
   -- Edinburgh_17
   --

   function Construct_Edinburgh_17_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "17_Edinburgh : Co-ed 3-18 : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5211.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7845.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_17_High_Cost;


   function Construct_Edinburgh_17_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "17_Edinburgh : Co-ed 3-18 : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5211.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7845.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_17_Low_Cost;



   function Construct_Edinburgh_17 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_17" );
      ap.Append( regime.applications, Construct_Edinburgh_17_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_17_Low_Cost );
      return regime;
   end Construct_Edinburgh_17;

   --
   -- Stirling_18
   --

   function Construct_Stirling_18_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "18_Stirling : Co-ed 3-18yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Stirling : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Stirling : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Stirling : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6210.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Stirling : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Stirling : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Stirling : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7845.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Stirling_18_High_Cost;


   function Construct_Stirling_18_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "18_Stirling : Co-ed 3-18yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Stirling : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Stirling : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Stirling : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6210.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Stirling : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Stirling : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Stirling : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7845.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Stirling_18_Low_Cost;



   function Construct_Stirling_18 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Stirling_18" );
      ap.Append( regime.applications, Construct_Stirling_18_High_Cost );
      ap.Append( regime.applications, Construct_Stirling_18_Low_Cost );
      return regime;
   end Construct_Stirling_18;

   --
   -- Glasgow_19
   --

   function Construct_Glasgow_19_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "19_Glasgow : 3-18yrs Girls : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Glasgow : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6165.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Glasgow : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7860.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_19_High_Cost;


   function Construct_Glasgow_19_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "19_Glasgow : 3-18yrs Girls : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Glasgow : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6165.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Glasgow : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7860.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_19_Low_Cost;



   function Construct_Glasgow_19 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Glasgow_19" );
      ap.Append( regime.applications, Construct_Glasgow_19_High_Cost );
      ap.Append( regime.applications, Construct_Glasgow_19_Low_Cost );
      return regime;
   end Construct_Glasgow_19;

   --
   -- Glasgow_20
   --

   function Construct_Glasgow_20_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "20_Glasgow : Co-ed 5-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Glasgow : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6126.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glasgow : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7869.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_20_High_Cost;


   function Construct_Glasgow_20_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "20_Glasgow : Co-ed 5-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Glasgow : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6126.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glasgow : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7869.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_20_Low_Cost;



   function Construct_Glasgow_20 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Glasgow_20" );
      ap.Append( regime.applications, Construct_Glasgow_20_High_Cost );
      ap.Append( regime.applications, Construct_Glasgow_20_Low_Cost );
      return regime;
   end Construct_Glasgow_20;

   --
   -- Hamilton_21
   --

   function Construct_Hamilton_21_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "21_Hamilton : Co-ed 5-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Hamilton : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Hamilton : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Hamilton : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4920.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Hamilton : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Hamilton : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Hamilton : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7935.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Hamilton_21_High_Cost;


   function Construct_Hamilton_21_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "21_Hamilton : Co-ed 5-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Hamilton : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Hamilton : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Hamilton : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4920.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Hamilton : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Hamilton : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Hamilton : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7935.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Hamilton_21_Low_Cost;



   function Construct_Hamilton_21 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Hamilton_21" );
      ap.Append( regime.applications, Construct_Hamilton_21_High_Cost );
      ap.Append( regime.applications, Construct_Hamilton_21_Low_Cost );
      return regime;
   end Construct_Hamilton_21;

   --
   -- Aberdeen_22
   --

   function Construct_Aberdeen_22_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "22_Aberdeen : Co-ed 3-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Aberdeen : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5100.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Aberdeen : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8025.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_22_High_Cost;


   function Construct_Aberdeen_22_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "22_Aberdeen : Co-ed 3-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Aberdeen : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5100.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Aberdeen : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8025.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_22_Low_Cost;



   function Construct_Aberdeen_22 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Aberdeen_22" );
      ap.Append( regime.applications, Construct_Aberdeen_22_High_Cost );
      ap.Append( regime.applications, Construct_Aberdeen_22_Low_Cost );
      return regime;
   end Construct_Aberdeen_22;

   --
   -- Kilmalcome_23
   --

   function Construct_Kilmalcome_23_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "23_Kilmalcome : Co-ed 3-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Kilmalcome : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Kilmalcome : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Kilmalcome : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2766.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Kilmalcome : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Kilmalcome : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Kilmalcome : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8043.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Kilmalcome_23_High_Cost;


   function Construct_Kilmalcome_23_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "23_Kilmalcome : Co-ed 3-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Kilmalcome : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Kilmalcome : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Kilmalcome : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2766.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Kilmalcome : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Kilmalcome : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Kilmalcome : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8043.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Kilmalcome_23_Low_Cost;



   function Construct_Kilmalcome_23 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Kilmalcome_23" );
      ap.Append( regime.applications, Construct_Kilmalcome_23_High_Cost );
      ap.Append( regime.applications, Construct_Kilmalcome_23_Low_Cost );
      return regime;
   end Construct_Kilmalcome_23;

   --
   -- Glasgow_24
   --

   function Construct_Glasgow_24_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "24_Glasgow : Co-ed 3-18 : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Glasgow : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5175.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glasgow : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8100.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_24_High_Cost;


   function Construct_Glasgow_24_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "24_Glasgow : Co-ed 3-18 : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Glasgow : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5175.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glasgow : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8100.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_24_Low_Cost;



   function Construct_Glasgow_24 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Glasgow_24" );
      ap.Append( regime.applications, Construct_Glasgow_24_High_Cost );
      ap.Append( regime.applications, Construct_Glasgow_24_Low_Cost );
      return regime;
   end Construct_Glasgow_24;

   --
   -- Aberdeen_25
   --

   function Construct_Aberdeen_25_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "25_Aberdeen : 3-18 yrs Girls : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Aberdeen : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4650.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Aberdeen : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8112.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_25_High_Cost;


   function Construct_Aberdeen_25_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "25_Aberdeen : 3-18 yrs Girls : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Aberdeen : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4650.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Aberdeen : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8112.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_25_Low_Cost;



   function Construct_Aberdeen_25 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Aberdeen_25" );
      ap.Append( regime.applications, Construct_Aberdeen_25_High_Cost );
      ap.Append( regime.applications, Construct_Aberdeen_25_Low_Cost );
      return regime;
   end Construct_Aberdeen_25;

   --
   -- Edinburgh_26
   --

   function Construct_Edinburgh_26_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "26_Edinburgh : Co-ed 3-18 : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5265.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8136.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_26_High_Cost;


   function Construct_Edinburgh_26_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "26_Edinburgh : Co-ed 3-18 : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5265.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8136.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_26_Low_Cost;



   function Construct_Edinburgh_26 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_26" );
      ap.Append( regime.applications, Construct_Edinburgh_26_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_26_Low_Cost );
      return regime;
   end Construct_Edinburgh_26;

   --
   -- Dollar_B_27
   --

   function Construct_Dollar_B_27_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "27_Dollar (B) : Co-ed 5-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dollar (B) : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Dollar (B) : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Dollar (B) : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6084.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dollar (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Dollar (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Dollar (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 18585.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Dollar_B_27_High_Cost;


   function Construct_Dollar_B_27_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "27_Dollar (B) : Co-ed 5-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dollar (B) : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Dollar (B) : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Dollar (B) : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6084.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dollar (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Dollar (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Dollar (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8145.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Dollar_B_27_Low_Cost;



   function Construct_Dollar_B_27 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Dollar_B_27" );
      ap.Append( regime.applications, Construct_Dollar_B_27_High_Cost );
      ap.Append( regime.applications, Construct_Dollar_B_27_Low_Cost );
      return regime;
   end Construct_Dollar_B_27;

   --
   -- Crieff_B_28
   --

   function Construct_Crieff_B_28_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "28_Crieff (B) : Co-ed 3-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Crieff (B) : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Crieff (B) : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Crieff (B) : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5385.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Crieff (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Crieff (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Crieff (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 19875.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Crieff_B_28_High_Cost;


   function Construct_Crieff_B_28_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "28_Crieff (B) : Co-ed 3-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Crieff (B) : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Crieff (B) : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Crieff (B) : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5385.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Crieff (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Crieff (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Crieff (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8145.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Crieff_B_28_Low_Cost;



   function Construct_Crieff_B_28 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Crieff_B_28" );
      ap.Append( regime.applications, Construct_Crieff_B_28_High_Cost );
      ap.Append( regime.applications, Construct_Crieff_B_28_Low_Cost );
      return regime;
   end Construct_Crieff_B_28;

   --
   -- Glasgow_29
   --

   function Construct_Glasgow_29_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "29_Glasgow : Co-ed 3 -18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Glasgow : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5067.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glasgow : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8163.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_29_High_Cost;


   function Construct_Glasgow_29_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "29_Glasgow : Co-ed 3 -18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Glasgow : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5067.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glasgow : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8163.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_29_Low_Cost;



   function Construct_Glasgow_29 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Glasgow_29" );
      ap.Append( regime.applications, Construct_Glasgow_29_High_Cost );
      ap.Append( regime.applications, Construct_Glasgow_29_Low_Cost );
      return regime;
   end Construct_Glasgow_29;

   --
   -- Aberdeen_30
   --

   function Construct_Aberdeen_30_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "30_Aberdeen : Co-ed 3 -18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Aberdeen : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5001.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Aberdeen : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8200.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_30_High_Cost;


   function Construct_Aberdeen_30_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "30_Aberdeen : Co-ed 3 -18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Aberdeen : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5001.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Aberdeen : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8200.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_30_Low_Cost;



   function Construct_Aberdeen_30 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Aberdeen_30" );
      ap.Append( regime.applications, Construct_Aberdeen_30_High_Cost );
      ap.Append( regime.applications, Construct_Aberdeen_30_Low_Cost );
      return regime;
   end Construct_Aberdeen_30;

   --
   -- Dundee_31
   --

   function Construct_Dundee_31_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "31_Dundee : Co-ed 5-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dundee : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Dundee : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Dundee : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5841.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dundee : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Dundee : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Dundee : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8304.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Dundee_31_High_Cost;


   function Construct_Dundee_31_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "31_Dundee : Co-ed 5-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dundee : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Dundee : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Dundee : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5841.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dundee : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Dundee : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Dundee : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8304.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Dundee_31_Low_Cost;



   function Construct_Dundee_31 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Dundee_31" );
      ap.Append( regime.applications, Construct_Dundee_31_High_Cost );
      ap.Append( regime.applications, Construct_Dundee_31_Low_Cost );
      return regime;
   end Construct_Dundee_31;

   --
   -- Glasgow_32
   --

   function Construct_Glasgow_32_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "32_Glasgow : Co-ed 3-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Glasgow : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5187.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glasgow : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8511.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_32_High_Cost;


   function Construct_Glasgow_32_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "32_Glasgow : Co-ed 3-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Glasgow : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5187.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glasgow : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Glasgow : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8511.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glasgow_32_Low_Cost;



   function Construct_Glasgow_32 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Glasgow_32" );
      ap.Append( regime.applications, Construct_Glasgow_32_High_Cost );
      ap.Append( regime.applications, Construct_Glasgow_32_Low_Cost );
      return regime;
   end Construct_Glasgow_32;

   --
   -- Ayr_33
   --

   function Construct_Ayr_33_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "33_Ayr : Co-ed 3-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Ayr : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Ayr : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Ayr : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4335.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Ayr : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Ayr : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Ayr : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8520.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Ayr_33_High_Cost;


   function Construct_Ayr_33_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "33_Ayr : Co-ed 3-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Ayr : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Ayr : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Ayr : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4335.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Ayr : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Ayr : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Ayr : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8520.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Ayr_33_Low_Cost;



   function Construct_Ayr_33 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Ayr_33" );
      ap.Append( regime.applications, Construct_Ayr_33_High_Cost );
      ap.Append( regime.applications, Construct_Ayr_33_Low_Cost );
      return regime;
   end Construct_Ayr_33;

   --
   -- Edinburgh_B_34
   --

   function Construct_Edinburgh_B_34_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "34_Edinburgh (B) : 0-18 yrs Girls : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5133.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10011.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_34_High_Cost;


   function Construct_Edinburgh_B_34_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "34_Edinburgh (B) : 0-18 yrs Girls : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5133.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Edinburgh (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8634.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_34_Low_Cost;



   function Construct_Edinburgh_B_34 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_B_34" );
      ap.Append( regime.applications, Construct_Edinburgh_B_34_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_B_34_Low_Cost );
      return regime;
   end Construct_Edinburgh_B_34;

   --
   -- St_Andrews_B_35
   --

   function Construct_St_Andrews_B_35_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "35_St Andrews (B) : Co-ed 12-19 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "St Andrews (B) : ages 12 - 19 - high cost" );
      target.min_age := 12;
      target.max_age := 19;
      target.isExclusive := True;      
            -- St Andrews (B) : ages 12 - 19 - high cost
      charge.name := To_Unbounded_String( "St Andrews (B) : ages 12 - 19 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 21174.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_St_Andrews_B_35_High_Cost;


   function Construct_St_Andrews_B_35_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "35_St Andrews (B) : Co-ed 12-19 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "St Andrews (B) : ages 12 - 19 - low cost" );
      target.min_age := 12;
      target.max_age := 19;
      target.isExclusive := True;      
            -- St Andrews (B) : ages 12 - 19 - low cost
      charge.name := To_Unbounded_String( "St Andrews (B) : ages 12 - 19 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8892.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_St_Andrews_B_35_Low_Cost;



   function Construct_St_Andrews_B_35 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "St_Andrews_B_35" );
      ap.Append( regime.applications, Construct_St_Andrews_B_35_High_Cost );
      ap.Append( regime.applications, Construct_St_Andrews_B_35_Low_Cost );
      return regime;
   end Construct_St_Andrews_B_35;

   --
   -- Crieff_B_36
   --

   function Construct_Crieff_B_36_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "36_Crieff (B) : Co-ed 3-13 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Crieff (B) : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Crieff (B) : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Crieff (B) : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4620.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Crieff (B) : ages 12 - 13 - high cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Crieff (B) : ages 12 - 13 - high cost
      charge.name := To_Unbounded_String( "Crieff (B) : ages 12 - 13 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 14817.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Crieff_B_36_High_Cost;


   function Construct_Crieff_B_36_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "36_Crieff (B) : Co-ed 3-13 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Crieff (B) : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Crieff (B) : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Crieff (B) : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4620.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Crieff (B) : ages 12 - 13 - low cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Crieff (B) : ages 12 - 13 - low cost
      charge.name := To_Unbounded_String( "Crieff (B) : ages 12 - 13 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 9858.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Crieff_B_36_Low_Cost;



   function Construct_Crieff_B_36 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Crieff_B_36" );
      ap.Append( regime.applications, Construct_Crieff_B_36_High_Cost );
      ap.Append( regime.applications, Construct_Crieff_B_36_Low_Cost );
      return regime;
   end Construct_Crieff_B_36;

   --
   -- Edinburgh_B_37
   --

   function Construct_Edinburgh_B_37_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "37_Edinburgh (B) : Co-ed 3-13 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6750.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 13 - high cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 12 - 13 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 13 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 13380.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_37_High_Cost;


   function Construct_Edinburgh_B_37_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "37_Edinburgh (B) : Co-ed 3-13 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6750.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 13 - low cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 12 - 13 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 13 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10080.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_37_Low_Cost;



   function Construct_Edinburgh_B_37 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_B_37" );
      ap.Append( regime.applications, Construct_Edinburgh_B_37_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_B_37_Low_Cost );
      return regime;
   end Construct_Edinburgh_B_37;

   --
   -- Edinburgh_B_38
   --

   function Construct_Edinburgh_B_38_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "38_Edinburgh (B) : Co-ed 7-13 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 7 - 11 - high cost" );
      target.min_age := 7;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 7 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 7 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10326.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 13 - high cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 12 - 13 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 13 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 16167.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_38_High_Cost;


   function Construct_Edinburgh_B_38_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "38_Edinburgh (B) : Co-ed 7-13 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 7 - 11 - low cost" );
      target.min_age := 7;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 7 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 7 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10326.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 13 - low cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 12 - 13 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 13 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10326.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_38_Low_Cost;



   function Construct_Edinburgh_B_38 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_B_38" );
      ap.Append( regime.applications, Construct_Edinburgh_B_38_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_B_38_Low_Cost );
      return regime;
   end Construct_Edinburgh_B_38;

   --
   -- Melrose_B_39
   --

   function Construct_Melrose_B_39_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "39_Melrose (B) : Co-ed 2-13 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Melrose (B) : ages 2 - 11 - high cost" );
      target.min_age := 2;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Melrose (B) : ages 2 - 11 - high cost
      charge.name := To_Unbounded_String( "Melrose (B) : ages 2 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7500.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Melrose (B) : ages 12 - 13 - high cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Melrose (B) : ages 12 - 13 - high cost
      charge.name := To_Unbounded_String( "Melrose (B) : ages 12 - 13 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 12300.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Melrose_B_39_High_Cost;


   function Construct_Melrose_B_39_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "39_Melrose (B) : Co-ed 2-13 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Melrose (B) : ages 2 - 11 - low cost" );
      target.min_age := 2;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Melrose (B) : ages 2 - 11 - low cost
      charge.name := To_Unbounded_String( "Melrose (B) : ages 2 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7500.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Melrose (B) : ages 12 - 13 - low cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Melrose (B) : ages 12 - 13 - low cost
      charge.name := To_Unbounded_String( "Melrose (B) : ages 12 - 13 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10500.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Melrose_B_39_Low_Cost;



   function Construct_Melrose_B_39 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Melrose_B_39" );
      ap.Append( regime.applications, Construct_Melrose_B_39_High_Cost );
      ap.Append( regime.applications, Construct_Melrose_B_39_Low_Cost );
      return regime;
   end Construct_Melrose_B_39;

   --
   -- Dunbar_B_40
   --

   function Construct_Dunbar_B_40_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "40_Dunbar (B) : Co-ed 7-13 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dunbar (B) : ages 7 - 11 - high cost" );
      target.min_age := 7;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Dunbar (B) : ages 7 - 11 - high cost
      charge.name := To_Unbounded_String( "Dunbar (B) : ages 7 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10920.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dunbar (B) : ages 12 - 13 - high cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Dunbar (B) : ages 12 - 13 - high cost
      charge.name := To_Unbounded_String( "Dunbar (B) : ages 12 - 13 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15750.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Dunbar_B_40_High_Cost;


   function Construct_Dunbar_B_40_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "40_Dunbar (B) : Co-ed 7-13 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dunbar (B) : ages 7 - 11 - low cost" );
      target.min_age := 7;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Dunbar (B) : ages 7 - 11 - low cost
      charge.name := To_Unbounded_String( "Dunbar (B) : ages 7 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10920.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dunbar (B) : ages 12 - 13 - low cost" );
      target.min_age := 12;
      target.max_age := 13;
      target.isExclusive := True;      
            -- Dunbar (B) : ages 12 - 13 - low cost
      charge.name := To_Unbounded_String( "Dunbar (B) : ages 12 - 13 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10920.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Dunbar_B_40_Low_Cost;



   function Construct_Dunbar_B_40 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Dunbar_B_40" );
      ap.Append( regime.applications, Construct_Dunbar_B_40_High_Cost );
      ap.Append( regime.applications, Construct_Dunbar_B_40_Low_Cost );
      return regime;
   end Construct_Dunbar_B_40;

   --
   -- Musselburgh_B_41
   --

   function Construct_Musselburgh_B_41_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "41_Musselburgh (B) : Co-ed 3-12 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Musselburgh (B) : ages 3 - 12 - high cost" );
      target.min_age := 3;
      target.max_age := 12;
      target.isExclusive := True;      
            -- Musselburgh (B) : ages 3 - 12 - high cost
      charge.name := To_Unbounded_String( "Musselburgh (B) : ages 3 - 12 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 16650.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Musselburgh_B_41_High_Cost;


   function Construct_Musselburgh_B_41_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "41_Musselburgh (B) : Co-ed 3-12 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Musselburgh (B) : ages 3 - 12 - low cost" );
      target.min_age := 3;
      target.max_age := 12;
      target.isExclusive := True;      
            -- Musselburgh (B) : ages 3 - 12 - low cost
      charge.name := To_Unbounded_String( "Musselburgh (B) : ages 3 - 12 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 11046.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Musselburgh_B_41_Low_Cost;



   function Construct_Musselburgh_B_41 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Musselburgh_B_41" );
      ap.Append( regime.applications, Construct_Musselburgh_B_41_High_Cost );
      ap.Append( regime.applications, Construct_Musselburgh_B_41_Low_Cost );
      return regime;
   end Construct_Musselburgh_B_41;

   --
   -- Bridge_of_Earn_B_42
   --

   function Construct_Bridge_of_Earn_B_42_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "42_Bridge of Earn (B) : 2-18 yrs Girls : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Bridge of Earn (B) : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Bridge of Earn (B) : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Bridge of Earn (B) : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6735.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Bridge of Earn (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Bridge of Earn (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Bridge of Earn (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 19935.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Bridge_of_Earn_B_42_High_Cost;


   function Construct_Bridge_of_Earn_B_42_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "42_Bridge of Earn (B) : 2-18 yrs Girls : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Bridge of Earn (B) : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Bridge of Earn (B) : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Bridge of Earn (B) : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6735.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Bridge of Earn (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, female );
            -- Bridge of Earn (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Bridge of Earn (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 11685.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Bridge_of_Earn_B_42_Low_Cost;



   function Construct_Bridge_of_Earn_B_42 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Bridge_of_Earn_B_42" );
      ap.Append( regime.applications, Construct_Bridge_of_Earn_B_42_High_Cost );
      ap.Append( regime.applications, Construct_Bridge_of_Earn_B_42_Low_Cost );
      return regime;
   end Construct_Bridge_of_Earn_B_42;

   --
   -- Montrose_B_43
   --

   function Construct_Montrose_B_43_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "43_Montrose (B) : Co-ed 0-14 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Montrose (B) : ages 0 - 11 - high cost" );
      target.min_age := 0;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Montrose (B) : ages 0 - 11 - high cost
      charge.name := To_Unbounded_String( "Montrose (B) : ages 0 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7248.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Montrose (B) : ages 12 - 14 - high cost" );
      target.min_age := 12;
      target.max_age := 14;
      target.isExclusive := True;      
            -- Montrose (B) : ages 12 - 14 - high cost
      charge.name := To_Unbounded_String( "Montrose (B) : ages 12 - 14 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15435.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Montrose_B_43_High_Cost;


   function Construct_Montrose_B_43_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "43_Montrose (B) : Co-ed 0-14 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Montrose (B) : ages 0 - 11 - low cost" );
      target.min_age := 0;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Montrose (B) : ages 0 - 11 - low cost
      charge.name := To_Unbounded_String( "Montrose (B) : ages 0 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7248.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Montrose (B) : ages 12 - 14 - low cost" );
      target.min_age := 12;
      target.max_age := 14;
      target.isExclusive := True;      
            -- Montrose (B) : ages 12 - 14 - low cost
      charge.name := To_Unbounded_String( "Montrose (B) : ages 12 - 14 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 12465.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Montrose_B_43_Low_Cost;



   function Construct_Montrose_B_43 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Montrose_B_43" );
      ap.Append( regime.applications, Construct_Montrose_B_43_High_Cost );
      ap.Append( regime.applications, Construct_Montrose_B_43_Low_Cost );
      return regime;
   end Construct_Montrose_B_43;

   --
   -- Musselburgh_B_44
   --

   function Construct_Musselburgh_B_44_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "44_Musselburgh (B) : Co-ed 12-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Musselburgh (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Musselburgh (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Musselburgh (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 22080.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Musselburgh_B_44_High_Cost;


   function Construct_Musselburgh_B_44_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "44_Musselburgh (B) : Co-ed 12-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Musselburgh (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Musselburgh (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Musselburgh (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 14595.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Musselburgh_B_44_Low_Cost;



   function Construct_Musselburgh_B_44 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Musselburgh_B_44" );
      ap.Append( regime.applications, Construct_Musselburgh_B_44_High_Cost );
      ap.Append( regime.applications, Construct_Musselburgh_B_44_Low_Cost );
      return regime;
   end Construct_Musselburgh_B_44;

   --
   -- Forgandenny_B_45
   --

   function Construct_Forgandenny_B_45_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "45_Forgandenny (B) : Co-ed 10-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Forgandenny (B) : ages 10 - 11 - high cost" );
      target.min_age := 10;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Forgandenny (B) : ages 10 - 11 - high cost
      charge.name := To_Unbounded_String( "Forgandenny (B) : ages 10 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 9690.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Forgandenny (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Forgandenny (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Forgandenny (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 21375.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Forgandenny_B_45_High_Cost;


   function Construct_Forgandenny_B_45_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "45_Forgandenny (B) : Co-ed 10-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Forgandenny (B) : ages 10 - 11 - low cost" );
      target.min_age := 10;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Forgandenny (B) : ages 10 - 11 - low cost
      charge.name := To_Unbounded_String( "Forgandenny (B) : ages 10 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 9690.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Forgandenny (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Forgandenny (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Forgandenny (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 14745.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Forgandenny_B_45_Low_Cost;



   function Construct_Forgandenny_B_45 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Forgandenny_B_45" );
      ap.Append( regime.applications, Construct_Forgandenny_B_45_High_Cost );
      ap.Append( regime.applications, Construct_Forgandenny_B_45_Low_Cost );
      return regime;
   end Construct_Forgandenny_B_45;

   --
   -- Dumfries_B_46
   --

   function Construct_Dumfries_B_46_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "46_Dumfries (B) : Small Specialist : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dumfries (B) : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Dumfries (B) : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Dumfries (B) : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15000.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dumfries (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Dumfries (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Dumfries (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 22300.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Dumfries_B_46_High_Cost;


   function Construct_Dumfries_B_46_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "46_Dumfries (B) : Small Specialist : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dumfries (B) : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Dumfries (B) : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Dumfries (B) : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15000.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Dumfries (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Dumfries (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Dumfries (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15000.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Dumfries_B_46_Low_Cost;



   function Construct_Dumfries_B_46 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Dumfries_B_46" );
      ap.Append( regime.applications, Construct_Dumfries_B_46_High_Cost );
      ap.Append( regime.applications, Construct_Dumfries_B_46_Low_Cost );
      return regime;
   end Construct_Dumfries_B_46;

   --
   -- Glenalmond_B_47
   --

   function Construct_Glenalmond_B_47_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "47_Glenalmond (B) : Co-ed 12-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glenalmond (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glenalmond (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Glenalmond (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 22545.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glenalmond_B_47_High_Cost;


   function Construct_Glenalmond_B_47_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "47_Glenalmond (B) : Co-ed 12-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Glenalmond (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Glenalmond (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Glenalmond (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15375.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Glenalmond_B_47_Low_Cost;



   function Construct_Glenalmond_B_47 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Glenalmond_B_47" );
      ap.Append( regime.applications, Construct_Glenalmond_B_47_High_Cost );
      ap.Append( regime.applications, Construct_Glenalmond_B_47_Low_Cost );
      return regime;
   end Construct_Glenalmond_B_47;

   --
   -- Aberdeen_48
   --

   function Construct_Aberdeen_48_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "48_Aberdeen : Co-ed 3-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Aberdeen : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 13800.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Aberdeen : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15434.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_48_High_Cost;


   function Construct_Aberdeen_48_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "48_Aberdeen : Co-ed 3-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - low cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Aberdeen : ages 3 - 11 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 3 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 13800.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Aberdeen : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Aberdeen : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15434.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Aberdeen_48_Low_Cost;



   function Construct_Aberdeen_48 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Aberdeen_48" );
      ap.Append( regime.applications, Construct_Aberdeen_48_High_Cost );
      ap.Append( regime.applications, Construct_Aberdeen_48_Low_Cost );
      return regime;
   end Construct_Aberdeen_48;

   --
   -- Edinburgh_B_49
   --

   function Construct_Edinburgh_B_49_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "49_Edinburgh (B) : 8-18 yrs Boys : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10005.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 21795.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_49_High_Cost;


   function Construct_Edinburgh_B_49_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "49_Edinburgh (B) : 8-18 yrs Boys : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      target.min_age := 5;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10005.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15585.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_49_Low_Cost;



   function Construct_Edinburgh_B_49 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_B_49" );
      ap.Append( regime.applications, Construct_Edinburgh_B_49_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_B_49_Low_Cost );
      return regime;
   end Construct_Edinburgh_B_49;

   --
   -- Edinburgh_B_50
   --

   function Construct_Edinburgh_B_50_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "50_Edinburgh (B) : Co-ed 13-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 13 - 18 - high cost" );
      target.min_age := 13;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 13 - 18 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 13 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 22326.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_50_High_Cost;


   function Construct_Edinburgh_B_50_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "50_Edinburgh (B) : Co-ed 13-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 13 - 18 - low cost" );
      target.min_age := 13;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Edinburgh (B) : ages 13 - 18 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 13 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15840.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_B_50_Low_Cost;



   function Construct_Edinburgh_B_50 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Edinburgh_B_50" );
      ap.Append( regime.applications, Construct_Edinburgh_B_50_High_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_B_50_Low_Cost );
      return regime;
   end Construct_Edinburgh_B_50;

   --
   -- Elgin_B_51
   --

   function Construct_Elgin_B_51_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "51_Elgin (B) : Co-ed 8-18 yrs : high cost" );
      app.max_children := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Elgin (B) : ages 8 - 11 - high cost" );
      target.min_age := 8;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Elgin (B) : ages 8 - 11 - high cost
      charge.name := To_Unbounded_String( "Elgin (B) : ages 8 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8823.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Elgin (B) : ages 12 - 18 - high cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Elgin (B) : ages 12 - 18 - high cost
      charge.name := To_Unbounded_String( "Elgin (B) : ages 12 - 18 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 24162.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Elgin_B_51_High_Cost;


   function Construct_Elgin_B_51_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "51_Elgin (B) : Co-ed 8-18 yrs : low cost" );
      app.max_children := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Elgin (B) : ages 8 - 11 - low cost" );
      target.min_age := 8;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Elgin (B) : ages 8 - 11 - low cost
      charge.name := To_Unbounded_String( "Elgin (B) : ages 8 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8823.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Elgin (B) : ages 12 - 18 - low cost" );
      target.min_age := 12;
      target.max_age := 18;
      target.isExclusive := True;      
            -- Elgin (B) : ages 12 - 18 - low cost
      charge.name := To_Unbounded_String( "Elgin (B) : ages 12 - 18 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 17700.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Elgin_B_51_Low_Cost;



   function Construct_Elgin_B_51 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Elgin_B_51" );
      ap.Append( regime.applications, Construct_Elgin_B_51_High_Cost );
      ap.Append( regime.applications, Construct_Elgin_B_51_Low_Cost );
      return regime;
   end Construct_Elgin_B_51;

   
   function Make_All_Regimes return Model.Charging.Charging_Regime_List is
      l : Model.Charging.Charging_Regime_List;
   begin
      rp.Append( l, Construct_Edinburgh_4 );
      rp.Append( l, Construct_Edinburgh_B_49 );
      
      -- rp.Append( l, Construct_Edinburgh_1 );
      -- rp.Append( l, Construct_Elgin_2 );
      -- rp.Append( l, Construct_Glasgow_3 );
      -- rp.Append( l, Construct_East_Lothian_5 );
      -- rp.Append( l, Construct_Edinburgh_6 );
      -- rp.Append( l, Construct_Edinburgh_B_7 );
      -- rp.Append( l, Construct_Aberdeen_8 );
      -- rp.Append( l, Construct_Glasgow_9 );
      -- rp.Append( l, Construct_St_Andrews_10 );
      -- rp.Append( l, Construct_Edinburgh_B_11 );
      -- rp.Append( l, Construct_Edinburgh_12 );
      -- rp.Append( l, Construct_Helensburgh_B_13 );
      -- rp.Append( l, Construct_Perth_14 );
      -- rp.Append( l, Construct_Edinburgh_B_15 );
      -- rp.Append( l, Construct_Edinburgh_B_16 );
      -- rp.Append( l, Construct_Edinburgh_17 );
      -- rp.Append( l, Construct_Stirling_18 );
      -- rp.Append( l, Construct_Glasgow_19 );
      -- rp.Append( l, Construct_Glasgow_20 );
      -- rp.Append( l, Construct_Hamilton_21 );
      -- rp.Append( l, Construct_Aberdeen_22 );
      -- rp.Append( l, Construct_Kilmalcome_23 );
      -- rp.Append( l, Construct_Glasgow_24 );
      -- rp.Append( l, Construct_Aberdeen_25 );
      -- rp.Append( l, Construct_Edinburgh_26 );
      -- rp.Append( l, Construct_Dollar_B_27 );
      -- rp.Append( l, Construct_Crieff_B_28 );
      -- rp.Append( l, Construct_Glasgow_29 );
      -- rp.Append( l, Construct_Aberdeen_30 );
      -- rp.Append( l, Construct_Dundee_31 );
      -- rp.Append( l, Construct_Glasgow_32 );
      -- rp.Append( l, Construct_Ayr_33 );
      -- rp.Append( l, Construct_Edinburgh_B_34 );
      -- rp.Append( l, Construct_St_Andrews_B_35 );
      -- rp.Append( l, Construct_Crieff_B_36 );
      -- rp.Append( l, Construct_Edinburgh_B_37 );
      -- rp.Append( l, Construct_Edinburgh_B_38 );
      -- rp.Append( l, Construct_Melrose_B_39 );
      -- rp.Append( l, Construct_Dunbar_B_40 );
      -- rp.Append( l, Construct_Musselburgh_B_41 );
      -- rp.Append( l, Construct_Bridge_of_Earn_B_42 );
      -- rp.Append( l, Construct_Montrose_B_43 );
      -- rp.Append( l, Construct_Musselburgh_B_44 );
      -- rp.Append( l, Construct_Forgandenny_B_45 );
      -- rp.Append( l, Construct_Dumfries_B_46 );
      -- rp.Append( l, Construct_Glenalmond_B_47 );
      -- rp.Append( l, Construct_Aberdeen_48 );
      -- rp.Append( l, Construct_Edinburgh_B_50 );
      -- rp.Append( l, Construct_Elgin_B_51 );
      
      return l;
   end Make_All_Regimes;   

   
end Private_School_Examples;
