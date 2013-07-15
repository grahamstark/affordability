with Ada.Strings.Unbounded;

with FRS_Enums;
with Model.Incomes;
with Model.Household;

package body Charging_Examples is

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

   function Construct_Medical_Charity_Hip_Replacement_60s_Plus return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "Medical: Hip Replacement for over 60s" );
      target.min_age := 60;
      charge.name := charge.name & "Total Hip Replacement";
      charge.period := annual;
      charge.frequency := 50.0; -- once in a lifetime; assumed over 5 years
      charge.charge_amount := 12_350.0;
      charge.interest_rate := 8.0;
      cp.Append( target.charges, charge );      
      tp.Append( app.targets, target );
      return app;
   end Construct_Medical_Charity_Hip_Replacement_60s_Plus;
   
   function Construct_Medical_Charity_Diagnostics return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "Medical: Consulations" );
      charge.name := charge.name & "Consulations at assumed 120";
      charge.period := monthly;
      charge.frequency := 3.0; -- once every 3 months
      charge.charge_amount := 120.0;
      cp.Append( target.charges, charge );      
      tp.Append( app.targets, target );
      return app;
   end Construct_Medical_Charity_Diagnostics;
   
   function Construct_Medical_Charity return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Medical" );
      ap.Append( regime.applications, Construct_Medical_Charity_Hip_Replacement_60s_Plus );
      ap.Append( regime.applications, Construct_Medical_Charity_Diagnostics );
      return regime;
   end Construct_Medical_Charity;
   
   function Construct_Golf_Club_One_Adult_One_Child return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "Golf: Apply to max 1 adult 1 child" );
      app.max_children := 1;
      app.max_adults := 1;

      target.name := target.name & "Student Membership";
      target.min_age := 17;
      target.max_age := 120;
      target.isExclusive := True;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every year
      charge.charge_amount := 60.0;
      ep.Include( target.employment, student );
      cp.Append( target.charges, charge );
      
      tp.Append( app.targets, target );

      ep.Clear( target.employment ); 
      target.name := target.name & "Normal Membership";
      target.min_age := 17;
      target.max_age := 59;
      target.isExclusive := True;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every year
      charge.charge_amount := 90.0;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      
      target.name := target.name & "Seniors";
      target.min_age := 60;
      target.max_age := 120;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; 
      charge.charge_amount := 60.0;      
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      
      target.name := target.name & "Juniors";
      target.min_age := 8;
      target.max_age := 16;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every term
      charge.charge_amount := 10.0;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      return app;
   end Construct_Golf_Club_One_Adult_One_Child;

   
   function Construct_Golf_Club_All_People return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "Golf: Apply to all people" );
      app.max_children := Child_Range'Last;
      app.max_adults := Household_Adult_Range'Last;

      target.name := target.name & "Student Membership";
      target.min_age := 17;
      target.max_age := 120;
      target.isExclusive := True;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every year
      charge.charge_amount := 60.0;
      ep.Include( target.employment, student );
      cp.Append( target.charges, charge );
      
      tp.Append( app.targets, target );

      ep.Clear( target.employment ); 
      target.name := target.name & "Normal Membership";
      target.min_age := 17;
      target.max_age := 59;
      target.isExclusive := True;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every year
      charge.charge_amount := 90.0;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      
      target.name := target.name & "Seniors";
      target.min_age := 60;
      target.max_age := 120;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; 
      charge.charge_amount := 60.0;      
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      
      target.name := target.name & "Juniors";
      target.min_age := 8;
      target.max_age := 16;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every term
      charge.charge_amount := 10.0;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      return app;
   end Construct_Golf_Club_All_People;
   
   function Constuct_Sports_Cheap return Model.Charging.Charging_Regime is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
      regime : Charging_Regime;   
   begin

 -- ======= MIN ========= 
      target.name := To_Unbounded_String( "Outdoor 5-a-Side Areas : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Outdoor 5-a-Side Areas : adult 
      charge.name := To_Unbounded_String( "Outdoor 5-a-Side Areas : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 12.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Outdoor 5-a-Side Areas : juvenile (per pitch per game)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Outdoor 5-a-Side Areas : juvenile (per pitch per game)
      charge.name := To_Unbounded_String( "Outdoor 5-a-Side Areas : juvenile (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7.07;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Outdoor 5-a-Side Areas : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Outdoor 5-a-Side Areas : unemployed 
      charge.name := To_Unbounded_String( "Outdoor 5-a-Side Areas : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Floodlighting for Synthetic : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Floodlighting for Synthetic : adult 
      charge.name := To_Unbounded_String( "Floodlighting for Synthetic : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Floodlighting for Synthetic : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Floodlighting for Synthetic : juvenile 
      charge.name := To_Unbounded_String( "Floodlighting for Synthetic : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Floodlighting for Synthetic : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Floodlighting for Synthetic : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Floodlighting for Synthetic : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Tennis Court : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Tennis Court : adult 
      charge.name := To_Unbounded_String( "Tennis Court : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Tennis Court : juvenile (per court per hour)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Tennis Court : juvenile (per court per hour)
      charge.name := To_Unbounded_String( "Tennis Court : juvenile (per court per hour)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Tennis Court : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Tennis Court : senior citizen 
      charge.name := To_Unbounded_String( "Tennis Court : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Tennis Court : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Tennis Court : unemployed 
      charge.name := To_Unbounded_String( "Tennis Court : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Tennis Season Ticket : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Tennis Season Ticket : adult 
      charge.name := To_Unbounded_String( "Tennis Season Ticket : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 24.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Tennis Season Ticket : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Tennis Season Ticket : juvenile (per person)
      charge.name := To_Unbounded_String( "Tennis Season Ticket : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 12.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Tennis Season Ticket : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Tennis Season Ticket : senior citizen 
      charge.name := To_Unbounded_String( "Tennis Season Ticket : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 12.45;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Tennis Season Ticket : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Tennis Season Ticket : unemployed 
      charge.name := To_Unbounded_String( "Tennis Season Ticket : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 12.45;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Session : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Bowls Session : adult 
      charge.name := To_Unbounded_String( "Bowls Session : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Session : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Bowls Session : juvenile (per person)
      charge.name := To_Unbounded_String( "Bowls Session : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Session : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Bowls Session : senior citizen 
      charge.name := To_Unbounded_String( "Bowls Session : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Session : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Bowls Session : unemployed 
      charge.name := To_Unbounded_String( "Bowls Session : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Bowls Season Ticket : adult 
      charge.name := To_Unbounded_String( "Bowls Season Ticket : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 20.9;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Bowls Season Ticket : juvenile (per person)
      charge.name := To_Unbounded_String( "Bowls Season Ticket : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Bowls Season Ticket : senior citizen 
      charge.name := To_Unbounded_String( "Bowls Season Ticket : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15.35;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Bowls Season Ticket : unemployed 
      charge.name := To_Unbounded_String( "Bowls Season Ticket : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15.35;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Bowls Season Ticket : adult 
      charge.name := To_Unbounded_String( "Bowls Season Ticket : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 9.7;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : juvenile (weekend) (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Bowls Season Ticket : juvenile (weekend) (per person)
      charge.name := To_Unbounded_String( "Bowls Season Ticket : juvenile (weekend) (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Bowls Season Ticket : senior citizen 
      charge.name := To_Unbounded_String( "Bowls Season Ticket : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Bowls Season Ticket : unemployed 
      charge.name := To_Unbounded_String( "Bowls Season Ticket : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Bowls Season Ticket : adult 
      charge.name := To_Unbounded_String( "Bowls Season Ticket : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5.75;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : juvenile (weekday) (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Bowls Season Ticket : juvenile (weekday) (per person)
      charge.name := To_Unbounded_String( "Bowls Season Ticket : juvenile (weekday) (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Bowls Season Ticket : senior citizen 
      charge.name := To_Unbounded_String( "Bowls Season Ticket : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Bowls Season Ticket : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Bowls Season Ticket : unemployed 
      charge.name := To_Unbounded_String( "Bowls Season Ticket : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Golf Season Ticket : adult 
      charge.name := To_Unbounded_String( "Golf Season Ticket : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 150.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Golf Season Ticket : juvenile (per person)
      charge.name := To_Unbounded_String( "Golf Season Ticket : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Golf Season Ticket : senior citizen 
      charge.name := To_Unbounded_String( "Golf Season Ticket : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 38.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Golf Season Ticket : unemployed 
      charge.name := To_Unbounded_String( "Golf Season Ticket : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 65.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Golf Season Ticket : adult 
      charge.name := To_Unbounded_String( "Golf Season Ticket : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.3;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket : juvenile (weekend) (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Golf Season Ticket : juvenile (weekend) (per person)
      charge.name := To_Unbounded_String( "Golf Season Ticket : juvenile (weekend) (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.25;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Golf Season Ticket : senior citizen 
      charge.name := To_Unbounded_String( "Golf Season Ticket : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.25;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Golf Season Ticket : unemployed 
      charge.name := To_Unbounded_String( "Golf Season Ticket : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.25;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket :  " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Golf Season Ticket :  
      charge.name := To_Unbounded_String( "Golf Season Ticket :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket :  " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Golf Season Ticket :  
      charge.name := To_Unbounded_String( "Golf Season Ticket :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket :  " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Golf Season Ticket :  
      charge.name := To_Unbounded_String( "Golf Season Ticket :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket :  " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Golf Season Ticket :  
      charge.name := To_Unbounded_String( "Golf Season Ticket :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Season Ticket : user " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;


      target.name := To_Unbounded_String( "Football Pitch (Sat pm) : juvenile (per pitch per game)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Football Pitch (Sat pm) : juvenile (per pitch per game)
      charge.name := To_Unbounded_String( "Football Pitch (Sat pm) : juvenile (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Football Pitch (Sat pm) : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Football Pitch (Sat pm) : unemployed 
      charge.name := To_Unbounded_String( "Football Pitch (Sat pm) : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Football Pitch - plus : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Football Pitch - plus : adult 
      charge.name := To_Unbounded_String( "Football Pitch - plus : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 13.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Football Pitch - plus : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Football Pitch - plus : juvenile 
      charge.name := To_Unbounded_String( "Football Pitch - plus : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Football Pitch - plus : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Football Pitch - plus : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Football Pitch - plus : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 6.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Rugby Pitch - plus : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Rugby Pitch - plus : adult 
      charge.name := To_Unbounded_String( "Rugby Pitch - plus : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Rugby Pitch - plus : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Rugby Pitch - plus : juvenile 
      charge.name := To_Unbounded_String( "Rugby Pitch - plus : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Rugby Pitch - plus : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Rugby Pitch - plus : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Rugby Pitch - plus : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Hockey Pitch - plus : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Hockey Pitch - plus : adult 
      charge.name := To_Unbounded_String( "Hockey Pitch - plus : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Hockey Pitch - plus : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Hockey Pitch - plus : juvenile 
      charge.name := To_Unbounded_String( "Hockey Pitch - plus : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Hockey Pitch - plus : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Hockey Pitch - plus : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Hockey Pitch - plus : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Shinty Pitch - plus : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Shinty Pitch - plus : adult 
      charge.name := To_Unbounded_String( "Shinty Pitch - plus : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 14.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Shinty Pitch - plus : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Shinty Pitch - plus : juvenile 
      charge.name := To_Unbounded_String( "Shinty Pitch - plus : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7.25;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Shinty Pitch - plus : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Shinty Pitch - plus : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Shinty Pitch - plus : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 7.25;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Cricket Pitch - plus : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Cricket Pitch - plus : adult 
      charge.name := To_Unbounded_String( "Cricket Pitch - plus : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Cricket Pitch - plus : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Cricket Pitch - plus : juvenile 
      charge.name := To_Unbounded_String( "Cricket Pitch - plus : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Cricket Pitch - plus : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Cricket Pitch - plus : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Cricket Pitch - plus : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Other Pitch Sports - plus : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Other Pitch Sports - plus : adult 
      charge.name := To_Unbounded_String( "Other Pitch Sports - plus : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 19.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Other Pitch Sports - plus : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Other Pitch Sports - plus : juvenile 
      charge.name := To_Unbounded_String( "Other Pitch Sports - plus : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 9.4;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Other Pitch Sports - plus : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Other Pitch Sports - plus : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Other Pitch Sports - plus : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 9.4;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Changing Facilities for : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Changing Facilities for : adult 
      charge.name := To_Unbounded_String( "Changing Facilities for : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.2;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Changing Facilities for : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Changing Facilities for : juvenile 
      charge.name := To_Unbounded_String( "Changing Facilities for : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Changing Facilities for : unemployed (Sat pm)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Changing Facilities for : unemployed (Sat pm)
      charge.name := To_Unbounded_String( "Changing Facilities for : unemployed (Sat pm)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.8;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Floodlighting for Grass : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Floodlighting for Grass : adult 
      charge.name := To_Unbounded_String( "Floodlighting for Grass : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Floodlighting for Grass : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Floodlighting for Grass : juvenile 
      charge.name := To_Unbounded_String( "Floodlighting for Grass : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Floodlighting for Grass : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Floodlighting for Grass : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Floodlighting for Grass : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Synthetic Grass Pitch : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Synthetic Grass Pitch : adult 
      charge.name := To_Unbounded_String( "Synthetic Grass Pitch : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 29.6;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Synthetic Grass Pitch : juvenile (Sat pm)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Synthetic Grass Pitch : juvenile (Sat pm)
      charge.name := To_Unbounded_String( "Synthetic Grass Pitch : juvenile (Sat pm)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Synthetic Grass Pitch : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Synthetic Grass Pitch : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Synthetic Grass Pitch : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 15.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Blaes/Dri-play/Polymeric : adult 
      charge.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 11.7;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Blaes/Dri-play/Polymeric : juvenile 
      charge.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5.85;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : unemployed (per pitch per game)" );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Blaes/Dri-play/Polymeric : unemployed (per pitch per game)
      charge.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : unemployed (per pitch per game)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 9.95;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric :  " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Blaes/Dri-play/Polymeric :  
      charge.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Blaes/Dri-play/Polymeric : adult 
      charge.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : juvenile (weekday) (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Blaes/Dri-play/Polymeric : juvenile (weekday) (per person)
      charge.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : juvenile (weekday) (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.7;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Blaes/Dri-play/Polymeric : senior citizen 
      charge.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.7;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Blaes/Dri-play/Polymeric : unemployed 
      charge.name := To_Unbounded_String( "Blaes/Dri-play/Polymeric : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Pitch 'n' Putt/Par 3 Golf : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Pitch 'n' Putt/Par 3 Golf : adult 
      charge.name := To_Unbounded_String( "Pitch 'n' Putt/Par 3 Golf : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Pitch 'n' Putt/Par 3 Golf : juvenile (per 9 holes per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Pitch 'n' Putt/Par 3 Golf : juvenile (per 9 holes per person)
      charge.name := To_Unbounded_String( "Pitch 'n' Putt/Par 3 Golf : juvenile (per 9 holes per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Pitch 'n' Putt/Par 3 Golf : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Pitch 'n' Putt/Par 3 Golf : senior citizen 
      charge.name := To_Unbounded_String( "Pitch 'n' Putt/Par 3 Golf : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Pitch 'n' Putt/Par 3 Golf : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Pitch 'n' Putt/Par 3 Golf : unemployed 
      charge.name := To_Unbounded_String( "Pitch 'n' Putt/Par 3 Golf : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Putting Round : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Putting Round : adult 
      charge.name := To_Unbounded_String( "Putting Round : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Putting Round : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Putting Round : juvenile (per person)
      charge.name := To_Unbounded_String( "Putting Round : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Putting Round : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Putting Round : senior citizen 
      charge.name := To_Unbounded_String( "Putting Round : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Putting Round : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Putting Round : unemployed 
      charge.name := To_Unbounded_String( "Putting Round : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Driving Range : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Golf Driving Range : adult 
      charge.name := To_Unbounded_String( "Golf Driving Range : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.4;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Driving Range : juvenile (per bucket of balls)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Golf Driving Range : juvenile (per bucket of balls)
      charge.name := To_Unbounded_String( "Golf Driving Range : juvenile (per bucket of balls)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.4;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Driving Range : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Golf Driving Range : senior citizen 
      charge.name := To_Unbounded_String( "Golf Driving Range : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.4;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Golf Driving Range : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Golf Driving Range : unemployed 
      charge.name := To_Unbounded_String( "Golf Driving Range : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.4;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Trampolining : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Trampolining : adult 
      charge.name := To_Unbounded_String( "Trampolining : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.1;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Trampolining : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Trampolining : juvenile (per person)
      charge.name := To_Unbounded_String( "Trampolining : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Athletics Session : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Athletics Session : adult 
      charge.name := To_Unbounded_String( "Athletics Session : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Athletics Session : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Athletics Session : juvenile (per person)
      charge.name := To_Unbounded_String( "Athletics Session : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Athletics Session : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Athletics Session : senior citizen 
      charge.name := To_Unbounded_String( "Athletics Session : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Athletics Session : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Athletics Session : unemployed 
      charge.name := To_Unbounded_String( "Athletics Session : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Athletics : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Athletics : adult 
      charge.name := To_Unbounded_String( "Athletics : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Athletics : juvenile (per session per group)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Athletics : juvenile (per session per group)
      charge.name := To_Unbounded_String( "Athletics : juvenile (per session per group)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5.6;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Athletics : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Athletics : senior citizen 
      charge.name := To_Unbounded_String( "Athletics : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5.6;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Athletics : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Athletics : unemployed 
      charge.name := To_Unbounded_String( "Athletics : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5.6;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Separate Use of Changing : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Separate Use of Changing : adult 
      charge.name := To_Unbounded_String( "Separate Use of Changing : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.95;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Separate Use of Changing : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Separate Use of Changing : juvenile 
      charge.name := To_Unbounded_String( "Separate Use of Changing : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.95;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Separate Use of Changing : senior citizen (per session)" );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Separate Use of Changing : senior citizen (per session)
      charge.name := To_Unbounded_String( "Separate Use of Changing : senior citizen (per session)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.95;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Separate Use of Changing : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Separate Use of Changing : unemployed 
      charge.name := To_Unbounded_String( "Separate Use of Changing : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.95;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Dry Ski Slope Session : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Dry Ski Slope Session : adult 
      charge.name := To_Unbounded_String( "Dry Ski Slope Session : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.15;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Dry Ski Slope Session : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Dry Ski Slope Session : juvenile (per person)
      charge.name := To_Unbounded_String( "Dry Ski Slope Session : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.2;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing Permit (from Bank) : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Fishing Permit (from Bank) : adult 
      charge.name := To_Unbounded_String( "Fishing Permit (from Bank) : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.6;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing Permit (from Bank) : juvenile (per person per day)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Fishing Permit (from Bank) : juvenile (per person per day)
      charge.name := To_Unbounded_String( "Fishing Permit (from Bank) : juvenile (per person per day)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing Permit (from Bank) : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Fishing Permit (from Bank) : senior citizen 
      charge.name := To_Unbounded_String( "Fishing Permit (from Bank) : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing Permit (from Bank) : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Fishing Permit (from Bank) : unemployed 
      charge.name := To_Unbounded_String( "Fishing Permit (from Bank) : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing Permit (from Bank) :  " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Fishing Permit (from Bank) :  
      charge.name := To_Unbounded_String( "Fishing Permit (from Bank) :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing Permit (from Boat) : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Fishing Permit (from Boat) : adult 
      charge.name := To_Unbounded_String( "Fishing Permit (from Boat) : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.1;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing Permit (from Boat) : juvenile (per person per day)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Fishing Permit (from Boat) : juvenile (per person per day)
      charge.name := To_Unbounded_String( "Fishing Permit (from Boat) : juvenile (per person per day)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing Permit (from Boat) : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Fishing Permit (from Boat) : senior citizen 
      charge.name := To_Unbounded_String( "Fishing Permit (from Boat) : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing Permit (from Boat) : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Fishing Permit (from Boat) : unemployed 
      charge.name := To_Unbounded_String( "Fishing Permit (from Boat) : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 4.1;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing - Annual Ticket : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Fishing - Annual Ticket : adult 
      charge.name := To_Unbounded_String( "Fishing - Annual Ticket : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 20.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing - Annual Ticket : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Fishing - Annual Ticket : juvenile (per person)
      charge.name := To_Unbounded_String( "Fishing - Annual Ticket : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing - Annual Ticket : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Fishing - Annual Ticket : senior citizen 
      charge.name := To_Unbounded_String( "Fishing - Annual Ticket : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Fishing - Annual Ticket : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Fishing - Annual Ticket : unemployed 
      charge.name := To_Unbounded_String( "Fishing - Annual Ticket : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 13.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Equestrian Centres : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Equestrian Centres : adult 
      charge.name := To_Unbounded_String( "Equestrian Centres : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 13.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Equestrian Centres : juvenile (per person per session)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Equestrian Centres : juvenile (per person per session)
      charge.name := To_Unbounded_String( "Equestrian Centres : juvenile (per person per session)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 11.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Windsurfing : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Windsurfing : adult 
      charge.name := To_Unbounded_String( "Windsurfing : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.4;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Windsurfing : juvenile (per person per session)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Windsurfing : juvenile (per person per session)
      charge.name := To_Unbounded_String( "Windsurfing : juvenile (per person per session)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.9;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Canoeing : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Canoeing : adult 
      charge.name := To_Unbounded_String( "Canoeing : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.3;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Canoeing : juvenile (per person per session)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Canoeing : juvenile (per person per session)
      charge.name := To_Unbounded_String( "Canoeing : juvenile (per person per session)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.8;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Canoeing : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Canoeing : unemployed 
      charge.name := To_Unbounded_String( "Canoeing : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.65;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sailing : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Sailing : adult 
      charge.name := To_Unbounded_String( "Sailing : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.4;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sailing : juvenile (per person per session)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Sailing : juvenile (per person per session)
      charge.name := To_Unbounded_String( "Sailing : juvenile (per person per session)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.9;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Boating Session : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Boating Session : adult 
      charge.name := To_Unbounded_String( "Boating Session : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Boating Session : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Boating Session : juvenile (per person)
      charge.name := To_Unbounded_String( "Boating Session : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.8;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Boating Session : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Boating Session : senior citizen 
      charge.name := To_Unbounded_String( "Boating Session : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.8;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Boating Session : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Boating Session : unemployed 
      charge.name := To_Unbounded_String( "Boating Session : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.8;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Boating Session :  " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Boating Session :  
      charge.name := To_Unbounded_String( "Boating Session :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Session : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Swimming Session : adult 
      charge.name := To_Unbounded_String( "Swimming Session : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.9;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Session : juvenile (per person per hour)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Swimming Session : juvenile (per person per hour)
      charge.name := To_Unbounded_String( "Swimming Session : juvenile (per person per hour)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Session : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Swimming Session : senior citizen 
      charge.name := To_Unbounded_String( "Swimming Session : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Session : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Swimming Session : unemployed 
      charge.name := To_Unbounded_String( "Swimming Session : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Leisure Pool : adult 
      charge.name := To_Unbounded_String( "Leisure Pool : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.1;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Leisure Pool : juvenile 
      charge.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Leisure Pool : senior citizen 
      charge.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Leisure Pool : unemployed 
      charge.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.85;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Lesson : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Swimming Lesson : adult 
      charge.name := To_Unbounded_String( "Swimming Lesson : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Lesson : juvenile (per person)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Swimming Lesson : juvenile (per person)
      charge.name := To_Unbounded_String( "Swimming Lesson : juvenile (per person)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.85;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Lesson : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Swimming Lesson : senior citizen 
      charge.name := To_Unbounded_String( "Swimming Lesson : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Lesson : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Swimming Lesson : unemployed 
      charge.name := To_Unbounded_String( "Swimming Lesson : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Leisure Pool : adult 
      charge.name := To_Unbounded_String( "Leisure Pool : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Leisure Pool : juvenile 
      charge.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Leisure Pool : senior citizen 
      charge.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Leisure Pool : unemployed 
      charge.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Water Aerobics : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Water Aerobics : adult 
      charge.name := To_Unbounded_String( "Water Aerobics : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Water Aerobics : juvenile (per session)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Water Aerobics : juvenile (per session)
      charge.name := To_Unbounded_String( "Water Aerobics : juvenile (per session)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Water Aerobics : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Water Aerobics : senior citizen 
      charge.name := To_Unbounded_String( "Water Aerobics : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Water Aerobics : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Water Aerobics : unemployed 
      charge.name := To_Unbounded_String( "Water Aerobics : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Leisure Pool : adult 
      charge.name := To_Unbounded_String( "Leisure Pool : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.25;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Leisure Pool : juvenile 
      charge.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.75;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Leisure Pool : senior citizen 
      charge.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Leisure Pool : unemployed 
      charge.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.75;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool :  " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Leisure Pool :  
      charge.name := To_Unbounded_String( "Leisure Pool :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Pool Hire : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Swimming Pool Hire : adult 
      charge.name := To_Unbounded_String( "Swimming Pool Hire : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 27.8;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Pool Hire : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Swimming Pool Hire : juvenile 
      charge.name := To_Unbounded_String( "Swimming Pool Hire : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 21.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Pool Hire : senior citizen (per session)" );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Swimming Pool Hire : senior citizen (per session)
      charge.name := To_Unbounded_String( "Swimming Pool Hire : senior citizen (per session)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 21.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Pool Hire : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Swimming Pool Hire : unemployed 
      charge.name := To_Unbounded_String( "Swimming Pool Hire : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 21.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Swimming : adult 
      charge.name := To_Unbounded_String( "Swimming : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 17.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming : juvenile (per person per 10 tickets)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Swimming : juvenile (per person per 10 tickets)
      charge.name := To_Unbounded_String( "Swimming : juvenile (per person per 10 tickets)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8.6;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Swimming : senior citizen 
      charge.name := To_Unbounded_String( "Swimming : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Swimming : unemployed 
      charge.name := To_Unbounded_String( "Swimming : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 8.6;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Leisure Pool : adult 
      charge.name := To_Unbounded_String( "Leisure Pool : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 18.9;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Leisure Pool : juvenile 
      charge.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 9.45;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Leisure Pool : senior citizen 
      charge.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Leisure Pool : unemployed 
      charge.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10.8;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Season Ticket : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Swimming Season Ticket : adult 
      charge.name := To_Unbounded_String( "Swimming Season Ticket : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 106.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Season Ticket : juvenile (per person per year)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Swimming Season Ticket : juvenile (per person per year)
      charge.name := To_Unbounded_String( "Swimming Season Ticket : juvenile (per person per year)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 53.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Season Ticket : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Swimming Season Ticket : senior citizen 
      charge.name := To_Unbounded_String( "Swimming Season Ticket : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Season Ticket : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Swimming Season Ticket : unemployed 
      charge.name := To_Unbounded_String( "Swimming Season Ticket : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 53.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Leisure Pool : adult 
      charge.name := To_Unbounded_String( "Leisure Pool : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 106.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Leisure Pool : juvenile 
      charge.name := To_Unbounded_String( "Leisure Pool : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 53.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Leisure Pool : senior citizen 
      charge.name := To_Unbounded_String( "Leisure Pool : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Leisure Pool : unemployed 
      charge.name := To_Unbounded_String( "Leisure Pool : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 53.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Spectator : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Swimming Spectator : adult 
      charge.name := To_Unbounded_String( "Swimming Spectator : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.1;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Spectator : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Swimming Spectator : juvenile 
      charge.name := To_Unbounded_String( "Swimming Spectator : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Spectator : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Swimming Spectator : senior citizen 
      charge.name := To_Unbounded_String( "Swimming Spectator : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming Spectator : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Swimming Spectator : unemployed 
      charge.name := To_Unbounded_String( "Swimming Spectator : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming if Flume Included : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Swimming if Flume Included : adult 
      charge.name := To_Unbounded_String( "Swimming if Flume Included : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.1;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming if Flume Included : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Swimming if Flume Included : juvenile 
      charge.name := To_Unbounded_String( "Swimming if Flume Included : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Swimming if Flume Included : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Swimming if Flume Included : unemployed 
      charge.name := To_Unbounded_String( "Swimming if Flume Included : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.85;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Flume : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Flume : adult 
      charge.name := To_Unbounded_String( "Flume : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.15;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Flume : juvenile " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Flume : juvenile 
      charge.name := To_Unbounded_String( "Flume : juvenile " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.15;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Flume :  " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Flume :  
      charge.name := To_Unbounded_String( "Flume :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Flume :  " );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Flume :  
      charge.name := To_Unbounded_String( "Flume :  " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sauna Session : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Sauna Session : adult 
      charge.name := To_Unbounded_String( "Sauna Session : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sauna Session : juvenile (per person per hour)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Sauna Session : juvenile (per person per hour)
      charge.name := To_Unbounded_String( "Sauna Session : juvenile (per person per hour)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.75;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sauna Session : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Sauna Session : senior citizen 
      charge.name := To_Unbounded_String( "Sauna Session : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sauna Session : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Sauna Session : unemployed 
      charge.name := To_Unbounded_String( "Sauna Session : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Turkish/Steam Bath : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Turkish/Steam Bath : adult 
      charge.name := To_Unbounded_String( "Turkish/Steam Bath : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Turkish/Steam Bath : juvenile (per person per session)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Turkish/Steam Bath : juvenile (per person per session)
      charge.name := To_Unbounded_String( "Turkish/Steam Bath : juvenile (per person per session)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.3;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Turkish/Steam Bath : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Turkish/Steam Bath : senior citizen 
      charge.name := To_Unbounded_String( "Turkish/Steam Bath : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Turkish/Steam Bath : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Turkish/Steam Bath : unemployed 
      charge.name := To_Unbounded_String( "Turkish/Steam Bath : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 1.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sunbed Session : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Sunbed Session : adult 
      charge.name := To_Unbounded_String( "Sunbed Session : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.1;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sunbed Session : juvenile (per person per 30 minutes)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Sunbed Session : juvenile (per person per 30 minutes)
      charge.name := To_Unbounded_String( "Sunbed Session : juvenile (per person per 30 minutes)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.1;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sunbed Session : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Sunbed Session : senior citizen 
      charge.name := To_Unbounded_String( "Sunbed Session : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3.1;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sunbed Session : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Sunbed Session : unemployed 
      charge.name := To_Unbounded_String( "Sunbed Session : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 0.5;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sunbed: Fast Tan/High Power : adult " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;

            -- Sunbed: Fast Tan/High Power : adult 
      charge.name := To_Unbounded_String( "Sunbed: Fast Tan/High Power : adult " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sunbed: Fast Tan/High Power : juvenile (per person per 30 minutes)" );
      target.min_age := 8;
      target.max_age := 15;
      target.isExclusive := True;

            -- Sunbed: Fast Tan/High Power : juvenile (per person per 30 minutes)
      charge.name := To_Unbounded_String( "Sunbed: Fast Tan/High Power : juvenile (per person per 30 minutes)" );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 5.2;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sunbed: Fast Tan/High Power : senior citizen " );
      target.min_age := 60;
      target.max_age := Adult_Age'Last;
      target.isExclusive := True;

            -- Sunbed: Fast Tan/High Power : senior citizen 
      charge.name := To_Unbounded_String( "Sunbed: Fast Tan/High Power : senior citizen " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );


      target.name := To_Unbounded_String( "Sunbed: Fast Tan/High Power : unemployed " );
      target.min_age := 16;
      target.max_age := 60;
      target.isExclusive := True;
      ep.Include( target.employment, unemployed );
      ep.Include( target.employment, permanently_sick_disabled );

            -- Sunbed: Fast Tan/High Power : unemployed 
      charge.name := To_Unbounded_String( "Sunbed: Fast Tan/High Power : unemployed " );
      charge.period := weekly;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 2.0;

      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      return regime;   
   end Constuct_Sports_Cheap;
   
   function Construct_Golf_Club return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Golf Club" );
      ap.Append( regime.applications, Construct_Golf_Club_All_People );
      ap.Append( regime.applications, Construct_Golf_Club_One_Adult_One_Child );
      return regime;
   end Construct_Golf_Club;
   

   function Construct_Kindergarden_Expensive return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "High Cost" );
      app.description := To_Unbounded_String( "Nursery Example; most expensive package; all children eligible, most after- and pre- school options taken up." );
      app.max_children := Child_Range'Last;
      
      target.name := target.name & "Primary";
      target.min_age := 4;
      target.max_age := 8;
      -- primary schools
      charge.name := charge.name & "5 Days";
      charge.period := termly;
      charge.frequency := 1.0; -- every term
      charge.charge_amount := 1_452.0;
      cp.Append( target.charges, charge );      
      tp.Append( app.targets, target );
      -- roseblooms - 
      target.name := To_Unbounded_String("Roseblooms");
      target.min_age := 4;
      target.max_age := 4;
      
      charge.name := To_Unbounded_String("5 Days All Day");
      charge.period := termly;
      charge.frequency := 1.0; -- every term
      charge.charge_amount := 900.0;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge );
      
      tp.Append( app.targets, target );
      -- rosebuds - 
      target.name := To_Unbounded_String("Rosebuds");
      target.min_age := 2;
      target.max_age := 3; -- could be 3 avoid overlap
      charge.name := To_Unbounded_String("5 Days All Day");
      charge.period := termly;
      charge.frequency := 1.0; -- 3 terms 
      charge.charge_amount := 1_180.0;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge );
      tp.Append( app.targets, target );
      --
      -- all age groups extended school day
      -- 
      target.name := To_Unbounded_String("Extended School Day; Care Activities");
      target.min_age := 2;
      target.max_age := 8;
      charge.name := To_Unbounded_String("8am-9:15am");
      charge.frequency := 1.0; -- every school day 
      charge.period := school_day;
      charge.charge_amount := 3.50;
      
      cp.Clear( target.charges );
      cp.Append( target.charges, charge );
      charge.name := To_Unbounded_String("12:30am-15:15pm: lunch play");
      charge.charge_amount := 10.0;
      cp.Append( target.charges, charge );
      charge.name := To_Unbounded_String("15:15am-18:00pm: Light Supper and Activities");
      charge.charge_amount := 7.50;
      cp.Append( target.charges, charge );
      tp.Append( app.targets, target );      
      return app;
   end Construct_Kindergarden_Expensive;

   function Construct_Kindergarden_Cheap return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "Low Cost" );
      app.description := To_Unbounded_String( "Nursery Example; most expensive package; at most one child attends, no after- and pre- school options taken up." );
      app.max_children := 1;
      
      target.name := target.name & "Primary; for 5yos and above";
      target.min_age := 6;
      target.max_age := 8;
      -- primary schools
      charge.name := charge.name & "5 Days";
      charge.period := termly;
      charge.frequency := 1.0; -- every term
      charge.charge_amount := 1_452.0;
      cp.Append( target.charges, charge );      
      tp.Append( app.targets, target );
      -- kindergarden 4-5 years
      target.name := To_Unbounded_String("Kindergarden");
      target.min_age := 4;
      target.max_age := 5;
      
      charge.name := To_Unbounded_String("5 Days Morning Only");
      charge.period := termly;
      charge.frequency := 1.0; -- every term
      charge.charge_amount := 435.0;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge );      
      tp.Append( app.targets, target );
      
      -- roseblooms morning only- 
      
      target.name := To_Unbounded_String("Roseblooms");
      target.min_age := 3;
      target.max_age := 3;
      
      charge.name := To_Unbounded_String("5 Days Morning Only");
      charge.period := termly;
      charge.frequency := 1.0; -- every term
      charge.charge_amount := 435.0;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge );
      
      tp.Append( app.targets, target );
      -- rosebuds - 
      target.name := To_Unbounded_String("Roseblooms Morning Only 2 yos only");
      target.min_age := 2;
      target.max_age := 2; -- could be 3 avoid overlap
      charge.name := To_Unbounded_String("5 Days All Day");
      charge.period := termly;
      charge.frequency := 1.0; -- 3 terms 
      charge.charge_amount := 1_180.0;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge );
      tp.Append( app.targets, target );

      return app;
      
   end Construct_Kindergarden_Cheap;

   function Construct_Kindergarden return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Kindergarden Private School" );
      ap.Append( regime.applications, Construct_Kindergarden_Expensive );
      ap.Append( regime.applications, Construct_Kindergarden_Cheap );
      return regime;
   end Construct_Kindergarden;
   
      --
   -- Elgin_B_51
   --

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



   function Construct_Elgin_B_51 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Elgin_B_51" );
      ap.Append( regime.applications, Construct_Elgin_B_51_Low_Cost );
      ap.Append( regime.applications, Construct_Elgin_B_51_High_Cost );
      return regime;
   end Construct_Elgin_B_51;
      

end Charging_Examples;
