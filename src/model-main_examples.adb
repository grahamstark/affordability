with Ada.Strings.Unbounded;
with FRS_Enums;
with Model.Incomes;
with Model.Household;
with Base_Model_Types;
with Ada.Text_IO;

package body Model.Main_Examples is

   use Model.Charging;
   use FRS_Enums;
   use Model.Incomes;
   use Model.Incomes.Incomes_Package.Set_Ops;
   use Model.Household;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   
   package ap renames Model.Charging.Application_Package;
   package tp renames Model.Charging.Target_Package;
   package cp renames Model.Charging.Charges_Package;
   package rp renames Model.Charging.Charging_Regime_Package;
   package ep renames Model.Household.Employment_Package.Set_Ops;
   package sp renames FRS_Enums.Gender_Package.Set_Ops;

   function Construct_RR_Charge( ch : Amount ) return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
      charge_string : constant String := Format_With_Commas( ch, False  );
   begin
      app.name := To_Unbounded_String( charge_string );
      app.description := To_Unbounded_String( "Affordability of a single fixed charge of &pound;" &charge_string&" p.a.." );
      app.max_people := 1;
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "" );
      target.min_age := 1;
      target.max_age := 120;
      target.isExclusive := True;      
      charge.name := To_Unbounded_String( "" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := ch;
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      return app;
    end Construct_RR_Charge;


   function Construct_Ready_Reckoner return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      put_line( "ready reckoner make charges entered " );
      regime.name := To_Unbounded_String( "Ready Reckoner" );
      regime.description := To_Unbounded_String( "A Simple Ready Reckoner applying a single fixed charge to each family in Scotland." );
      for i in CHARGES'First .. CHARGES'Last loop
         ap.Append( regime.applications, Construct_RR_Charge( CHARGES(i) ) );
      end loop;
      put_line( "ready reckoner charges made OK " );
      return regime;
   end Construct_Ready_Reckoner;

 
   --
   -- Edinburgh_4
   --

   function Construct_Edinburgh_4_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "High Cost" );
      app.description := To_Unbounded_String( "High cost assumption: all people in ages 3-18 are eligible." );
      app.max_children := Child_Range'Last;
      app.max_adults := 0;
      app.max_people := Child_Range'Last;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      target.min_age := 3;
      target.max_age := 11;
      target.isExclusive := True;      
            -- Edinburgh : ages 3 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh : ages 3 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 3590.0;
      
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
      charge.charge_amount := 6593.0;
      
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
      app.description := To_Unbounded_String( "Low cost assumption: maximum of 1 person per family is eligible" );
      app.max_adults := 0;
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
      charge.charge_amount := 3590.0;
      
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
      charge.charge_amount := 6593.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_4_Low_Cost;

   function Construct_Edinburgh_4 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Private School 1" );
      regime.description := To_Unbounded_String( "Medium Cost Private High School: Co-ed 3-18 yrs; charging &pound;3,590 for Juniors and 6593.0 for seniors" );
      ap.Append( regime.applications, Construct_Edinburgh_4_Low_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_4_High_Cost );
      return regime;
   end Construct_Edinburgh_4;
   
      --
   -- Edinburgh_B_49
   --

   function Construct_Edinburgh_49_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "High Cost" );
      app.description := To_Unbounded_String( "8-18 yrs Boys : high cost; 22,295 p.a. per 12+ Child boarding; 11,505 per junior. " );
      app.description := app.description & "All eligible children attend, 12+ year-olds board.";
      app.max_children := Child_Range'Last;
      app.max_adults := 0;
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 8 - 11 - high cost" );
      target.min_age := 8;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 5 - 11 - high cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 8 - 11 - high cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10_505.0;
      
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
      charge.charge_amount := 22_295.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_49_High_Cost;

   function Construct_Edinburgh_49_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "Low Cost" );
      app.description := To_Unbounded_String( "8-18 yrs Boys school. Low cost option: maximum of 1 person per household. Non-boarding" );
      app.max_children := 1;
      app.max_people := 1;
      
      cp.Clear( target.charges );
      target.name := To_Unbounded_String( "Edinburgh (B) : ages 8 - 11 - low cost" );
      target.min_age := 8;
      target.max_age := 11;
      target.isExclusive := True;      
      sp.Include( target.genders, male );
            -- Edinburgh (B) : ages 5 - 11 - low cost
      charge.name := To_Unbounded_String( "Edinburgh (B) : ages 5 - 11 - low cost" );
      charge.period := annual;
      charge.frequency := 1.0; -- once per period
      charge.charge_amount := 10_505.0;
      
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
      charge.charge_amount := 16_085.0;
      
      cp.Append( target.charges, charge ); 

      tp.Append( app.targets, target );
      return app;
    end Construct_Edinburgh_49_Low_Cost;

   function Construct_Edinburgh_49 return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Private School 2" );
      regime.description := To_Unbounded_String( "Expensive Private High School: Boys school 8-18 yrs; charging &pound;10,505 for Juniors and ");
      regime.description := regime.description &" &pound;22,295 (boarding) &pound;16,085 (non-boarding) for seniors.";
      ap.Append( regime.applications, Construct_Edinburgh_49_Low_Cost );
      ap.Append( regime.applications, Construct_Edinburgh_49_High_Cost );
      return regime;
   end Construct_Edinburgh_49;


   function Construct_Medical_Charity_Hip_Replacement_60s_Plus return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "Hip replacement" );
      app.description := To_Unbounded_String( "Hip Replacement for all aged 60 and over, &pound;12,350.0 repaid over 5 years at an 8% interest rate." );
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
      app.name := To_Unbounded_String( "Consulations" );
      app.description := To_Unbounded_String( "Consulations at an assumed &pound;120 each, once every 6 months, for all persons in the family." );
      charge.name := charge.name & "Consulations at an assumed &pound;120 each, once every 6 months, for all persons in the family.";
      charge.period := monthly;
      charge.frequency := 6.0; -- once every 3 months
      charge.charge_amount := 120.0;
      cp.Append( target.charges, charge );      
      tp.Append( app.targets, target );
      return app;
   end Construct_Medical_Charity_Diagnostics;
   
   function Construct_Medical_Charity return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Medical" );
      regime.description := To_Unbounded_String( "Medical procedures: hip replacements for 60+, consulations for everyone" );
      ap.Append( regime.applications, Construct_Medical_Charity_Diagnostics );
      ap.Append( regime.applications, Construct_Medical_Charity_Hip_Replacement_60s_Plus );
      return regime;
   end Construct_Medical_Charity;
   
   function Construct_Golf_Club_One_Adult_One_Child return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
   begin
      app.name := To_Unbounded_String( "Max 1 adult 1 child" );
      app.description := To_Unbounded_String( "Low-cost Golf Club membership for up to 1 adult and 1 child per family. " );
      app.description := app.description &
         "Charges &pound;60pa for students, &pound;90 normal; &pound;60 pensioners; &pound;10 juniors. No Green fees. Up to 1 adult and 1 child per family.";
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
      app.name := To_Unbounded_String( "Everyone 8+" );
      app.description := To_Unbounded_String( "Low-cost Golf Club membership for up to 1 adult and 1 child per family. " );
      app.description := app.description &
         "Charges &pound;60pa for students, &pound;90 normal; &pound;60 pensioners; &pound;10 juniors. No Green fees. Up to 1 adult and 1 child per family.";
      
      app.max_children := Child_Range'Last;
      app.max_adults := 2;

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

  function Construct_Golf_Examples return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Golf" );
      regime.description := To_Unbounded_String( "Golf Membership for a relatively low-cost gold club." );
      ap.Append( regime.applications, Construct_Golf_Club_One_Adult_One_Child );
      ap.Append( regime.applications, Construct_Golf_Club_All_People );
      return regime;
   end Construct_Golf_Examples;


   
   -- Example 1: Examples outlines costs for music courses. 
-- 
-- Full time course costs as follows:
-- Year 1 & 2
-- £ 3250.00 per year
-- Year 3
-- £ 2150 for the Session
-- 
-- 18-25 year olds £2,400 pa
-- 
-- Part time courses
-- STARFACTORY INFANT CLASS (1.5 hours per week) 5-7 year olds
-- £480.00 per year
-- STARFACTORY COURSE SINGLE OPTION (3 hours per week) 8-17 year olds
-- f 780.00 per year
-- STARFACTORY ADVANCED & DOUBLE OPTlON COURSE (6hours per week) £ 1260.00 per year 8-17 yo
-- 
-- # age 5-7 845pa   
    function Construct_Star_Factory_High_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
    begin  
      app.name := To_Unbounded_String( "High Cost" );
      app.description := To_Unbounded_String( "All 5-25 year olds eligible. No limit on the number per family.");
      app.description := app.description & "Infant class (1.5 hours per week) for 5-7 year olds &pound;480p.a.. ";
      app.description := app.description & "Double optlon course (6 hours per week) &pound;1,260.00 per year for 8-17 year olds. " ;
      app.description := app.description & "Full time course (year 1 and 2 rate) for all 18-25 year olds at &pound;3,250 per year ";
      app.description := app.description & "(assumed paid immediately and not coverered by student grants or loans).";
      
      app.max_children := Child_Range'Last;
      app.max_adults := 2;
      app.max_people := Child_Range'Last + 2;

      target.name := target.name & "Infant class (1.5 hours per week) 5-7 year olds &pound;480p.a.";
      target.min_age := 5;
      target.max_age := 7;
      target.isExclusive := True;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every year
      charge.charge_amount := 480.0;
      cp.Append( target.charges, charge );
      tp.Append( app.targets, target );

      target.name := target.name & "Starfactory course single option (3 hours per week) 8-17 year olds 780.00 per yea";
      target.min_age := 8;
      target.max_age := 17;
      target.isExclusive := True;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every year
      charge.charge_amount := 1_260.00;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      
      target.name := target.name & "28-25yos";
      target.min_age := 18;
      target.max_age := 25;
      charge.name := charge.name & "Full Time";
      charge.period := annual;
      charge.frequency := 1.0; 
      charge.charge_amount := 3_250.0;      
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      return app;
   end Construct_Star_Factory_High_Cost;

    function Construct_Star_Factory_Low_Cost return Application_Type is
      app    : Application_Type;
      target : Target_Type;
      charge : Charges_Type;
    begin  
      app.name := To_Unbounded_String( "Low Cost" );
      app.description := To_Unbounded_String( "Maximum of <b>one</b> person per family attends. ");
      app.description := app.description & "All 5-25 year olds eligible. Infant class (1.5 hours per week) for 5-7 year olds &pound;480p.a.. ";
      app.description := app.description & "Single option (3 hours per week) for 8-17 year olds &pound;780 per year. " ;
      app.description := app.description & "Full time course (year 1 and 2 rate) for all 18-25 year olds at &pound;3,250 per year ";
      app.description := app.description & "(assumed paid immediately and not coverered by student grants or loans).";
      
      app.max_children := 1;
      app.max_adults := 1;
      app.max_people := 1;

      target.name := target.name & "Infant class (1.5 hours per week) 5-7 year olds &pound;480p.a.";
      target.min_age := 5;
      target.max_age := 7;
      target.isExclusive := True;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every year
      charge.charge_amount := 480.0;
      cp.Append( target.charges, charge );
      tp.Append( app.targets, target );

      target.name := target.name & "Starfactory course single option (3 hours per week) 8-17 year olds 780.00 per yea";
      target.min_age := 8;
      target.max_age := 17;
      target.isExclusive := True;
      charge.name := charge.name & "Standard";
      charge.period := annual;
      charge.frequency := 1.0; -- every year
      charge.charge_amount := 780.00;
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      
      target.name := target.name & "28-25yos";
      target.min_age := 18;
      target.max_age := 25;
      charge.name := charge.name & "Full Time";
      charge.period := annual;
      charge.frequency := 1.0; 
      charge.charge_amount := 3_250.0;      
      cp.Clear( target.charges );
      cp.Append( target.charges, charge ); 
      tp.Append( app.targets, target );
      return app;
   end Construct_Star_Factory_Low_Cost;
   
   -- See http://www.scottishballet.co.uk/whats-on/current-productions/autumn-season-2008/autumn-season-2008.htm
   
   
   

   function Construct_Arts_Examples return Model.Charging.Charging_Regime is
      regime : Charging_Regime;      
   begin
      regime.name := To_Unbounded_String( "Arts Education" );
      regime.description := To_Unbounded_String( "Musical Education Courses" );
      ap.Append( regime.applications, Construct_Star_Factory_Low_Cost );
      ap.Append( regime.applications, Construct_Star_Factory_High_Cost );
      return regime;
   end Construct_Arts_Examples;
   
   
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
      regime.name := To_Unbounded_String( "Private Nursery" );
      regime.description := To_Unbounded_String( "Kindergarden-type Private School" );
      ap.Append( regime.applications, Construct_Kindergarden_Cheap );
      ap.Append( regime.applications, Construct_Kindergarden_Expensive );
      return regime;
   end Construct_Kindergarden;

 
   
   function Make_All_Regimes return Model.Charging.Charging_Regime_List is
      l : Model.Charging.Charging_Regime_List;
   begin
      rp.Append( l, Construct_Ready_Reckoner );
      rp.Append( l, Construct_Edinburgh_4 );
      rp.Append( l, Construct_Edinburgh_49 );
      rp.Append( l, Construct_Medical_Charity );
      rp.Append( l, Construct_Golf_Examples );
      rp.Append( l, Construct_Arts_Examples );
      rp.Append( l, Construct_Kindergarden );
      return l;
   end Make_All_Regimes;   
   
end Model.Main_Examples;
