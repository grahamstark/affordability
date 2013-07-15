with Oscr_Tests;
with AUnit.Test_Suites;

function OSCR_Suite return AUnit.Test_Suites.Access_Test_Suite is

   use AUnit.Test_Suites;
   
   result : Access_Test_Suite := new Test_Suite;
   
begin 
   Add_Test( result, new OSCR_Tests.Test_Case );   
   return result;      
end OSCR_Suite;
