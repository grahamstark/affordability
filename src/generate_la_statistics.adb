--
--  $Author: graham_s $
--  $Date: 2008-11-24 00:25:25 +0000 (Mon, 24 Nov 2008) $
--  $Revision: 6297 $
--
with la_statistics_functions;

procedure generate_la_statistics is
begin

   la_statistics_functions.generate_costs_tests;
   la_statistics_functions.generate_upper_limits;
   la_statistics_functions.generate_model_statistics;
   la_statistics_functions.generate_counts_in_each_state;

end generate_la_statistics;
