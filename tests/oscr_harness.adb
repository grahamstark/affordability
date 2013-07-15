with OSCR_Suite;

with AUnit.Run;
with AUnit.Reporter.Text;

procedure OSCR_Harness is

   use AUnit.Run;
   use AUnit.Reporter.Text;

   procedure Run is new AUnit.Run.Test_Runner( OSCR_Suite );
   reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Run( reporter );        
end OSCR_Harness;
