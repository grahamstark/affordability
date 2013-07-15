   function Make_Submenu( regime : mc.Charging_Regime; this_application : Unbounded_String ) return String is
  
      use Model.Income_Measures;
      use mc;
      
      outs : Unbounded_String;
      application : Application_Type;
      application_name : String := To_Dir_Name( application.name );
      NUM_APPLICATIONS : constant Costs_Range := Costs_Range( ap.Length( regime.applications ));
   begin
      outs := TuS( "<ul id='snavlist'>" );
      Each_Charge:
      for application_no in 1 .. num_applications loop
         declare
             application : Application_Type := ap.Element( regime.applications, application_no );
             application_name : String := To_Dir_Name( application.name );
             regime_name : String := To_Dir_Name( regime.name );                
             relative_dir : String := "/output/?regime=" &  regime_name & "&amp;regime=" & application_name;               
         begin
            if( this_application /= application.name ) then
               outs := outs & "     <li>" & LINE_BREAK;
               outs := outs & "              <a class='navlist' href='" & relative_dir & "'>" & application.name & "</a>" & LINE_BREAK;
               outs := outs & "     </li>" & LINE_BREAK;         
            else
               outs := outs & "     <li>" & LINE_BREAK;
               outs := outs & "              <span class='modelsNav current'>" & application.name & "</span>" & LINE_BREAK;
               outs := outs & "     </li>" & LINE_BREAK;
            end if;
         end;
      end loop Each_Charge;
      outs := outs & "</ul>" & LINE_BREAK;   
      return To_String( outs );
   end Make_Submenu;


