with Ada.Text_IO;
with Ada.Characters.Handling; 
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings;

with AWS.URL;

with Web_Utils;
with Model.Web_Constants;
with Google_Chart_Constants;
with Templates_Parser;

with Text_Utils;
with T_Utils.Google_Chart_Generator;

package body Model.Income_Measures.Breakdown_Package is

    use type Templates_Parser.Vector_Tag;
    package ubstr renames Ada.Strings.Unbounded;
    use Web_Utils;
    use Ada.Text_IO;
    
    package Slots_Google_Charts is new Slots_Package.Google_Chart_Generator;
    
    function Make_Google_Chart( 
      title        : String;
      subtitle     : String;
      measure      : Affordability_Measure_Type;
      data         : Basic_Breakdown;
      ctype        : Chart_Types;
      printer      : Slots_Package.Pretty_Print_Item_Access_T;
      is_thumbnail : Boolean ) return Unbounded_String is
    use Google_Chart_Constants;
    use Text_Utils;
      google_type : Google_Chart_Constants.Chart_Type;
      url : Unbounded_String;
      utitle : Unbounded_String := TuS( title );
    begin
      if( subtitle /= "" ) then
         utitle := utitle & "|" & subtitle;
      end if;
      case ctype is
         when bar => google_type := horizontal_bar;
         when pie => google_type := pie;
         when radar => google_type := radar;
      end case;
            
      url := Slots_Google_Charts.Make_Univariate_Chart(
         TS( utitle ),
         data,
         printer,
         google_type,
         is_thumbnail );
      return url;
    end Make_Google_Chart;
    
   
   function Slot_Range_Str_Text_Version( measure : Affordability_Measure_Type; slot : Slot_Range ) return String is
   begin
      if( measure = residual_income_level ) then
         case slot is
            when 1 => return "less than -5,000 ";
            when 2 => return "-5,000 to -1,000.01";
            when 3 => return "-1,000 to 0.01";
            when 4 => return "0 to 999.99";
            when 5 => return "1,000 to 99,999.99";
            when 6 => return "10,000 and above";
         end case;
      else
         case slot is
            when 1 => return "less than 1%";
            when 2 => return "1% to 9.99%";
            when 3 => return "10% to 33.33%";
            when 4 => return "33.34% to 49.99%";
            when 5 => return "50% to 99.99%";
            when 6 => return "100% and over";
         end case;
      end if;
   end Slot_Range_Str_Text_Version; 

   function Pct_Slot( m : Amount ) return Slot_Range is
   begin
      if( m < 1.0 ) then
         return 1;
      elsif ( m < 10.0 ) then
         return 2;
      elsif ( m < 33.34 ) then
         return 3;
      elsif( m < 50.0 ) then
         return 4;
      elsif( m < 100.0 ) then
         return 5;
      else
         return 6;
      end if;
   end Pct_Slot;
   
   function Amount_Slot( m : Amount ) return Slot_Range is
   begin
      if( m < -5_000.0 ) then
         return 1;
      elsif ( m < -1_000.0 ) then
         return 2;
      elsif ( m < 0.0 ) then
         return 3;
      elsif( m < 1_000.0 ) then
         return 4;
      elsif( m < 10_000.0 ) then
         return 5;
      else
         return 6;
      end if;
      
   end Amount_Slot;

   procedure Increment( breakdown : Breakdown_Var; 
                        table : in out Table_Rec; 
                        output : One_Complete_Income_Measure_Output;
                        run_settings    : settings.Settings_Rec ) is
      slot : Slot_Range;
      cost_pos : Costs_Range;
   begin            
      measures: for measure in Affordability_Measure_Type loop
         if(( run_settings.inc_ctl.exclude_is_cases and output.on_income_support ) or 
           ( run_settings.inc_ctl.exclude_net_incomes_below_poverty_line and (output.incomes( net ) < output.poverty_line ))) then
            cost_pos := 0;
            if( measure = residual_income_level ) then
               slot := 1;
            else
               slot := 6;
            end if;
            if( not run_settings.inc_ctl.include_only_non_zero_costs ) or ( output.costs( 1 ) > 0.0 ) then
               table.cells( breakdown )( measure, cost_pos )( slot ) := table.cells( breakdown )( measure, cost_pos )( slot ) + output.grossing_factor;
               table.row_totals( measure, cost_pos )( slot ) := table.row_totals( measure, cost_pos )( slot ) + output.grossing_factor;
               table.col_totals( breakdown )( measure, cost_pos ) := table.col_totals( breakdown )( measure, cost_pos ) + output.grossing_factor;
               table.overall_totals( measure, cost_pos ) := table.overall_totals( measure, cost_pos ) +  output.grossing_factor;
            end if;
         else 
            costs: for cost_pos in 1 .. output.num_cost_measures loop
               if( measure = residual_income_level ) then
                  slot := Amount_Slot( output.affordabilities( cost_pos )( measure ));
               else
                  slot := Pct_Slot( output.affordabilities( cost_pos )( measure ));
               end if;
               if( not run_settings.inc_ctl.include_only_non_zero_costs ) or ( output.costs( cost_pos ) > 0.0 ) then
                  table.cells( breakdown )( measure, cost_pos )( slot ) := table.cells( breakdown )( measure, cost_pos )( slot ) + output.grossing_factor;
                  table.row_totals( measure, cost_pos )( slot ) := table.row_totals( measure, cost_pos )( slot ) + output.grossing_factor;
                  table.col_totals( breakdown )( measure, cost_pos ) := table.col_totals( breakdown )( measure, cost_pos ) + output.grossing_factor;
                  table.overall_totals( measure, cost_pos ) := table.overall_totals( measure, cost_pos ) +  output.grossing_factor;
               end if;
            end loop costs;
         end if;
      end loop measures;
   end Increment;

   subtype Income_Measures_TABLE_SIZE is Integer range 1 .. 40;

    function Make_Chart_File_Name( 
         blockname : String; 
         ctype : Chart_Types;
         is_total     : Boolean;
         breakdown    : Breakdown_Var;
         measure      : Affordability_Measure_Type;
         is_thumbnail : Boolean ) return String is
      use Ada.Strings.Unbounded;
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;
      use Ada.Strings;
      use Text_Utils;
      
      s : Unbounded_String;
         
    begin
      s := s & "chart_";
      s := s & Trim(ctype'Img, Both ) & "_";
      if( is_total ) then
         s := s & "totals";
      else            
         s := s & blockname & "_";
         s := s & Trim(breakdown'Img, Both );
      end if;
      s := s & "_" & Trim( measure'Img, Both );
      if( is_thumbnail ) then
         s := s & "_thumb"; 
      end if;
      -- s := s & ".png";
      return To_Lower(To_String( s ));
    end Make_Chart_File_Name;
    
    function Make_RR_Row( cost : Amount; data  : Basic_Breakdown ) return String is
      use ubstr;
      s : Unbounded_String;
    begin
      s := s & "<row><entry>" & Format_With_Commas( cost ) & "</entry>";
      for i in data'Range loop      
         s := s & "<entry>" & Format( data( i )) & "</entry>";
      end loop;
      s := s & "<row>";
      return To_String( s ); 
    end Make_RR_Row;
    
    function Make_RR_Row( measure : Affordability_Measure_Type ) return String is
      use ubstr;
      s : Unbounded_String;
    begin
      s := s & "<row><entry></entry>";
      for i in Slot_Range loop      
         s := s & "<entry>" & Slot_Range_Str_Text_Version( measure, i ) & "</entry>";
      end loop;
      s := s & "</row>";
      return To_String( s ); 
    end Make_RR_Row;
    

   --
   --  FIXME: this needs generalised badly
   --
   function Make_Single_Cals_Table(
         header : String; 
         data  : Basic_Breakdown;
         chart_name : String;
         measure : Affordability_Measure_Type
          ) return ubstr.Unbounded_String is
      use UK_Format_Utils;
      use ubstr;
      use Templates_Parser;
      translations : Translate_Set;
   begin
      Insert( translations, Assoc ("HEADER", header  ));
      Insert( translations, Assoc ( "CHART-NAME", chart_name ));
      if( measure = residual_income_level ) then
         Insert( translations, Assoc ( "INCOME_NAME", "Remaining Income (£s pa)" ));
      else
         Insert( translations, Assoc ( "INCOME_NAME", "Ratio" ));
      end if;
      Insert( translations, Assoc ( "INC-1", Slot_Range_Str_Text_Version( measure, 1 )  ));
      Insert( translations, Assoc ( "INC-2", Slot_Range_Str_Text_Version( measure, 2 )  ));
      Insert( translations, Assoc ( "INC-3", Slot_Range_Str_Text_Version( measure, 3 )  ));
      Insert( translations, Assoc ( "INC-4", Slot_Range_Str_Text_Version( measure, 4 )  ));
      Insert( translations, Assoc ( "INC-5", Slot_Range_Str_Text_Version( measure, 5 )  ));
      Insert( translations, Assoc ( "INC-6", Slot_Range_Str_Text_Version( measure, 6 )  ));
      Insert( translations, Assoc ( "POP-1", Format( data(1)  )));
      Insert( translations, Assoc ( "POP-2", Format( data(2)  )));
      Insert( translations, Assoc ( "POP-3", Format( data(3)  )));
      Insert( translations, Assoc ( "POP-4", Format( data(4)  )));
      Insert( translations, Assoc ( "POP-5", Format( data(5)  )));
      Insert( translations, Assoc ( "POP-6", Format( data(6)  )));
      
      return Web_Utils.Parse_Template( Model.Web_Constants.OSCR_Paths.template_components_path & "single_cals_table", translations );      
   end Make_Single_Cals_Table;
   
   function Make_Docbook_Section(
      chart_directory     : String;
      section_header : String;
      summary_info : One_Summary_Info_Rec;
      html_output_dir : String ) return ubstr.Unbounded_String is
      use UK_Format_Utils;
      use ubstr;
      use Templates_Parser;
      translations : Translate_Set;
      residual_chart_name : constant String := chart_directory & Text_Utils.DIR_SEPARATOR &
         Make_Chart_File_Name( "", bar, True, Breakdown_Var'First, residual_income_level, False );
      disposable_chart_name : constant String := chart_directory & Text_Utils.DIR_SEPARATOR &
         Make_Chart_File_Name( "", bar, True, Breakdown_Var'First, cost_disposable_ratio, False );
   begin
      Insert( translations, Assoc( "HEADER", section_header  ));
      Insert( translations, Assoc( "HTML-OUTPUT-DIR", html_output_dir  ));
      
      Insert( translations, Assoc( "DISPOSABLE-UNAFFORDABLE", Format(summary_info.unaffordable( cost_disposable_ratio ))));      
      Insert( translations, Assoc( "RESIDUAL-UNAFFORDABLE", Format( summary_info.unaffordable( residual_income_level ))));
      Insert( translations, Assoc( "DISPOSABLE-TABLE", 
         Make_Single_Cals_Table(
             Affordability_Measure_Type_Name( cost_disposable_ratio ),            
             summary_info.graph_data( cost_disposable_ratio ),
             disposable_chart_name,
             cost_disposable_ratio
            )));
      Insert( translations, Assoc( "RESIDUAL-TABLE", 
         Make_Single_Cals_Table(
             Affordability_Measure_Type_Name( residual_income_level ),
             summary_info.graph_data( residual_income_level ),
             residual_chart_name,
             residual_income_level
         )));
      return Web_Utils.Parse_Template( Model.Web_Constants.OSCR_Paths.template_components_path & "single_docbook_report", translations );
   end Make_Docbook_Section;
   
   
   function Make_Single_Table(
         root  : String;
         regime_text : String;
         application_text : String;
         regime_id : String;
         application_id : String;
         header : String; 
         id : String; 
         summary_infos : One_Summary_Info_Rec; 
         breakdown    : Breakdown_Var;
         is_total : Boolean ) return ubstr.Unbounded_String is
      use UK_Format_Utils;
      use ubstr;
      use Templates_Parser;
      use Text_Utils;
      translations : Translate_Set;  
      chart_num : Positive := 1;
      c         : Positive := 1;
      chartstr  : Unbounded_String;
   begin
      Insert( translations,
        Templates_Parser.Assoc( "ROOT", root ));
      Insert( translations,
        Templates_Parser.Assoc ("APPLICATION-TEXT", AWS.URL.Encode( application_text )));
      Insert( translations,
        Templates_Parser.Assoc ("REGIME-TEXT", AWS.URL.Encode( regime_text )));
      Insert( translations,
        Templates_Parser.Assoc ("RESIDUAL-UNAFFORDABLE", Format( summary_infos.unaffordable( residual_income_level ))));
      Insert( translations,
        Templates_Parser.Assoc ("HEADER", header  ));
      Insert( translations,
        Templates_Parser.Assoc ( "DISPOSABLE-UNAFFORDABLE", Format(summary_infos.unaffordable( cost_disposable_ratio ))));
      Insert( translations,
        Templates_Parser.Assoc ( "GROSS-UNAFFORDABLE", Format(summary_infos.unaffordable( cost_gross_ratio ))));
      Insert( translations, Templates_Parser.Assoc ( "RUN-DIR", id )); 
      Insert( translations, Templates_Parser.Assoc ( "REGIME", regime_id ));   
      Insert( translations, Templates_Parser.Assoc ( "APPLICATION", application_id ));   
      Insert( translations, Templates_Parser.Assoc ( "POPULATION", Format_With_Commas( summary_infos.population, False )));
      for measure in Affordability_Measure_Type loop
        for ctype in Chart_Types loop
            Put_Line( "making chart " & measure'Img & " : " & ctype'Img );
            if( measure = residual_income_level ) then
               chartstr := Make_Google_Chart( 
                     header, 
                     Affordability_Measure_Type_Name( measure ), 
                     measure, 
                     summary_infos.graph_data( measure ), 
                     ctype,
                     Slot_Range_Str_Residual_Income'Access,
                     False ); 
            else
               chartstr := Make_Google_Chart( 
                     header, 
                     Affordability_Measure_Type_Name( measure ), 
                     measure, 
                     summary_infos.graph_data( measure ), 
                     ctype,
                     Slot_Range_Str_Ratio'Access,
                     False ); 
            end if;
            Insert(translations,
              Templates_Parser.Assoc( "CHART-FULL-" & Format(c), AWS.URL.Encode( TS( chartstr )))); 
               -- Make_Chart_File_Name( id, ctype, is_total, breakdown, measure, True )));
            Insert(translations,
              Templates_Parser.Assoc( "CHART-" & Format(c), 
               Make_Google_Chart( 
                     header, 
                     "", 
                     measure, 
                     summary_infos.graph_data( measure ), 
                     ctype, 
                     Slot_Range_Str_Residual_Income'Access,
                     True ))); 
              --Make_Chart_File_Name( id, ctype, is_total, breakdown, measure, False )));
              if( is_total ) then
                 Insert(translations, 
                    Templates_Parser.Assoc( "ALT-" & Format(c), 
                       Chart_Types'Image(ctype) & " chart of " & Affordability_Measure_Type_Name( measure )));
              else
                 Insert(translations,
                    Templates_Parser.Assoc( "ALT-" & Format(c), 
                       Chart_Types'Image(ctype) & " chart of " & Affordability_Measure_Type_Name( measure ) & " by " & Get_Breakdown_Description( breakdown )));
              end if;
              c := c + 1;
        end loop;
      end loop;
      Put_Line( "Make_Single_Table exiting OK" );
      return Web_Utils.Parse_Template( Model.Web_Constants.OSCR_Paths.template_components_path & "single_table", translations );
   end Make_Single_Table;

   function Create_Complete_Block(
      root : String;
      regime_text, application_text, 
      regime_id, application_id, section_title : String; id : String; 
      summary_infos : Summary_Info_Array ) return Unbounded_String is
      use ubstr;
      use Templates_Parser;
      use type Templates_Parser.Vector_Tag;
      s1 : Unbounded_String;
      s2 : Unbounded_String;
      translations :  Translate_Set;
      tables : Templates_Parser.Vector_Tag; 
   begin
      for b in Breakdown_Var loop
         if( b > Breakdown_Var'First ) then
            s2 := Make_Single_Table( 
               root,
               regime_text, 
               application_text, 
               regime_id, 
               application_id, 
               Get_Breakdown_Description( b ), 
               id, 
               summary_infos( b ), 
               b, 
               False ); 
            tables := tables & To_String( s2 );
         end if;            
      end loop;
      Insert(translations, Templates_Parser.Assoc ("ROOT", root ));      
      Insert(translations, Templates_Parser.Assoc ("DISAGREGGATED_TABLES", tables ));
      Insert(translations, Templates_Parser.Assoc ("TITLE", section_title ));
      Insert(translations, Templates_Parser.Assoc ( "ID", id ));
      Insert( translations, Templates_Parser.Assoc ( "REGIME", regime_id ));   
      Insert( translations, Templates_Parser.Assoc ( "APPLICATION", application_id ));   
      return  Web_Utils.Parse_Template( Model.Web_Constants.OSCR_Paths.template_components_path & "disagreggated", translations );
   end Create_Complete_Block;

   
   function Make_Summary_Data( table : Table_Rec; which_charge : Costs_Range; which_totals_slot : Slot_Range ) return Summary_Info_Rec is
      sinfo : Summary_Info_Rec;
   begin
   
      sinfo.totals.unaffordable( residual_income_level ) := Get_Percentage_Below_Poverty_Line( 
         table,  
         which_charge );
         
      
      sinfo.totals.unaffordable( cost_disposable_ratio ) := Get_Baseline_Unaffordable( 
         table,  
         which_charge,
         cost_disposable_ratio, which_totals_slot  ); 
         
      sinfo.totals.unaffordable( cost_gross_ratio ) := Get_Baseline_Unaffordable( 
         table,  
         which_charge,
         cost_gross_ratio, which_totals_slot );
         
      for measure in Affordability_Measure_Type loop
         sinfo.totals.graph_data( measure ) := Slots_Package.To_Percent( table.row_totals( measure, which_charge ));
      end loop;
      
      sinfo.totals.population := Slots_Package.Sum(table.row_totals( Affordability_Measure_Type'First, which_charge ));
      
      for b in Breakdown_Var loop
         
         sinfo.sm(b).unaffordable( residual_income_level ) := Get_Percentage_Below_Poverty_Line( 
            table,  
            which_charge,
            b,
            False );
         sinfo.sm(b).unaffordable( cost_disposable_ratio ) := Get_Baseline_Unaffordable( 
            table,  
            which_charge,
            cost_disposable_ratio,
            which_totals_slot,
            b,
            False
            ); 
         sinfo.sm(b).unaffordable( cost_gross_ratio ) := Get_Baseline_Unaffordable( 
            table,  
            which_charge,
            cost_gross_ratio,
            which_totals_slot,
            b,
            False );
         
         for measure in Affordability_Measure_Type loop
            sinfo.sm( b ).graph_data( measure ) := Slots_Package.To_Percent( table.cells( b )(measure, which_charge ));
         end loop;
         sinfo.sm(b).population := Slots_Package.Sum(table.cells(b)( Affordability_Measure_Type'First, which_charge ));
            
      end loop;
      return sinfo;
   end Make_Summary_Data;
   
   function Create_One_Chart_Data(
      filename : String; 
      title : String;
      subtitle : String;
      data  : Basic_Breakdown;
      measure : Affordability_Measure_Type; -- residual_income_level, cost_disposable_ratio, cost_gross_ratio
      ctype : Chart_Types;
      is_thumbnail : Boolean ) return String is
      
      use Ada.Strings.Unbounded;
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;
      use Ada.Strings;
      use Text_Utils;
      
      s : Unbounded_String;
      num_series : Positive := 1;
   begin
      -- if( ctype /= pie ) then
         -- num_series := 2;
      -- end if;
      s := s & filename & LINE_BREAK;
      s := s & title & LINE_BREAK;
      s := s & LINE_BREAK;
      s := s & "(c) OSCR/Virtual Worlds 2008" & LINE_BREAK;
      if( ctype = pie ) then
          s := s & LINE_BREAK & LINE_BREAK;
      else
         if( measure = residual_income_level ) then
            s := s & "free income in &pounds; p.a." & LINE_BREAK;               
         else
            s := s & "proportion of income used by charge" & LINE_BREAK;
         end if;
         s := s & "% of affected families" & LINE_BREAK;
      end if;
      s := s & "-1" &  LINE_BREAK;
      s := s & "-1" &  LINE_BREAK;
      s := s & To_Lower(Chart_Types'Image( ctype )) & LINE_BREAK;
      s := s & "1" & LINE_BREAK;
      if( is_thumbnail ) then
         s := s & "1" & LINE_BREAK;
      else
         s := s & "0" & LINE_BREAK;
      end if;
      s := s & "pre" & LINE_BREAK;
      s := s & "1" & LINE_BREAK; -- num data series
      for i in 1 .. num_series loop
         if( i = 1 ) then
            s := s & "pre" & LINE_BREAK;
         else
            s := s & "post" & LINE_BREAK;
         end if;
         s := s & "#b53a30"& LINE_BREAK;
         for i in data'Range loop
            s := s & Trim( data( i )'Img, Both );
            if( i /= data'Last ) then 
               s := s & ",";
            end if;
         end loop;
         s := s & LINE_BREAK;
      end loop;
      s := s & data'Last'Img & LINE_BREAK;
      for i in data'Range loop
         --if( is_thumbnail ) then
         --   s := s & LINE_BREAK;
         --else
         s := s & Slot_Range_Str( measure, i ) & LINE_BREAK;
         --end if;
      end loop;
      return To_String( s );
   end Create_One_Chart_Data;
   
    
   function Write_Chart_Block( 
      directory  : String;
      block_name : String;
      sinfo : Summary_Info_Rec ) return String is
      use Ada.Strings.Unbounded;
      s : Unbounded_String;
      
   begin
      --  put_line( "Write_Chart_Block; writing to " & directory & " block " & block_name );
      for measure in Affordability_Measure_Type loop
         for ctype in Chart_Types loop
            -- fixme Unix file separator!
            s := s & Create_One_Chart_Data( 
               directory & "/" & Make_Chart_File_Name( block_name, ctype, True, Breakdown_Var'First, measure, False ),
               Affordability_Measure_Type_Name( measure ) & " : Whole Population", 
               Affordability_Units_Name( measure ), 
               sinfo.totals.graph_data( measure ),
               measure,
               ctype,
               False );
            s := s & Create_One_Chart_Data( 
               directory & "/" & Make_Chart_File_Name( block_name, ctype, True, Breakdown_Var'First, measure, True ),
               Affordability_Measure_Type_Name( measure ) & " : Whole Population", 
               Affordability_Units_Name( measure ), 
               sinfo.totals.graph_data( measure ),
               measure,
               ctype,
               True );
         end loop;
      end loop;
      for bv in Breakdown_Var loop
         if( bv > Breakdown_Var'First ) then -- skip null or missing which is always 1st
            for measure in Affordability_Measure_Type loop
               for ctype in Chart_Types loop
                  -- Put_Line( "1st item for " & ctype'Img & " : " & measure'Img & " = " &
                  --         format(sinfo.sm( bv ).graph_data( measure )( 1 )));
                  s := s & Create_One_Chart_Data( 
                     directory & "/" & Make_Chart_File_Name( block_name, ctype, False, bv, measure, False ),
                     Affordability_Measure_Type_Name( measure ) & " : " & Get_Breakdown_Description( bv ), 
                     Affordability_Units_Name( measure ), 
                     sinfo.sm( bv ).graph_data( measure ),
                     measure,
                     ctype,
                     False );
                  s := s & Create_One_Chart_Data( 
                     directory & "/" & Make_Chart_File_Name( block_name, ctype, False, bv, measure, True ),
                     Affordability_Measure_Type_Name( measure ) & " : " & Get_Breakdown_Description( bv ), 
                     Affordability_Units_Name( measure ), 
                     sinfo.sm( bv ).graph_data( measure ),
                     measure,
                     ctype,
                     True );
               end loop;
            end loop;
         end if;
      end loop;
      return To_String( s );   
   end Write_Chart_Block;
   
   function To_HTML(
                    root : String;
                    regime_text : String;
                    application_text : String;
                    regime_id : String;
                    application_id : String;
                    sinfo : Summary_Info_Rec;
                    section_title : String; 
                    id : String;
                    to_percent   : Boolean := false ) return Unbounded_String is
   begin
      return Create_Complete_Block(
         root,
         regime_text, application_text, 
         regime_id, application_id, section_title, id, sinfo.sm );
   end To_HTML;

   function To_Percent( 
         table : Table_Rec; 
         num_cost_measures : Costs_Range;
         run_settings    : settings.Settings_Rec ) return Table_Rec is
      outt : Table_Rec;
      start_pos : Costs_Range := 1;
   begin
      if( run_settings.inc_ctl.exclude_is_cases or run_settings.inc_ctl.exclude_net_incomes_below_poverty_line ) then
         start_pos := 0;
      end if;
      for measure in Affordability_Measure_Type loop
         for slot in Slot_Range loop
            for bdv in Breakdown_Var loop
               for i in start_pos .. num_cost_measures loop
                  if( table.col_totals( bdv )( measure, i) > 0.0 ) then
                     outt.cells( bdv )( measure, i)( slot ) := 
                        Amount(Rate(100.0) * Rate(table.cells( bdv )( measure, i)( slot ))/Rate( table.col_totals( bdv )( measure, i)));
                  end if;
               end loop;                  
            end loop;
         end loop;
      end loop;
      for measure in Affordability_Measure_Type loop
         for slot in Slot_Range loop
            for i in start_pos .. num_cost_measures loop
               if( table.overall_totals( measure, i) > 0.0 ) then
                  outt.row_totals( measure, i)( slot ) := 
                     Amount(Rate(100.0) * Rate(table.row_totals( measure, i)( slot ))/Rate( table.overall_totals( measure, i)));
               end if;
            end loop;                  
         end loop;
      end loop;
      return outt;
   end To_Percent;

   
   function To_CDA( table : Table_Rec; 
                    num_cost_measures : Costs_Range;
                    run_settings    : settings.Settings_Rec;
                    to_percent : Boolean := false ) return String is
      use Text_Utils;
      use Ada.Strings.Unbounded;
      outs : Unbounded_String := To_Unbounded_String( ",," );
      start_pos : Costs_Range := 1;
      tab_to_use : Table_Rec := table;
   begin
      if( to_percent ) then
         tab_to_use := Breakdown_Package.To_Percent( table, num_cost_measures, run_settings );
      end if;
      if( run_settings.inc_ctl.exclude_is_cases or run_settings.inc_ctl.exclude_net_incomes_below_poverty_line ) then
         start_pos := 0;
      end if;
      --
      -- header lines: the breakdown variables (say, Council House, Private Rented, etc.) if this is a by tenure table)
      --
      for bdv in Breakdown_Var loop
         for i in start_pos .. num_cost_measures loop
            outs := outs & '"' & Get_Breakdown_Description( bdv ) & '"' & ",";
         end loop;
      end loop;
      for i in start_pos .. num_cost_measures loop
         outs := outs & "TOTALS,";
      end loop;
      outs := outs & LINE_BREAK;
      --
      -- Then, on the next line, an identifier for each assumption set (high cost, low cost, etc) for each breakdown
      -- 
      outs := outs & ",,";
      for bdv in Breakdown_Var loop
         for i in start_pos .. num_cost_measures loop
            --
            -- the 0th row, if present, stores the Income Support cases we're otherwise ignoring
            --
            if( i = 0 ) then
               outs := outs & '"' & "Income Support Cases" & '"' & ',';
            else
               outs := outs & "Assumption Set" & i'Img & ",";
            end if;               
         end loop;
      end loop;
      --
      -- Extra assumption breakdowns on the end for the totals.
      --
      for i in start_pos .. num_cost_measures loop
         if( i = 0 ) then
            outs := outs & '"' & "Income Support Cases" & '"' & ',';
         else
            outs := outs & "Assumption Set" & i'Img & ",";
         end if;               
      end loop;      
      outs := outs & LINE_BREAK;
      --
      -- Then, the data, 1 block for each measure (gross income, redisual income ratio..)
      --
      for measure in Affordability_Measure_Type loop
         outs := outs & '"' & Pretty_Print( measure )& '"';
         outs := outs & LINE_BREAK;
         --
         -- Then 1 line for each slot ( 10-20%, or 10,000-20,000 pounds for whatever)
         -- 
         for slot in Slot_Range loop
            outs := outs & '"' & Slot_Range_Str( measure, slot ) & '"' & ",,";
            --
            -- Then the breakdowns in groups of columns. So each tenure type, region, or whatever.
            --
            for bdv in Breakdown_Var loop
               --
               -- Print each affordabilty assumption for each breakdown variable.
               --
               for i in start_pos .. num_cost_measures loop
                  outs := outs & format( tab_to_use.cells( bdv )( measure, i )( slot )) & ",";
               end loop;
            end loop;
            --
            -- Totals counts at the end.
            --
            for i in start_pos .. num_cost_measures loop
               outs := outs & format( tab_to_use.row_totals( measure, i )( slot )) & ",";
            end loop;
            outs := outs & LINE_BREAK;
         end loop;
      end loop;
      return To_String( outs );      
   end To_CDA;
   
   
   function Get_Percentage_Below_Poverty_Line( 
      table : Table_Rec; 
      which_charge :Costs_Range;          
      which_breakdown : Breakdown_Var := Breakdown_Var'First;
      is_total : Boolean := True  ) return Amount is
      denom, num, unafford : Amount := 0.0;
   begin
      if( is_total ) then
         denom := table.overall_totals( residual_income_level, which_charge );
         for slot in Slot_Range loop
            if( slot < 4 ) then
                num := num + table.row_totals( residual_income_level, which_charge )( slot );
            end if;
         end loop;
      else
         denom := table.col_totals( which_breakdown )( residual_income_level, which_charge );
         for slot in Slot_Range loop
            if( slot < 4 ) then
                num := num + table.cells( which_breakdown )( residual_income_level, which_charge )( slot );
            end if;
         end loop;
      end if;
      -- put_line( " num " & Format( num ) & " denom " & format( denom ));
      if( denom > 0.0 ) then
         unafford := ( 100.0 * num)/denom; 
      end if;
      --  put_line( "made unafordable as " & Format( unafford ));
      return unafford;         
   end Get_Percentage_Below_Poverty_Line;
   
   function Get_Baseline_Unaffordable( 
      table : Table_Rec; 
      which_charge :Costs_Range; 
      measure : Affordability_Measure_Type;
      which_totals_slot : Slot_Range;       
      which_breakdown    : Breakdown_Var := Breakdown_Var'First;
      is_total : Boolean := True ) return Amount is 
      use Ada.Text_IO; -- FIXME kill
      num, denom, unafford : Amount := 0.0;
   begin
      if( is_total ) then
         denom := table.overall_totals( measure, which_charge );
         for slot in Slot_Range loop
            if slot > which_totals_slot then
               --  Ada.Text_IO.put_line( " adding " & Format(  table.row_totals( measure, which_charge )( slot ) ) & " slot " & slot'Img & " which_charge " & which_charge'Img );
               num := num + table.row_totals( measure, which_charge )( slot );
            end if;
         end loop;
      else
         denom := table.col_totals( which_breakdown )( measure, which_charge );
         for slot in Slot_Range loop
            if slot > which_totals_slot then
               num := num + table.cells( which_breakdown )( measure, which_charge )( slot );
               --  Ada.Text_IO.put_line( " adding " & Format(  num ) & " slot " & slot'Img & " which_charge " & which_charge'Img );                  
            end if;
         end loop;
      end if;
      --  put_line( " num " & Format( num ) & " denom " & format( denom ));
      if( denom > 0.0 ) then
         unafford := (100.0 * num)/denom; 
      end if;
      --  put_line( "made unafordable as " & Format( unafford ));
      return unafford;
   end Get_Baseline_Unaffordable;


end Model.Income_Measures.Breakdown_Package;
