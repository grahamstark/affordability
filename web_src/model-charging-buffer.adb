with Ada.Text_IO;
with Ada.Characters.Handling;

with GNAT.String_Split;

with Model.Web_Constants;
with T_Utils.Web_IO;
with T_Utils;
with Templates_Parser;
with Text_Utils;
with Web_Utils;
with Ada.Strings;
with Ada.Strings.Fixed;

package body Model.Charging.Buffer is

   use Text_Utils;   
   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   
   package mwcn renames Model.Web_Constants;
   package ach renames Ada.Characters.Handling;
  
   package Frequency_Package is new T_Utils( Rate_Type=>Rate, Amount_Type=>Amount, Counter_Type=>Counter_Type, T => Frequency_Type );
   
   function Pretty_Print( f : Frequency_Type ) return String is
   begin
      case f is
         when  Ten_Times_Per_Period => return "Ten Times Per Period";
         when  Five_Times_Per_Period => return "Five Times Per Period";
         when  Three_Times_Per_Period => return "Three Times Per Period";
         when  Twice_Times_Per_Period => return "Twice Per Period";
         when  Once_Per_Period => return "Once Per Period";
         when  Once_Every_Two_Periods => return "Once Every Two Periods";
         when  Once_Every_Three_Periods => return "Once Every Three Periods";
         when  Once_Every_Five_Periods => return "Once Every Five Periods";
         when  Once_Every_Ten_Periods => return "Once Every Ten Periods";
      end case;      
   end Pretty_Print;  
   
   function To_Rate( f : Frequency_Type ) return Rate is
   begin
      case f is
         when  Ten_Times_Per_Period => return 0.1;
         when  Five_Times_Per_Period => return 0.2;
         when  Three_Times_Per_Period => return 1.0/3.0;
         when  Twice_Times_Per_Period => return 0.5;
         when  Once_Per_Period => return 1.0;
         when  Once_Every_Two_Periods => return 2.0;
         when  Once_Every_Three_Periods => return 3.0;
         when  Once_Every_Five_Periods => return 5.0;
         when  Once_Every_Ten_Periods => return 10.0;
      end case;      
   end To_Rate;
   
   function From_Rate( r : Rate ) return  Frequency_Type is
   begin
      if( Nearly_Equal( r, 0.1 )) then return Ten_Times_Per_Period;
      elsif( Nearly_Equal( r, 0.2 )) then return Five_Times_Per_Period;
      elsif( Nearly_Equal( r, 1.0/3.0 )) then return Three_Times_Per_Period;
      elsif( Nearly_Equal( r, 0.5 )) then return Twice_Times_Per_Period;
      elsif( Nearly_Equal( r, 1.0 )) then return Once_Per_Period;
      elsif( Nearly_Equal( r, 2.0 )) then return Once_Every_Two_Periods;
      elsif( Nearly_Equal( r, 3.0 )) then return Once_Every_Three_Periods;
      elsif( Nearly_Equal( r, 5.0 )) then return Once_Every_Five_Periods;
      elsif( Nearly_Equal( r, 10.0 )) then return Once_Every_Ten_Periods;
      else
      -- RAISE EXCEPTION
            return Once_Per_Period;
      end if;
   end From_Rate;  


   
   function Load_Edit_Field( s : String; min : Amount := Amount'First; max : Amount := Amount'Last ) return Float_Edit_Field is
      ef : Float_Edit_Field;
      v  : Amount := 0.0;
   begin
      ef.buffer := TuS( s );
      Validate( s, v, ef.error_message, ef.error, min, max );
      if( ef.error = Utils.no_error ) then
         ef.buffer := TuS( Format( v ));
      end if;
      return ef;
   end Load_Edit_Field;
   
   function Load_Edit_Field( s : String; min_length : Natural := 0 ) return Text_Edit_Field is
      ef : Text_Edit_Field;
   begin
      ef.buffer := TuS( s );
      ef.error := no_error;
      if( Length( ef.buffer ) < min_length ) then
         ef.error := Utils.Format_Error;
         ef.error_message := TuS( "This needs to be at least " & Natural'Image(min_length) & " characters long " );
      end if;
      return ef;
   end Load_Edit_Field;

   
   function Load_Edit_Field( s : String; min : Integer := Integer'First; max : Integer := Integer'Last ) return Int_Edit_Field is
      ef : Int_Edit_Field;
      v  : Integer := 0; 
    begin
      ef.buffer := TuS( s );
      Validate( s, v, ef.error_message, ef.error, min, max );
      if( ef.error = Utils.no_error ) then
         ef.buffer := TuS( Format( v ));
      end if;
      return ef;
   end Load_Edit_Field;

   procedure Load_Charge( 
      params          : AWS.Parameters.List; 
      application_id  : Unbounded_String;
      target_id       : Unbounded_String;
      charge          : in out Charges_Buffer_Type ) is
   use AWS.Parameters;   
      charge_path : constant String := TS( application_id & "_" & target_id & "_" & charge.id );  
   begin
      charge.name := TuS( Trim( AWS.Parameters.Get( params, "charge-name_" & charge_path, 1 ), Both ));
      charge.charge_amount := Load_Edit_Field( AWS.Parameters.Get( params, "charge-amount_"& charge_path, 1 ), 0.0, 1_000_000.0 );
      Put_Line( " Load_Charge: charge_path |" & charge_path & "| got period as " & AWS.Parameters.Get( params, "charge-period_"& charge_path, 1 ));
      charge.period := Periods'Value( ach.To_Upper( AWS.Parameters.Get( params, "charge-period_"& charge_path, 1 )));
      charge.frequency := Frequency_Type'Value( ach.To_Upper( AWS.Parameters.Get( params, "charge-frequency_"& charge_path, 1 ))); --Load_Edit_Field( AWS.Parameters.Get( params, "charge-frequency_"& charge_path, 1 ), 0.0, 100.0 );
      charge.interest_rate := Load_Edit_Field( AWS.Parameters.Get( params, "charge-interest_"& charge_path, 1 ), 0.0, 50.0 );
   end Load_Charge;
   
   procedure Load_Target( 
      params          : AWS.Parameters.List; 
      application_id  : Unbounded_String;
      target          : in out Target_Buffer_Type ) is
   use Charges_Buffer_Package;
   use AWS.Parameters;
      package Benefits_Web is new Broad_Benefits_Package.Web_IO;
      package Employment_Web is new Employment_Package.Web_IO;
      package Gender_Web is new Gender_Package.Web_IO;
      n           : constant Natural := Natural( Length( target.charges ));
      target_path : constant String := TS( application_id & "_" & target.id ); 
      charge      : Charges_Buffer_Type; 
   begin
      
      target.name :=TuS( Trim( AWS.Parameters.Get( params, "target-name_" & target_path, 1 ), Both ));
      target.min_age := Load_Edit_Field( AWS.Parameters.Get( params, "min-age_"& target_path, 1 ), Person_Age'First, Person_Age'Last );
      target.max_age := Load_Edit_Field( AWS.Parameters.Get( params, "max-age_"& target_path, 1 ), Person_Age'First, Person_Age'Last );
      target.benefits := Benefits_Web.Handle( "benefits_" & target_path, params );
      target.employment := Employment_Web.Handle( "employment_" & target_path, params );
      target.genders := Gender_Web.Handle( "gender_" & target_path, params );
      for i in 1 .. n loop
         charge := Element( target.charges, i );
         Load_Charge( params, application_id, target.id, charge );
         Replace_Element( target.charges, i, charge );
      end loop;      
   end Load_Target;
   
   procedure Load_Application( 
      params       : AWS.Parameters.List; 
      application  : in out Application_Buffer_Type ) is
   use Targets_Buffer_Package;
   use AWS.Parameters;
      n            : constant Natural := Natural( Length( application.targets ));
      target       : Target_Buffer_Type;
      application_path : constant String := TS( application.id );
   begin
      application.name :=TuS( Trim( AWS.Parameters.Get( params, "application-name_" & application_path, 1 ), Both ));
      application.description :=TuS(AWS.Parameters.Get( params, "application-description_" & application_path, 1 ));
      Put_Line( "looking for application.name as |application-name_" & application_path & "| " );
      Put_Line( "got name as " & TS( application.name ));
      application.max_people := Load_Edit_Field( AWS.Parameters.Get( params, "application-max-people_"& application_path, 1 ), 1, Household_People_Range'Last  );
      application.max_children := Load_Edit_Field( AWS.Parameters.Get( params, "application-max-children_"& application_path, 1 ), 0, Household_Child_Range'Last );
      
      Put_Line( "looking for application.max_children as |application-max-children_" & application_path & "| got |" & TS(application.max_children.buffer) &"| "); 
      application.max_adults := Load_Edit_Field( AWS.Parameters.Get( params, "application-max-adults_"& application_path, 1 ), 0,  Household_Adult_Range'Last );
      for i in 1 .. n loop
         target := Element( application.targets, i );
         Load_Target( params, application.id, target );
         Replace_Element( application.targets, i, target );
      end loop;      
   end Load_Application;
   
   procedure Load_Regime(
      params       : AWS.Parameters.List;
      regime  : in out Charging_Regime_Buffer ) is
   use Targets_Buffer_Package;
   use AWS.Parameters;
      regime_path    : constant String := TS( regime.id );
      application_id : constant Unbounded_String := TuS(AWS.Parameters.Get( params, "application_id", 1 ));
      application    : Application_Buffer_Type;
   begin
      regime.name := Load_Edit_Field( Trim( AWS.Parameters.Get( params, "regime-name_" & regime_path, 1 ), Both ), 2 );
      regime.description :=TuS(AWS.Parameters.Get( params, "regime-description_" & regime_path, 1 ));
      Put_Line( "looking for regime.name as |regime-name_" & regime_path & "| " );
      Put_Line( "got name as " & TS( regime.name.buffer ));
      application := Get_Application( regime, application_id );
      Load_Application( params, application );
      Insert_Or_Replace_Application( regime, application_id, replace, application );
    end Load_Regime;
  
   function Make_Initialised_Charge return Charges_Buffer_Type is
      charge : Charges_Buffer_Type;
   begin
      charge.id := TuS(Utils.Random_String);
      charge.charge_amount.buffer := TuS( "0.0" );
      charge.discount.buffer := TuS( "0.0" ); 
      charge.frequency := Once_Per_Period;
      charge.interest_rate.buffer := TuS( "0.0" );  
      return charge;
   end Make_Initialised_Charge;
   
   function Make_Initialised_Target return Target_Buffer_Type is
   use Charges_Buffer_Package;
      target : Target_Buffer_Type;
      charge : Charges_Buffer_Type := Make_Initialised_Charge;
   begin
      target.id := TuS(Utils.Random_String);
      Append( target.charges, charge );
      target.min_age.buffer := TuS( "0" );
      target.max_age.buffer := TuS( "0" );
      return target;
   end Make_Initialised_Target;
   
   function Make_Initialised_Application return Application_Buffer_Type is
   use Targets_Buffer_Package;
      target : Target_Buffer_Type;
      application : Application_Buffer_Type;
   begin
      target := Make_Initialised_Target;
      application.id := TuS(Utils.Random_String);
      application.max_people.buffer := TuS( "1" );
      application.max_adults.buffer := TuS( "0" );
      application.max_children.buffer := TuS( "0" );
      Append( application.targets, target );
      return application;
   end Make_Initialised_Application;
   
   function Make_Initialised_Regime return Charging_Regime_Buffer is
   use Application_Buffer_Package;
      regime : Charging_Regime_Buffer := BLANK_CHARGING_REGIME_BUFFER;
      application : Application_Buffer_Type;
   begin
      application := Make_Initialised_Application;
      regime.id := TuS(Utils.Random_String);
      Append( regime.applications, application );
      return regime;
   end Make_Initialised_Regime;
   
   function Copy_Charge( charge : Charges_Buffer_Type; append_to_name : Boolean := False ) return Charges_Buffer_Type is
      copy : Charges_Buffer_Type;
   begin
       if( charge = BLANK_CHARGES_BUFFER ) then
         copy := Make_Initialised_Charge;
      else 
         copy := charge;
         copy.id := TuS(Utils.Random_String);
      end if;
      if( copy.name = TuS( "" )) then
         copy.name := TuS( "New Charge" );
      elsif( append_to_name ) then
         copy.name := TuS( "Copy of: " ) & copy.name;
      end if;
      return copy;
   end Copy_Charge;
   
   function Copy_Target( target : Target_Buffer_Type; append_to_name : Boolean := False ) return Target_Buffer_Type is
   use Charges_Buffer_Package;
      copy : Target_Buffer_Type;
      n    : constant Natural := Natural( Length( target.charges ));
      charge : Charges_Buffer_Type;
   begin
      if( target = BLANK_TARGET_BUFFER ) then
         copy := Make_Initialised_Target;
      else 
         copy := target;
         copy.id := TuS(Utils.Random_String);
         Clear( copy.charges );
         for i in 1 .. n loop
            charge := Element( target.charges, i );
            charge.id := TuS(Utils.Random_String);
            Append( copy.charges, Copy_Charge( charge, False ) );
         end loop;
      end if;
      if( copy.name = TuS( "" )) then
         copy.name := TuS( "New Beneficiary Group" );
      elsif( append_to_name ) then
         copy.name := TuS( "Copy of: " ) & copy.name;
      end if;
      return copy;
   end Copy_Target;
   
   function Copy_Application( 
      application : Application_Buffer_Type; 
      append_to_name : Boolean := False ) return Application_Buffer_Type is
   use Targets_Buffer_Package;
      copy : Application_Buffer_Type;
      n    : constant Natural := Natural( Length( application.targets ));
      target : Target_Buffer_Type;
   begin
      if( application = BLANK_APPLICATION_BUFFER ) then
         copy := Make_Initialised_Application;     
      else
         copy := application;
         Clear( copy.targets );
         copy.id := TuS(Utils.Random_String);
         for i in 1 .. n loop
            target := Element( application.targets, i );
            Append( copy.targets, Copy_Target( target, False ));
         end loop;
      end if;
      if( copy.name = TuS( "" )) then
         copy.name := TuS( "New Assumptions Set" );
      elsif( append_to_name ) then
         copy.name := TuS( "Copy of: " ) & copy.name;
      end if;
      return copy;
   end Copy_Application;
   
   function Copy_Regime( 
      regime : Charging_Regime_Buffer; 
      append_to_name : Boolean := False ) return Charging_Regime_Buffer is
   use Application_Buffer_Package;
      copy : Charging_Regime_Buffer;
      n    : constant Natural := Natural( Length( regime.applications ));
      application : Application_Buffer_Type;
   begin
      if( regime = BLANK_CHARGING_REGIME_BUFFER ) then
         copy := Make_Initialised_Regime;
      else
         copy := regime;
         Clear( copy.applications );
         copy.id := TuS(Utils.Random_String);
         for i in 1 .. n loop
            application := Element( regime.applications, i );
            Append( copy.applications, Copy_Application( application, False ));
         end loop;
      end if;
      if( copy.name.buffer = TuS( "" )) then
         copy.name.buffer := TuS( "New Charity" );
      elsif( append_to_name ) then
         copy.name.buffer := TuS( "Copy of: " ) & copy.name.buffer;
      end if;
      return copy;
   end Copy_Regime;

   procedure Split_Charge_Path( 
      charge_path : String;
      varname        : out Unbounded_String;
      application_id : out Unbounded_String;
      target_id      : out Unbounded_String;
      charge_id      : out Unbounded_String ) is
      use GNAT.String_Split;
      subset : Slice_Set;      
   begin
      Create( subset, charge_path, "_", Single );
      varname := TuS( Slice( subset, 1 ) );
      application_id := TuS( Slice( subset, 2 ) );
      if( Slice_Count( subset ) > 2 ) then
         target_id := TuS( Slice( subset, 3 ) );
      else
         target_id := TuS( "" );
      end if;
      if( Slice_Count( subset ) > 3 ) then
         charge_id := TuS( Slice( subset, 4 ) );
      else
         charge_id := TuS( "" );
      end if;
   end Split_Charge_Path;
   
   -- 
   --
   function Get_Application_Position( 
      buffer          : Charging_Regime_Buffer; 
      application_id  : Unbounded_String ) return Application_Buffer_Package.Cursor is
   use Application_Buffer_Package;
      application_pos : Cursor;
   begin
      application_pos := First( buffer.applications );
      if( application_pos = No_Element )then
         Put_Line( "Get_Application_Position returning No_Element" );
         return No_Element;
      end if;
      loop
         if( application_pos = No_Element ) then
            return application_pos;
         end if;
         if( Element( application_pos ).id = application_id ) then
            return application_pos;
         end if;
         application_pos := Next( application_pos );
      end loop;
   end Get_Application_Position;
   
   function Get_Application( 
      buffer          : Charging_Regime_Buffer; 
      application_id  : Unbounded_String ) return Application_Buffer_Type is
   use Application_Buffer_Package;
      application_pos : Cursor := Get_Application_Position( buffer, application_id );
   begin
      if( application_pos = No_Element )then
         Put_Line( "Get_Application: returning with No_Element " ); 
         return BLANK_APPLICATION_BUFFER;
      end if;
      return Element( application_pos );
   end Get_Application;
   
   function Get_Target_Position( 
      application : Application_Buffer_Type;
      target_id       : Unbounded_String ) return Targets_Buffer_Package.Cursor is
      use Targets_Buffer_Package;
      target_pos      : Targets_Buffer_Package.Cursor;
   begin
      target_pos := First( application.targets );
      if( target_pos = Targets_Buffer_Package.No_Element )then
         Put_Line( "no element in Get_Target_Position " );
         return No_Element;
      end if;
      loop
         if( target_pos = No_Element ) then
            Put_Line( "Get_Target_Position: exiting without finding " );
            return No_Element;
         end if;
         if( Element( target_pos ).id = target_id ) then
            Put_Line( "returning with target pos " & Targets_Buffer_Package.To_Index( target_pos )'Img );
            return target_pos;
         end if;
         target_pos := Next( target_pos );
      end loop;
   end Get_Target_Position;
   
   function Get_Target( 
      application : Application_Buffer_Type;
      target_id       : Unbounded_String ) return Target_Buffer_Type is
   use Targets_Buffer_Package;
      target_pos : Targets_Buffer_Package.Cursor := Get_Target_Position( application, target_id );
   begin
      if( target_pos = No_Element ) then
         return BLANK_TARGET_BUFFER;
      end if;
      return Element( target_pos );
   end Get_Target;
   
   function Get_Charge_Position( 
      target          : Target_Buffer_Type;
      charge_id       : Unbounded_String ) return Charges_Buffer_Package.Cursor is
   use Charges_Buffer_Package;
      charge_pos      : Charges_Buffer_Package.Cursor;
   begin
      charge_pos := First( target.charges );
      if( charge_pos = No_Element )then
         return No_Element;
      end if;
      Put_Line( "looking for " & TS(charge_id));
      loop
         if ( charge_pos = No_Element ) then
            return No_Element;
         end if;
         if( Element( charge_pos ).id = charge_id ) then
            return charge_pos;
         end if;
         Put_Line( "looking for " & TS(charge_id) & " Element( charge_pos ).id " & TS( Element( charge_pos ).id ) );
         charge_pos := Next( charge_pos );
      end loop;
   end Get_Charge_Position;
   
   function Get_Charge( 
      target : Target_Buffer_Type;
      charge_id   : Unbounded_String ) return Charges_Buffer_Type is
   use Charges_Buffer_Package;
      charge_pos : Charges_Buffer_Package.Cursor := Get_Charge_Position( target, charge_id );
   begin
      if( charge_pos = No_Element ) then
         return BLANK_CHARGES_BUFFER;
      end if;
      return Element( charge_pos );
   end Get_Charge;
      
   procedure Delete_Charge( 
      target       : in out Target_Buffer_Type; 
      charge_id    : Unbounded_String ) is
   use Charges_Buffer_Package;
      charge_pos      : Charges_Buffer_Package.Cursor;
   begin
      charge_pos := Get_Charge_Position( target, charge_id );
      if( charge_pos /= No_Element ) then
         Delete( target.charges, charge_pos );
      end if;
   end Delete_Charge;
   

   procedure Insert_Or_Replace_Charge( 
      target : in out Target_Buffer_Type; 
      charge_id       : Unbounded_String;
      before_or_after : Before_Or_After_Type;
      charge : Charges_Buffer_Type ) is
   use Charges_Buffer_Package;
      charge_pos      : Charges_Buffer_Package.Cursor := Get_Charge_Position( target, charge_id );
   begin
      
      if( charge_pos /= No_Element ) then
         if( before_or_after = after ) then
            charge_pos := Next( charge_pos );
         end if;
         if( charge_pos = No_Element ) then -- wound forward after end
            Append( target.charges, charge );
         else
            if(  before_or_after /= replace ) then 
               Insert( target.charges, charge_pos, charge );
            else
               Replace_Element( target.charges, charge_pos, charge );
            end if;
         end if;
      end if;
   end Insert_Or_Replace_Charge;
   
   procedure Insert_Or_Replace_Target( 
      application : in out Application_Buffer_Type; 
      target_id       : Unbounded_String;
      before_or_after : Before_Or_After_Type;
      target : Target_Buffer_Type ) is
   use Targets_Buffer_Package;
      target_pos      : Cursor;
   begin
      target_pos := Get_Target_Position( application, target_id );
      if( target_pos /= No_Element ) then
         if( before_or_after = after ) then
            target_pos := Next( target_pos );
         end if;
         if( target_pos = No_Element ) then -- wound beyond end
            Append( application.targets, target );
         else
            if(  before_or_after /= replace ) then 
               Insert( application.targets, target_pos, target );
            else
               Replace_Element( application.targets, target_pos, target );
            end if;
         end if;
      end if;
   end Insert_Or_Replace_Target;
   
   procedure Insert_Or_Replace_Application( 
      buffer : in out Charging_Regime_Buffer; 
      application_id  : Unbounded_String;
      before_or_after : Before_Or_After_Type;
      application : Application_Buffer_Type ) is
   use Application_Buffer_Package;
      application_pos : Cursor;
   begin
      application_pos := Get_Application_Position( buffer, application_id );
      if( application_pos /= No_Element ) then
         Put_Line( "Insert_Or_Replace_Application; action is " &   before_or_after'Img );
         if( before_or_after = after ) then
            application_pos := Next( application_pos );
         end if;
         if( application_pos = No_Element ) then -- wound forward beyond end
            Append( buffer.applications, application );
         else
            if(  before_or_after /= replace ) then 
               Insert( buffer.applications, application_pos, application );
            else
               Replace_Element( buffer.applications, application_pos, application );
            end if;
         end if;
      end if;
   end Insert_Or_Replace_Application;
  
   
   procedure Delete_Target(
      application : in out Application_Buffer_Type; 
      target_id      : Unbounded_String ) is
   use Targets_Buffer_Package;
      target_pos      : Targets_Buffer_Package.Cursor;            
   begin
      target_pos := Get_Target_Position( application, target_id );  
      if( target_pos /= No_Element ) then
         Delete( application.targets, target_pos );
      end if;
   end Delete_Target;
   
   procedure Delete_Application( 
      buffer : in out Charging_Regime_Buffer; 
      application_id : Unbounded_String ) is
   use Application_Buffer_Package;
      application_pos : Cursor;
   begin
      application_pos := Get_Application_Position( buffer, application_id );
      if( application_pos /= No_Element ) then
         Delete( buffer.applications, application_pos );
      end if;
   end Delete_Application;   

   function Wrap_Ajax( call : Unbounded_String; action : Ajax_Action_Type ) return Unbounded_String is
      url : Unbounded_String;
   begin
      url := "<img onclick=""" & call & """";
      case action is
      when insert_above => 
         url := url & "src='" & mwcn.OSCR_Paths.root & INSERT_BEFORE_IMG & "' alt='Insert a blank item before this one' />"; 
      when insert_below => 
         url := url & "src='" & mwcn.OSCR_Paths.root & INSERT_AFTER_IMG & "' alt='Insert a blank item after this one' />";
      when delete => 
         url := url & " src='" & mwcn.OSCR_Paths.root & DELETE_IMG & "' alt='Delete this item' />";
      when save => 
         url := url & " src='" & mwcn.OSCR_Paths.root & SAVE_IMG & "' alt='Save this item' />";
      when copy =>
         url := url & " src='" & mwcn.OSCR_Paths.root & COPY_IMG & "' alt='Make a copy of this item' />";
      when error_check  => null;
      end case;
      return url;
   end Wrap_Ajax;
   
   ROOT : constant String := TS( mwcn.OSCR_Paths.root );
   
   ERROR_CHECK_JS : constant String := " new Ajax.Updater( 'error_section', '" & root & "ajax', { method: 'post', parameters: 'action=error_check'})";
   
   
   
   function Make_Ajax_Call_Charge( 
      action : Ajax_Action_Type;
      application_id  : Unbounded_String;
      target_id       : Unbounded_String;
      charge_id       : Unbounded_String ) return Unbounded_String is
      call : Unbounded_String := TuS( "new Ajax.Updater(" ) & "'charges_" & target_id & "', '" & ROOT & "ajax', ";
      params : Unbounded_String := TuS( "{ method: 'post', parameters: '" );
   begin
      params := params & "action=" & Censor_String( Ajax_Action_Type'Image(action) ) & PARAM_SEP;
      params := params & "type=charge" & PARAM_SEP;
      params := params & "application_id=" & application_id & PARAM_SEP;
      params := params & "target_id=" & target_id & PARAM_SEP;
      params := params & "charge_id=" & charge_id & PARAM_SEP;
      -- params := params & "' +"  & " $( '" & "charges_" & target_id & "' ).serialize()";
      params := params & "' +"  & " $( 'mainform' ).serialize()";
      params := params & " }";
      call := call & params & " ); " & ERROR_CHECK_JS;
      return Wrap_Ajax( call, action );
   end Make_Ajax_Call_Charge;

   function Make_Ajax_Call_Target( 
      action : Ajax_Action_Type;
      application_id  : Unbounded_String;
      target_id       : Unbounded_String ) return Unbounded_String is
      call : Unbounded_String := TuS( "new Ajax.Updater(" ) & "'targets_" & application_id & "', '" & ROOT & "ajax', ";
      params : Unbounded_String := TuS( "{ method: 'post', parameters: '" );      
   begin
      params := params & "action=" & Censor_String( Ajax_Action_Type'Image(action) ) & PARAM_SEP;
      params := params & "type=target" & PARAM_SEP;
      params := params & "application_id=" & application_id & PARAM_SEP;
      params := params & "target_id=" & target_id & PARAM_SEP;
      params := params & "' +"  & " $( 'mainform' ).serialize()";
      params := params & " }";
      call := call & params & " ); " & ERROR_CHECK_JS;
      return Wrap_Ajax( call, action );
   end Make_Ajax_Call_Target;

   function Make_Ajax_Call_Application( 
      action         : Ajax_Action_Type;
      regime_id      : Unbounded_String;
      application_id : Unbounded_String ) return Unbounded_String is
      call : Unbounded_String := TuS( "new Ajax.Updater(" ) & "'regime_" & regime_id & "', '" & ROOT & "ajax', ";
      params : Unbounded_String := TuS( "{ method: 'post', parameters: '" ); 
   begin
      params := params & "action=" & Censor_String( Ajax_Action_Type'Image(action) ) & PARAM_SEP;
      params := params & "type=application" & PARAM_SEP;
      params := params & "application_id=" & application_id & PARAM_SEP;
      params := params & "' +"  & " $( 'mainform' ).serialize()";
      params := params & " }";
      call := call & params & " ); " & ERROR_CHECK_JS;
      return Wrap_Ajax( call, action );
   end Make_Ajax_Call_Application;

   
   function Charge_To_HTML( 
      application_id  : Unbounded_String;
      target_id       : Unbounded_String;
      charge          : Charges_Buffer_Type;
      num_charges     : Natural ) return Unbounded_String is
   use Templates_Parser;
      
      translations : Translate_Set;
      charge_path  : constant String := TS( application_id & "_" & target_id & "_" & charge.id );
      package Periods_Web is new Periods_Package.Web_IO;
      package Frequency_Web is new Frequency_Package.Web_IO;
   begin
   
      Insert( translations, Assoc( "CHARGE-NAME", charge.name ));
      
      if( charge.charge_amount.error /= No_Error ) then
         Insert( translations, 
            Assoc( "CHARGE-AMOUNT-ERROR-MESSAGE", 
               "<br/><div class='error_message'>" & TS(charge.charge_amount.error_message) & "</div>" ));
         Insert( translations, 
            Assoc( "CHARGE-AMOUNT-INPUT-CLASS", "class='error'" ));
      else
         Insert( translations, 
            Assoc( "CHARGE-AMOUNT-ERROR-MESSAGE", "" ));         
         Insert( translations, 
            Assoc( "CHARGE-AMOUNT-INPUT-CLASS", "" ));
      end if;
      
      Insert( translations, Assoc( "CHARGE-AMOUNT", charge.charge_amount.buffer ));
      
      Insert( translations, Assoc( "PERIOD-SELECTS",  
         Periods_Web.Make_Select_Elements( charge.period, Pretty_Print'Access ) 
         ));
         
      Insert( translations, Assoc( "FREQUENCY-SELECTS",  
         Frequency_Web.Make_Select_Elements( charge.frequency, Pretty_Print'Access ) 
         ));
      
         
      if( charge.interest_rate.error /= No_Error ) then
         Insert( translations, 
            Assoc( "CHARGE-INTEREST-ERROR-MESSAGE", 
               "<br/><div class='error_message'>" & TS(charge.interest_rate.error_message) & "</div>" ));
         Insert( translations, 
            Assoc( "CHARGE-INTEREST-INPUT-CLASS", "class='error'" ));
      else
         Insert( translations, 
            Assoc( "CHARGE-INTEREST-ERROR-MESSAGE", "" ));         
         Insert( translations, 
            Assoc( "CHARGE-INTEREST-INPUT-CLASS", "" ));
      end if;
      Insert( translations, Assoc( "CHARGE-INTEREST", charge.interest_rate.buffer ));
      
      Insert( translations, Assoc( "CHARGE-PATH", charge_path )); 
      Insert( translations, Assoc( "CHARGE-ID", charge.id ));
      Insert( translations, Assoc( "INSERT-BELOW-SECTION", 
         TS(Make_Ajax_Call_Charge( insert_below, application_id, target_id, charge.id )))); 
      Insert( translations, Assoc( "INSERT-ABOVE-SECTION",
         TS(Make_Ajax_Call_Charge( insert_above, application_id, target_id, charge.id ))));
      if( num_charges > 1 ) then
         Insert( translations, Assoc( "DELETE-SECTION", 
            TS(Make_Ajax_Call_Charge( delete, application_id, target_id, charge.id ))));
      else
         Insert( translations, Assoc( "DELETE-SECTION", "" ) );
      end if;
      Insert( translations, Assoc( "SAVE-SECTION", 
         TS(Make_Ajax_Call_Charge( save, application_id, target_id, charge.id )))); 
      Insert( translations, Assoc( "COPY-SECTION", 
         TS(Make_Ajax_Call_Charge( copy, application_id, target_id, charge.id )))); 
      Insert( translations, Assoc( "SAVE-SECTION", 
         TS(Make_Ajax_Call_Charge( save, application_id, target_id, charge.id )))); 
      return Web_Utils.Parse_Template( 
         mwcn.OSCR_Paths.template_components_path & "single_charge", 
         translations );
   end Charge_To_HTML;
   


   function Charges_To_HTML(
      application_id : Unbounded_String;
      target_id : Unbounded_String;
      charges : Charges_Buffer_List ) return Unbounded_String is
   use Templates_Parser;
      translations : Translate_Set;
      charges_block : Vector_Tag;
      charge        : Charges_Buffer_Type;
      n             : constant Natural := Natural( Charges_Buffer_Package.Length( charges ));
   begin
      for i in 1 .. n loop
         charge := Charges_Buffer_Package.Element( charges, i );
         charges_block := charges_block & Charge_To_HTML( 
            application_id,
            target_id,
            charge,
            n );
      end loop;
      Insert( translations, Assoc( "CHARGES", charges_block ));
      Insert( translations, Assoc( "TARGET-ID", target_id ));
      return Web_Utils.Parse_Template( 
         mwcn.OSCR_Paths.template_components_path & "charges_list", 
         translations );
    end Charges_To_HTML;
    
   
   function Target_To_HTML( 
      application_id  : Unbounded_String;
      target          : Target_Buffer_Type;
      num_targets     : Natural ) return Unbounded_String is
   use Templates_Parser;
      package Benefits_Web is new Broad_Benefits_Package.Web_IO;
      package Employment_Web is new Employment_Package.Web_IO;
      package Gender_Web is new Gender_Package.Web_IO;
      
      all_genders_except_missing : constant Gender_Package.Set := Make_Gender_Set_Without_Missing;
      all_emps_except_missing : Employment_Package.Set := Make_Employment_Set_Without_Missing;
      
      translations : Translate_Set;
      target_path  : constant String := TS( application_id & "_" & target.id  );    
      s             : Unbounded_String;
      ben_box : Unbounded_String := 
         Benefits_Web.Make_Discrete_Select_Boxes( 
            target_path, "benefits", target.benefits, Pretty_Print'Access ); 
      emp_box : Unbounded_String := 
         Employment_Web.Make_Discrete_Select_Boxes( 
            target_path, "employment", target.employment, Pretty_Print'Access, all_emps_except_missing ); 
      gen_box : Unbounded_String := 
         Gender_Web.Make_Discrete_Select_Boxes( 
            target_path, "gender", target.genders, Pretty_Print'Access, all_genders_except_missing ); 
   begin
      
      Insert( translations, Assoc( "TARGET-NAME", target.name ));
      Insert( translations, Assoc( "TARGET-BENEFITS", ben_box ));
      Insert( translations, Assoc( "TARGET-EMPLOYMENT", emp_box ));
      Insert( translations, Assoc( "TARGET-GENDERS", gen_box ));
      Insert( translations, Assoc( "CHARGES_LIST", 
         Charges_To_HTML(
            application_id,
            target.id,
            target.charges ) ));
      Insert( translations, Assoc( "MIN-AGE", target.min_age.buffer ));
      if( target.min_age.error /= No_Error ) then
         Insert( translations, 
            Assoc( "MIN-AGE-ERROR-MESSAGE", 
               "<br/><div class='error_message'>" & TS(target.min_age.error_message) & "</div>" ));
         Insert( translations, 
            Assoc( "MIN-AGE-INPUT-CLASS", "class='error'" ));
      else
         Insert( translations, 
            Assoc( "MIN-AGE-ERROR-MESSAGE", "" ));         
         Insert( translations, 
            Assoc( "MIN-AGE-INPUT-CLASS", "" ));
      end if;
      
      Insert( translations, Assoc( "MAX-AGE", target.max_age.buffer ));
      if( target.max_age.error /= No_Error ) then
         Insert( translations, 
            Assoc( "MAX-AGE-ERROR-MESSAGE", 
               "<br/><div class='error_message'>" & TS(target.max_age.error_message) & "</div>" ));
         Insert( translations, 
            Assoc( "MAX-AGE-INPUT-CLASS", "class='error'" ));
      else
         Insert( translations, 
            Assoc( "MAX-AGE-ERROR-MESSAGE", "" ));         
         Insert( translations, 
            Assoc( "MAX-AGE-INPUT-CLASS", "" ));
      end if;
      -- Make_Ajax_Call_Application
      Insert( translations, Assoc( "TARGET-PATH", target_path )); 
      Insert( translations, Assoc( "TARGET-ID", target.id ));
      Insert( translations, Assoc( "INSERT-BELOW-SECTION", 
         TS(Make_Ajax_Call_Target( insert_below, application_id, target.id )))); 
      Insert( translations, Assoc( "INSERT-ABOVE-SECTION",
         TS(Make_Ajax_Call_Target( insert_above, application_id, target.id ))));
      if( num_targets > 1 ) then
         Insert( translations, Assoc( "DELETE-SECTION", 
            TS(Make_Ajax_Call_Target( delete, application_id, target.id ))));
      else
         Insert( translations, Assoc( "DELETE-SECTION", "" ) );
      end if;
      Insert( translations, Assoc( "SAVE-SECTION", 
         TS(Make_Ajax_Call_Target( save, application_id, target.id )))); 
      Insert( translations, Assoc( "COPY-SECTION", 
         TS(Make_Ajax_Call_Target( copy, application_id, target.id )))); 
      Insert( translations, Assoc( "SAVE-SECTION", 
         TS(Make_Ajax_Call_Target( save, application_id, target.id )))); 
      return Web_Utils.Parse_Template( 
         mwcn.OSCR_Paths.template_components_path & "single_target", 
         translations );
   end Target_To_HTML;
   
   function Targets_To_HTML(
      application_id  : Unbounded_String;
      targets         : Targets_Buffer_List )  return Unbounded_String is
   use Templates_Parser;
      n             : constant Natural := Natural( Targets_Buffer_Package.Length( targets ));
      targets_block : Vector_Tag;
      target : Target_Buffer_Type;          
      translations : Translate_Set;
   begin
      for i in 1..n loop
         target := Targets_Buffer_Package.Element( targets, i );
         targets_block := targets_block & Target_To_HTML( 
            application_id,
            target,
            n );
      end loop;
      Insert( translations, Assoc( "APPLICATION-ID", application_id ));
      Insert( translations, Assoc( "TARGETS", targets_block ));
      return Web_Utils.Parse_Template( 
         mwcn.OSCR_Paths.template_components_path & "targets_list", 
         translations );
   end Targets_To_HTML;
   
   function Application_To_HTML( 
      regime_id            : Unbounded_String;
      application          : Application_Buffer_Type;
      num_applications     : Natural ) return Unbounded_String is
   use Templates_Parser;
      translations : Translate_Set;
   begin
      Insert( translations, Assoc( "APPLICATION-NAME", application.name ));
      Insert( translations, Assoc( "APPLICATION-DESCRIPTION", application.description ));
      
      Insert( translations, Assoc( "MAX-ADULTS", application.max_adults.buffer ));
      Insert( translations, Assoc( "TARGETS-LIST", 
         Targets_To_HTML( application.id, application.targets ) ));
      if( application.max_adults.error /= No_Error ) then
         Insert( translations, 
            Assoc( "MAX-ADULTS-ERROR-MESSAGE", 
               "<br/><div class='error_message'>" & TS(application.max_adults.error_message) & "</div>" ));
         Insert( translations, 
            Assoc( "MAX-ADULTS-INPUT-CLASS", "class='error'" ));
      else
         Insert( translations, 
            Assoc( "MAX-ADULTS-ERROR-MESSAGE", "" ));         
         Insert( translations, 
            Assoc( "MAX-ADULTS-INPUT-CLASS", "" ));
      end if;
      
      Insert( translations, Assoc( "MAX-CHILDREN", application.max_children.buffer ));
      if( application.max_children.error /= No_Error ) then
         Insert( translations, 
            Assoc( "MAX-CHILDREN-ERROR-MESSAGE", 
               "<br/><div class='error_message'>" & TS(application.max_children.error_message) & "</div>" ));
         Insert( translations, 
            Assoc( "MAX-CHILDREN-INPUT-CLASS", "class='error'" ));
      else
         Insert( translations, 
            Assoc( "MAX-CHILDREN-ERROR-MESSAGE", "" ));         
         Insert( translations, 
            Assoc( "MAX-CHILDREN-INPUT-CLASS", "" ));
      end if;
      Insert( translations, Assoc( "MAX-PEOPLE", application.max_people.buffer ));
      if( application.max_people.error /= No_Error ) then
         Insert( translations, 
            Assoc( "MAX-PEOPLE-ERROR-MESSAGE", 
               "<br/><div class='error_message'>" & TS(application.max_people.error_message) & "</div>" ));
         Insert( translations, 
            Assoc( "MAX-PEOPLE-INPUT-CLASS", "class='error'" ));
      else
         Insert( translations, 
            Assoc( "MAX-PEOPLE-ERROR-MESSAGE", "" ));         
         Insert( translations, 
            Assoc( "MAX-PEOPLE-INPUT-CLASS", "" ));
      end if;
      
      Insert( translations, Assoc( "APPLICATION-PATH", application.id )); 
      Insert( translations, Assoc( "APPLICATION-ID", application.id ));
      
      Insert( translations, Assoc( "INSERT-BELOW-SECTION", 
         TS(Make_Ajax_Call_Application( insert_below, regime_id, application.id )))); 
      Insert( translations, Assoc( "INSERT-ABOVE-SECTION",
         TS(Make_Ajax_Call_Application( insert_above, regime_id, application.id  ))));
      if( num_applications > 1 ) then
         Insert( translations, Assoc( "DELETE-SECTION", 
            TS(Make_Ajax_Call_Application( delete, regime_id, application.id  ))));
      else
         Insert( translations, Assoc( "DELETE-SECTION", "" ) );
      end if;
      Insert( translations, Assoc( "SAVE-SECTION", 
         TS(Make_Ajax_Call_Application( save, regime_id, application.id  )))); 
      Insert( translations, Assoc( "COPY-SECTION", 
         TS(Make_Ajax_Call_Application( copy, regime_id, application.id  )))); 
      Insert( translations, Assoc( "SAVE-SECTION", 
         TS(Make_Ajax_Call_Application( save, regime_id, application.id  )))); 
      return Web_Utils.Parse_Template( 
         mwcn.OSCR_Paths.template_components_path & "single_application", 
         translations );
   end Application_To_HTML;
   
   function Regime_To_HTML( regime : Charging_Regime_Buffer; application_id : Unbounded_String ) return Unbounded_String is
   use Templates_Parser;
   use Application_Buffer_Package;
   use type Ada.Containers.Count_Type;
      application_buffer : constant Application_Buffer_Type := Get_Application( regime, application_id );
      n : constant Natural := Natural( Application_Buffer_Package.Length( regime.applications )); 
      app_text : constant Unbounded_String := Application_To_HTML( regime.id, application_buffer, n );
      translations : Translate_Set;
   begin
      Insert( translations, Assoc( "APPLICATION-SECTION", app_text ));
      if( Length( regime.applications ) > 1 ) then
         Insert( translations, Assoc( "MENU", Make_Submenu( regime, application_id )));
      end if;
      Insert( translations, Assoc( "REGIME-ID", regime.id ));
      Insert( translations, Assoc( "REGIME-NAME", regime.name.buffer ));
      if( regime.name.error /= No_Error ) then
         Insert( translations, 
            Assoc( "REGIME-NAME-ERROR-MESSAGE", 
               "<br/><div class='error_message'>" & TS( regime.name.error_message) & "</div>" ));
         Insert( translations, 
            Assoc( "REGIME-NAME-INPUT-CLASS", "class='error'" ));
      else
         Insert( translations, 
            Assoc( "REGIME-NAME-ERROR-MESSAGE", "" ));         
         Insert( translations, 
            Assoc( "REGIME-NAME-INPUT-CLASS", "" ));
      end if;
      Insert( translations, Assoc( "REGIME-DESCRIPTION", regime.description ));
      return Web_Utils.Parse_Template( 
         mwcn.OSCR_Paths.template_components_path & "single_regime", 
         translations );      
   end Regime_To_HTML;
   
   
   function Map_From( charge : Charges_Type ) return Charges_Buffer_Type is
      buffer : Charges_Buffer_Type;
   begin
      buffer.name := charge.name;
      buffer.id   := Tus( Utils.Random_String );
      buffer.charge_amount.buffer := Format_L( charge.charge_amount );
      buffer.discount.buffer := Format_L( charge.discount );
      buffer.period := charge.period;
      buffer.frequency := From_Rate( charge.frequency ); -- .buffer := Format_L( charge.frequency ); -- period = daily frequency = 10.0 => 1ce every 10 days and so on; 0.5 = twice a day 
      buffer.interest_rate.buffer := Format_L( charge.interest_rate );  
      buffer.join := charge.join;
      return buffer;
   end Map_From;

   function Map_From( target : Target_Type ) return Target_Buffer_Type is
      buffer : Target_Buffer_Type;
   begin
      buffer.name := target.name;
      buffer.id   := Tus( Utils.Random_String );
      buffer.min_age.buffer := Format_L( target.min_age );
      buffer.max_age.buffer := Format_L( target.max_age );
      buffer.genders := target.genders;
      buffer.benefits := target.benefits;
      buffer.employment := target.employment;
      -- buffer.max_charges.buffer := Format_L( target.max_charges );
      Charges_Buffer_Package.Clear( buffer.charges );
      for i in 1 .. Charges_Package.Length( target.charges ) loop
         Charges_Buffer_Package.Append( buffer.charges, 
            Map_From( Charges_Package.Element( target.charges, Positive(i) ) ));
      end loop;
      buffer.isExclusive := target.isExclusive;
      return buffer;
   end Map_From;
   
   function Error_Count( buffer : Charges_Buffer_Type ) return Natural is
      errs : Natural := 0;
   begin
      if buffer.charge_amount.error /= No_Error then errs := errs + 1; end if;
      if buffer.discount.error /= No_Error then errs := errs + 1; end if;
      if buffer.interest_rate.error /= No_Error then errs := errs + 1; end if;
      return errs;
   end Error_Count;
   
   function Map_To( buffer : Charges_Buffer_Type ) return Charges_Type is
      charge : Charges_Type;
   begin
      charge.name := buffer.name;
      charge.id   := buffer.id;
      charge.charge_amount := Lenient_Convert( buffer.charge_amount.buffer );
      charge.discount := Lenient_Convert( buffer.discount.buffer );
      charge.period := buffer.period;
      charge.frequency :=  To_Rate( buffer.frequency ); -- Lenient_Convert( buffer.frequency.buffer );  
      charge.interest_rate := Lenient_Convert( buffer.interest_rate.buffer );  
      charge.join := buffer.join;
      return charge;
   end Map_To;
   
   function Error_Count( buffer : Target_Buffer_Type ) return Natural is
   use Charges_Buffer_Package;
      errs : Natural := 0;
      
      procedure Add_Errors( pos : Cursor ) is
         charge : Charges_Buffer_Type :=  element( pos );
      begin
         errs := errs + Error_Count( charge );
      end Add_Errors;
      
   begin
      if buffer.min_age.error /= No_Error then errs := errs + 1; end if;
      if buffer.max_age.error /= No_Error then errs := errs + 1; end if;
      -- if buffer.max_charges.error /= No_Error then errs := errs + 1; end if;
      Iterate( buffer.charges, Add_Errors'Access );
      return errs;
   end Error_Count;
   
   function Map_To( buffer : Target_Buffer_Type ) return Target_Type is
      target : Target_Type;
   begin
      target.name := buffer.name;
      target.id := buffer.id;
      target.min_age := Integer'Value( TS( buffer.min_age.buffer ));
      target.max_age := Integer'Value( TS( buffer.max_age.buffer ));
      target.genders := buffer.genders;
      target.benefits := buffer.benefits;
      target.employment := buffer.employment;
      --  target.max_charges := Integer'Value( TS(buffer.max_charges.buffer) );
      Charges_Package.Clear( target.charges );
      for i in 1 .. Charges_Buffer_Package.Length( buffer.charges ) loop
         -- Put_Line( "Target_Buffer_Type::Map To " & i'Img );
         Charges_Package.Append( target.charges, 
            Map_To( Charges_Buffer_Package.Element( buffer.charges, Positive(i) )));
      end loop;
      -- Put_Line( "Charges_Buffer_Package.Length " & Charges_Buffer_Package.Length( buffer.charges )'Img ); 
      target.isExclusive := buffer.isExclusive;
      return target;
   end Map_To;
   
   function Map_From( application : Application_Type ) return Application_Buffer_Type is
      buffer : Application_Buffer_Type;
   begin
      buffer.name := application.name;
      buffer.id   := Tus( Utils.Random_String );
      buffer.description := application.description;
      buffer.max_people.buffer := Format_L( application.max_people );
      buffer.max_children.buffer := Format_L( application.max_children ); 
      buffer.max_adults.buffer   := Format_L( application.max_adults );
      Targets_Buffer_Package.Clear( buffer.targets );
      for i in 1 .. Target_Package.Length( application.targets ) loop
         -- Put_Line( "Target_Buffer_Type::Map From " & i'Img );
         Targets_Buffer_Package.Append( buffer.targets, 
            Map_From( Target_Package.Element( application.targets, Positive(i) ) ));
      end loop;
      return buffer;
   end Map_From;

   function Error_Count( buffer : Application_Buffer_Type ) return Natural is
   use Targets_Buffer_Package;
      errs : Natural := 0;
      
      procedure Add_Errors( pos : Cursor ) is
         charge : Target_Buffer_Type :=  element( pos );
      begin
         errs := errs + Error_Count( charge );
      end Add_Errors;
      
   begin
      if buffer.max_people.error /= No_Error then errs := errs + 1; end if;
      if buffer.max_children.error /= No_Error then errs := errs + 1; end if;
      if buffer.max_adults.error /= No_Error then errs := errs + 1; end if;
      Iterate( buffer.targets, Add_Errors'Access );
      return errs;
   end Error_Count;

   
   function Map_To( buffer : Application_Buffer_Type ) return Application_Type is
      application : Application_Type;
   begin
      application.name := buffer.name;
      application.id   := buffer.id;
      application.description := buffer.description;
      application.max_people := Household_People_Range'Value( TS(buffer.max_people.buffer) );
      application.max_children := Household_Child_Range'Value( TS(buffer.max_children.buffer) ); 
      application.max_adults := Household_Adult_Range'Value( TS(buffer.max_adults.buffer) ); 
      Target_Package.Clear( application.targets );
      for i in 1 .. Targets_Buffer_Package.Length( buffer.targets ) loop
         -- Put_Line( "Map_To( buffer : Application_Buffer_Type ) " & i'Img );
         Target_Package.Append( application.targets, 
            Map_To( Targets_Buffer_Package.Element( buffer.targets, Positive(i) ) ));
      end loop;
      return application;
   end Map_To;

   function Error_Count( buffer : Charging_Regime_Buffer ) return Natural is
   use Application_Buffer_Package;
      errs : Natural := 0;
      
      procedure Add_Errors( pos : Cursor ) is
         charge : Application_Buffer_Type :=  element( pos );
      begin
         errs := errs + Error_Count( charge );
      end Add_Errors;
      
   begin
      if( buffer.name.error /= Utils.no_error ) then
         errs := 1;
      end if;
      Iterate( buffer.applications, Add_Errors'Access );
      return errs;
   end Error_Count;

   function Map_From( regime : Charging_Regime ) return Charging_Regime_Buffer is
      buffer : Charging_Regime_Buffer;
   begin
      buffer.name.buffer := regime.name;
      buffer.id   := Tus( Utils.Random_String );
      buffer.description := regime.description;
      for i in 1 .. Application_Package.Length( regime.applications ) loop
         Application_Buffer_Package.Append( buffer.applications, 
            Map_From( Application_Package.Element( regime.applications, Positive(i) ) ));
      end loop;
      return buffer;
   end Map_From;
 
   function Map_To( buffer : Charging_Regime_Buffer ) return Charging_Regime is
      regime : Charging_Regime;
   begin
      regime.name := buffer.name.buffer;
      regime.id   := buffer.id;
      regime.description := buffer.description;
      -- Put_Line( "Application_Buffer_Package.Length( buffer.applications ) = " & Application_Buffer_Package.Length( buffer.applications )'Img );
      for i in 1 .. Application_Buffer_Package.Length( buffer.applications ) loop
         Application_Package.Append( regime.applications, 
            Map_To( Application_Buffer_Package.Element( buffer.applications, Positive(i) ) ));
      end loop;
      return regime;
   end Map_To;
   
   function Make_Submenu( regime : Charging_Regime_Buffer; this_application_id : Unbounded_String ) return Unbounded_String is
   use Unbounded_String_Vector_Package;
   use Text_Utils;
      application_names : Unbounded_String_List;
      application_ids : Unbounded_String_List;
      NUM_APPLICATIONS : constant Natural := Natural( Application_Buffer_Package.Length( regime.applications ));
      outs : Unbounded_String;
   begin
      for application_no in 1 .. NUM_APPLICATIONS loop
         Append( application_names, Application_Buffer_Package.Element( regime.applications, application_no ).name );
         Append( application_ids, Application_Buffer_Package.Element( regime.applications, application_no ).id );
      end loop;
      outs := Web_Utils.Make_Menu( TS(mwcn.OSCR_Paths.root) & "input", "application_id", application_names, application_ids, this_application_id, 0 );
      return outs;
   end Make_Submenu;
   
end Model.Charging.Buffer;
