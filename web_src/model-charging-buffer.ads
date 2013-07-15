with AWS.Parameters;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with T_Utils;
with FRS_Enums;
with Model.Household;
with Model.Incomes;
with Model.Income_Measure_Types;

with Ada.Strings.Unbounded;
with Utils;

package Model.Charging.Buffer is

   use FRS_Enums;
   use Model.Household;
   use Model.Incomes;
   use Ada.Strings.Unbounded;
   use Utils;
   
   type Before_Or_After_Type is ( Before, After, Replace );
   type Ajax_Action_Type is ( insert_above, insert_below, delete, save, copy, error_check );
   type Block_Type is ( charge, target, application ); 

   package mimt renames Model.Income_Measure_Types;
   
   --
   -- edit images, from the Java repository,
   -- relative to the server root
   --
   JAVA_IMAGES_PATH  : constant String := "images/java_graphs/";
   INSERT_BEFORE_IMG : constant String := JAVA_IMAGES_PATH & "table/RowInsertBefore24.gif";
   INSERT_AFTER_IMG  : constant String := JAVA_IMAGES_PATH & "table/RowInsertAfter24.gif";
   DELETE_IMG        : constant String := JAVA_IMAGES_PATH & "table/RowDelete24.gif";
   SAVE_IMG          : constant String := JAVA_IMAGES_PATH & "general/Save24.gif";
   COPY_IMG          : constant String := JAVA_IMAGES_PATH & "general/Copy24.gif";
   PASTE_IMG         : constant String := JAVA_IMAGES_PATH & "general/Paste24.gif";
   
   PARAM_SEP : constant String := "&amp;";

   type Float_Edit_Field is record
      error          : Error_Type := No_Error;
      buffer         : Unbounded_String;
      error_message  : Unbounded_String;
   end record;
   
   type Int_Edit_Field is record
      error           : Error_Type := No_Error;
      buffer          : Unbounded_String;
      error_message   : Unbounded_String;
   end record;
   
   type Text_Edit_Field is record
      error           : Error_Type := No_Error;
      buffer          : Unbounded_String := To_Unbounded_String( "" );
      error_message   : Unbounded_String;
   end record;
   
   function Load_Edit_Field( s : String; min : Amount := Amount'First; max : Amount := Amount'Last ) return Float_Edit_Field; 
   function Load_Edit_Field( s : String; min : Integer := Integer'First; max : Integer := Integer'Last ) return Int_Edit_Field; 
   function Load_Edit_Field( s : String; min_length : Natural := 0 ) return Text_Edit_Field; 
   
   
   BLANK_INT_EDIT_FIELD : constant Int_Edit_Field := (
      error           => No_Error,
      buffer          => To_Unbounded_String("0"),
      error_message   => To_Unbounded_String("")
   );
   
   BLANK_TEXT_EDIT_FIELD : constant Text_Edit_Field := (
      error           => No_Error,
      buffer          => To_Unbounded_String(""),
      error_message   => To_Unbounded_String("")
   );
   
   BLANK_FLOAT_EDIT_FIELD : constant Float_Edit_Field := (
      error           => No_Error,
      buffer          => To_Unbounded_String("0.0"),
      error_message   => To_Unbounded_String("")
   );
   
   type Frequency_Type is ( Ten_Times_Per_Period, Five_Times_Per_Period, Three_Times_Per_Period, Twice_Times_Per_Period, Once_Per_Period,
      Once_Every_Two_Periods, Once_Every_Three_Periods, Once_Every_Five_Periods, Once_Every_Ten_Periods );

   
   type Charges_Buffer_Type is record
      name     : Unbounded_String := To_Unbounded_String( "" );
      id       : Unbounded_String := To_Unbounded_String( "" );
      charge_amount : Float_Edit_Field;
      discount : Float_Edit_Field; 
      period   : Periods := daily;
      frequency : Frequency_Type; -- Float_Edit_Field; -- period = daily frequency = 10.0 => 1ce every 10 days and so on; 0.5 = twice a day 
      interest_rate : Float_Edit_Field;  
      join     : Join_Type := j_and;
   end record;
   
   BLANK_CHARGES_BUFFER : constant Charges_Buffer_Type := (
      name          => To_Unbounded_String( "" ),
      id            => To_Unbounded_String( "" ),
      charge_amount => BLANK_FLOAT_EDIT_FIELD,
      discount      => BLANK_FLOAT_EDIT_FIELD,
      period        => daily,
      frequency     => once_per_period, 
      interest_rate => BLANK_FLOAT_EDIT_FIELD,  
      join          => j_and
   );
   
   
   package Charges_Buffer_Package is new Ada.Containers.Vectors( 
      Element_Type => Charges_Buffer_Type, Index_Type => Positive );
   subtype Charges_Buffer_List is Charges_Buffer_Package.Vector; 
   
   type Target_Buffer_Type is record
      name        : Unbounded_String := To_Unbounded_String( "" );
      id          : Unbounded_String := To_Unbounded_String( "" );
      min_age     : Int_Edit_Field;
      max_age     : Int_Edit_Field;
      genders     : Gender_Set;
      benefits    : Broad_Benefits_Set;
      employment  : Employment_Set;
      max_charges : Int_Edit_Field;
      charges     : Charges_Buffer_List;
      isExclusive : Boolean := False; -- so only this applies, any subsequent hits discarded
   end record;
   
   package Targets_Buffer_Package is new Ada.Containers.Vectors( 
      Element_Type => Target_Buffer_Type, Index_Type => Positive );
   subtype Targets_Buffer_List is Targets_Buffer_Package.Vector;
   
   BLANK_TARGET_BUFFER : constant Target_Buffer_Type := (
      name        => To_Unbounded_String( "" ),
      id          => To_Unbounded_String( "" ),
      min_age     => BLANK_INT_EDIT_FIELD,
      max_age     => BLANK_INT_EDIT_FIELD,
      genders     => Gender_Package.Empty_Set,
      benefits    => Broad_Benefits_Package.Empty_Set,
      employment  => Employment_Package.Empty_Set,
      max_charges => BLANK_INT_EDIT_FIELD,
      charges     => Charges_Buffer_Package.Empty_Vector,
      isExclusive => False
   );
   
   
   type Application_Buffer_Type is record
      name : Unbounded_String := To_Unbounded_String( "" );
      id   : Unbounded_String := To_Unbounded_String( "" );
      description : Unbounded_String := To_Unbounded_String( "" );
      max_people   : Int_Edit_Field;
      max_children : Int_Edit_Field;
      max_adults   : Int_Edit_Field;
      targets      : Targets_Buffer_List;
   end record;
   
   BLANK_APPLICATION_BUFFER : constant Application_Buffer_Type := (
      name         =>To_Unbounded_String( "" ),
      id           => To_Unbounded_String( "" ),
      description  => To_Unbounded_String( "" ),
      max_people   => BLANK_INT_EDIT_FIELD,
      max_children => BLANK_INT_EDIT_FIELD,
      max_adults   => BLANK_INT_EDIT_FIELD,     
      targets     => Targets_Buffer_Package.Empty_Vector
   );

   package Application_Buffer_Package is new Ada.Containers.Vectors( 
      Element_Type => Application_Buffer_Type, Index_Type => Positive );
   subtype Application_Buffer_List is Application_Buffer_Package.Vector; 
   
   type Charging_Regime_Buffer is record
      name : Text_Edit_Field ;
      description : Unbounded_String := To_Unbounded_String( "" );
      id   : Unbounded_String := To_Unbounded_String( "" );
      applications : Application_Buffer_List;
  end record;
   
   BLANK_CHARGING_REGIME_BUFFER : constant Charging_Regime_Buffer := (
      name         => BLANK_TEXT_EDIT_FIELD,
      description  => To_Unbounded_String( "" ),
      id           => To_Unbounded_String( "" ),
      applications => Application_Buffer_Package.Empty_Vector
   );
   
      

   package Charging_Regime_Buffer_Package is new Ada.Containers.Vectors( 
      Element_Type => Charging_Regime_Buffer, Index_Type => Positive );
   subtype Charging_Regime_Buffer_List is Charging_Regime_Buffer_Package.Vector; 
   
   function Map_From( regime : Charging_Regime ) return Charging_Regime_Buffer;
   function Map_To( buffer : Charging_Regime_Buffer ) return Charging_Regime;
   
   function Get_Application( 
      buffer          : Charging_Regime_Buffer; 
      application_id  : Unbounded_String ) return Application_Buffer_Type;
      
   function Get_Target( 
      application     : Application_Buffer_Type;
      target_id       : Unbounded_String ) return Target_Buffer_Type;
      
   function Get_Charge( 
      target          : Target_Buffer_Type;
      charge_id       : Unbounded_String ) return Charges_Buffer_Type;
      
      
   procedure Insert_Or_Replace_Charge( 
      target          : in out Target_Buffer_Type; 
      charge_id       : Unbounded_String;
      before_or_after : Before_Or_After_Type;
      charge          : Charges_Buffer_Type );
      
   procedure Insert_Or_Replace_Target(
      application     : in out Application_Buffer_Type;
      target_id       : Unbounded_String;
      before_or_after : Before_Or_After_Type;
      target          : Target_Buffer_Type );
      
   procedure Insert_Or_Replace_Application( 
      buffer          : in out Charging_Regime_Buffer; 
      application_id  : Unbounded_String;
      before_or_after : Before_Or_After_Type;
      application     : Application_Buffer_Type );
      
   procedure Delete_Charge( 
      target          : in out Target_Buffer_Type; 
      charge_id       : Unbounded_String );
      
   procedure Delete_Target(
      application     : in out Application_Buffer_Type; 
      target_id       : Unbounded_String );
      
   procedure Delete_Application( 
      buffer : in out Charging_Regime_Buffer; 
      application_id : Unbounded_String );

   function Error_Count( buffer : Charges_Buffer_Type ) return Natural;
   function Error_Count( buffer : Target_Buffer_Type ) return Natural;
   function Error_Count( buffer : Application_Buffer_Type ) return Natural;
   function Error_Count( buffer : Charging_Regime_Buffer ) return Natural;
   
   --
   -- Testing only
   --
   function Make_Ajax_Call_Charge( 
      action : Ajax_Action_Type; 
      application_id  : Unbounded_String;
      target_id       : Unbounded_String;
      charge_id       : Unbounded_String ) return Unbounded_String;
      
   function Make_Ajax_Call_Target( 
      action : Ajax_Action_Type;
      application_id  : Unbounded_String;
      target_id       : Unbounded_String ) return Unbounded_String;
      
   function Make_Ajax_Call_Application( 
      action : Ajax_Action_Type;
      regime_id      : Unbounded_String;
      application_id  : Unbounded_String ) return Unbounded_String;
      
   function Charge_To_HTML( 
      application_id  : Unbounded_String;
      target_id       : Unbounded_String;
      charge          : Charges_Buffer_Type;
      num_charges     : Natural ) return Unbounded_String;
      
   function Charges_To_HTML( 
      application_id  : Unbounded_String;
      target_id : Unbounded_String;
      charges         : Charges_Buffer_List )  return Unbounded_String;
      
   function Target_To_HTML( 
      application_id  : Unbounded_String;
      target          : Target_Buffer_Type;
      num_targets     : Natural ) return Unbounded_String;
      
   function Targets_To_HTML(
      application_id  : Unbounded_String;
      targets         : Targets_Buffer_List )  return Unbounded_String;
      
   function Application_To_HTML( 
      regime_id            : Unbounded_String;
      application          : Application_Buffer_Type;
      num_applications     : Natural ) return Unbounded_String;
   
   function Regime_To_HTML( 
      regime : Charging_Regime_Buffer; 
      application_id : Unbounded_String ) return Unbounded_String;
    
   function Make_Submenu( 
      regime : Charging_Regime_Buffer; 
      this_application_id : Unbounded_String ) return Unbounded_String;

      
   procedure Load_Charge( 
      params          : AWS.Parameters.List; 
      application_id  : Unbounded_String;
      target_id       : Unbounded_String;
      charge          : in out Charges_Buffer_Type );
      
   procedure Load_Target( 
      params          : AWS.Parameters.List; 
      application_id  : Unbounded_String;
      target          : in out Target_Buffer_Type );
      
   procedure Load_Application( 
      params          : AWS.Parameters.List; 
      application     : in out Application_Buffer_Type );
      
   procedure Load_Regime(
      params       : AWS.Parameters.List;
      regime  : in out Charging_Regime_Buffer );
      
   function Copy_Charge( charge : Charges_Buffer_Type; append_to_name : Boolean := False ) return Charges_Buffer_Type;
   function Copy_Target( target : Target_Buffer_Type; append_to_name : Boolean := False ) return Target_Buffer_Type;
   function Copy_Application( application : Application_Buffer_Type; append_to_name : Boolean := False ) return Application_Buffer_Type;
   function Copy_Regime( regime : Charging_Regime_Buffer; append_to_name : Boolean := False ) return Charging_Regime_Buffer;

   function Make_Initialised_Target return Target_Buffer_Type;
   function Make_Initialised_Application return Application_Buffer_Type;
   function Make_Initialised_Regime return Charging_Regime_Buffer;

end Model.Charging.Buffer;
