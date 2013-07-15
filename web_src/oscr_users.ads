with AWS.Session;
with Ada.Strings.Unbounded;
with AWS.Response;
with AWS.Status;
with Text_Utils;

package OSCR_Users is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   
   type Preference_Type is ( use_svg_graphics, other_preference );
   type Preference_Array is array( Preference_Type ) of Boolean;


   type User_Type is record
      username     : Unbounded_String;
      password     : Unbounded_String;
      title        : Unbounded_String;
      description  : Unbounded_String;
      email        : Unbounded_String;
      preferences : Preference_Array := ( others=>False );
   end record;
   
   INVALID_USER : constant User_Type :=
      (username    => TuS("INVALID"),
       password    => TuS("INVALID"),
       title       => TuS("INVALID"),
       description => TuS("INVALID"),
       email       => TuS("INVALID"),
       preferences => ( others=> false ) );

   function Validate( 
      username : Unbounded_String; 
      password : Unbounded_String ) return User_Type;
      
   package User_Session_Data is new AWS.Session.Generic_Data(
       User_Type,
       INVALID_USER );
       
    type Login_Result is record
      user     : User_Type := INVALID_USER;
      response : AWS.Response.Data;
      validated : Boolean := False;
      new_session : Boolean := False;
   end record;

end OSCR_Users;
