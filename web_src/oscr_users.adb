with Ada.Containers.Hashed_Maps;
with Model.Charging.IO.Web;
with Model.Charging;
with Model.Main_Examples; -- FIXME: temp import
with Model.OSCR_Output.Generator;
with Model.Run_Settings;
with Model.Charging.Buffer;
with Model.Web_Constants;

package body OSCR_Users is

   package rs   renames Model.Run_Settings;
   package mwcn renames Model.Web_Constants;
   package moo  renames Model.OSCR_Output;
   package rp   renames Model.Charging.Charging_Regime_Package;
   package ap   renames Model.Charging.Application_Package;
   package mc   renames Model.Charging;


   function Compare_Users( left, right : User_Type ) return Boolean is
   begin
      return left = right;
   end Compare_Users;

   package User_Maps is new Ada.Containers.Hashed_Maps(
      Key_Type         => Unbounded_String,
      Element_Type     => User_Type,
      Hash             => Text_Utils.Hash_String,
      "="              => Compare_Users,
      Equivalent_Keys  => Text_Utils.Compare_String );

   users : User_Maps.Map;

   function Validate( 
      username : Unbounded_String; 
      password : Unbounded_String ) return User_Type is
      
      use User_Maps;
      user : User_Type := INVALID_USER;
   begin
      if( Contains( users, username )) then
         user := Element( users, username );
         if( user.password /= password ) then
            user := INVALID_USER;
         end if;
      end if;
      return user;
   end Validate;
   

begin
   User_Maps.Insert( 
       users,
       TuS("a_user"),
       ( username    => TuS("a_user"),
         password    => TuS("xx"),
         title       => TuS("Mr X"),
         description => TuS(""),
         email       => TuS( "xx@xx.xx" ),
         preferences => ( use_svg_graphics=> True, others=> False ) ));
  
end OSCR_Users;
