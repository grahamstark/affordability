   ------------------------------------------------------------------------------
--                                                                          --
--  Handlers for each of OSCR's callbacks, plus some support functions      --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
--                                                                          --
with Ada.Strings.Unbounded;
with Text_Utils;

with AWS.Session;
with AWS.Response;
with AWS.Parameters;
with AWS.Status;
with OSCR_Users;

with Model.Web_Commons;

package OSCR_Callbacks is
   
   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Model.Web_Commons;
   
   function Index_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Input_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Run_Progress_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Serve_File_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Output_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Popup_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Create_Dirname_For_Run( session_id : AWS.Session.Id ) return Unbounded_String; 
   function Logout_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Ajax_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Run_Settings_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Serve_Static_Resource( request : in AWS.Status.Data ) return AWS.Response.Data;
   
end OSCR_Callbacks;
