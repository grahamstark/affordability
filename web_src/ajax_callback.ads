--
--  $Author: graham_s $
--  $Date: 2009-06-18 09:57:13 +0100 (Thu, 18 Jun 2009) $
--  $Revision: 7463 $
--
with AWS.Response;
with AWS.Status;

package Ajax_Callback is

   function Test_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Home_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;

end Ajax_Callback;
