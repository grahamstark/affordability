package org.oscr.charts;

import java.text.*;

public class ChartUtils {
        
        public static int[] range( int start, int stop ){
                int n = stop - start+1;
                int[] out = new int[ n ];
                int p = start;
                for( int i = 0; i < n; i++ ){
                        out[i] = p;
                        p++;
                }
                return out;
        }
        
        /**
         * Radar charts die if passed data with zero length, or all zero data
         * so..
         * @param postValues
         * @param preValues
         * @return  true if either data is of length less than 2, 
         *      or if both have all zero values
         */
        public static boolean unsuitableForRadarChart( double[] postValues, double[] preValues ){
                if( ( postValues.length < 2 ) || ( preValues.length < 2 )){
                        return true;
                }
                int numNonZero = 0; 
                for( double p : postValues ){
                        if( p != 0.0 ) numNonZero++;
                }
                if( numNonZero < 2 ) return true;
                numNonZero = 0; 
                for( double p : preValues ){
                        if( p != 0.0 ) numNonZero++;
                }
                if( numNonZero < 2 ) return true;
                return false;
        }
        
        public static double[] absDifferences( double[] pre, double[] post ){
                int n = pre.length;
                double[] out = new double[ n ];
                for( int i = 0; i < n; i++ ){
                        out[i] = post[i]-pre[i];
                }
                return out;
        }
        
        public static String[] stringRange( int start, int stop ){
                int[] range =  range( start, stop );
                String[] out = new String[ range.length ];
                for( int r = 0; r < range.length; r++ ){
                        out[r] = Integer.toString ( range[r] );
                }
                return out;
        }
        
        private static final DecimalFormat form0 =new DecimalFormat( "###,###,##0" );
        private static final DecimalFormat form1 =new DecimalFormat( "###,###,##0.0" );
        
//      used as a callback for y 
        public static String miniRealFormat( double x ){
                String unit="";
                String out = "";
                if( x > 1000000000.0 ){
                        x /= 1000000000.0;
                        unit = "bn";
                } else if( x > 1000000.0 ){
                        x /= 1000000.0;
                        unit = "mn";
                }
                if (x < 100){
                        out = form1.format(x);
                } else {
                        out = form0.format(x); 
                }
                return out+" "+unit;
        }

        
        public static double[] doubleRange( int start, int stop ){
                int n = stop - start+1;
                double[] out = new double[ n ];
                double p = start;
                for( int i = 0; i < n; i++ ){
                        out[i] = p;
                        p++;
                }
                return out;
        }
        /**
         *@param a - array of numbers
         *@param discount - for example, 5 for a 5% discount per period;
         * 
        */
        public static double discountedSum( double[] a, double discount ){
                double sum = 0.0;
                discount /= 100.0;
                double disc = 1.0;                
                for( double x : a ){
                        sum += x*disc;
                        disc /= (1.0+discount);
                }
                return sum;
        }
        
        public static double discountedSum( double[] a ){
                return discountedSum( a, 0.0 );
        }
        

        public static double average( double[] aa ){
                return discountedSum( aa )/(double)aa.length;
        }

        public static double[] toShares( double[] aa ){
               double s = discountedSum( aa );
               int n = aa.length;
               double[] out = new double[ n ];
               
               for( int i = 0; i < n; i++ ){
                       out [ i ]= ( s != 0.0 ) ? 100.0*aa[i]/s : 0.0;       
               }
               return out;
        }
}
