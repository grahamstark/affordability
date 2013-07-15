package org.oscr.utils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.io.File;
import java.net.URL;
import java.net.URLConnection;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Calendar;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import org.apache.commons.codec.EncoderException;
import org.apache.commons.codec.net.URLCodec;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;

/**
 * A fairly large collection of general purpose static methods.
 * 
 * @author graham_s
 * @created 10 October 2002
 */
public class Utils{

        public static final String DEFAULT_PROPERTIES_DIR_ID = "wcgproperties";

        /**
         * We can ignore these in some lenient string comparisons.
         */
        private static final String IGNORABLE_CHARS = "\t ?%";

        /**
         * Default string for converting sets to and from Strings.
         */
        public final static char DEFAULT_DELIMITER = ' ';

        private static ThreadLocal GET_URL_ENCODE_POOL = new ThreadLocal();

        private static Random rand;

        /**
         * a 1.4 re for testing for whether a string represents an array matches
         * (e.g fred[200] with group(1) = fred and group(2) = 200)
         */
        public final static Pattern ARRAY_INDEX_EXTRACT = Pattern.compile( " *([\\.\\w]*) *\\[ *(\\w*) *\\]" );

        /**
         * Description of the Field
         */
        public final static double TOL = 0.001;

        /**
         * convert a string to an array of bytes using UTF encoding, and
         * optionally compress it
         * 
         * @param s
         *                string to covert
         * @param compressed
         *                true if you want to compress this string using GZIP
         * @return string as array of bytes
         */
        public static byte[] stringToBytes( String s, boolean compressed ) {
                byte[] buff = null;

                try {
                        ByteArrayOutputStream baos = null;
                        DataOutputStream dos = null;

                        baos = new ByteArrayOutputStream();

                        int buffsize;

                        if (compressed) {
                                GZIPOutputStream gzout = new GZIPOutputStream( new BufferedOutputStream( baos ) );

                                dos = new DataOutputStream( gzout );
                        } else {
                                dos = new DataOutputStream( new BufferedOutputStream( baos ) );
                        }

                        char[] sc = s.toCharArray();

                        dos.writeBytes( s );
                        dos.flush();
                        dos.close();
                        // output's in the string buffer; flush it out to the
                        // output stream & send back
                        buff = baos.toByteArray();
                } catch ( IOException ioe ) {
                        ioe.printStackTrace();
                }
                return buff;
        }

        /**
         * convert a string to an array of bytes using UTF encoding
         * 
         * @param s
         *                string to covert
         * @return string as array of bytes
         */
        public static byte[] stringToBytes( String s ) {
                return stringToBytes( s, false );
        }

        /**
         * reconvert a UTF encoded (and optionally GZIP compressed) byte array
         * into a String
         * 
         * @param buff
         *                input byte array
         * @param compressed
         *                true of bytes are GZIP compressed
         * @return Description of the Return Value
         * @return converted String
         */
        public static String bytesToString( byte[] buff, boolean compressed ) {
                DataInputStream in = null;
                String outStr = null;

                try {
                        ByteArrayInputStream bais = new ByteArrayInputStream( buff );
                        int buffsize;

                        if (compressed) {
                                GZIPInputStream gzin = new GZIPInputStream( new BufferedInputStream( bais ) );

                                in = new DataInputStream( gzin );
                        } else {
                                in = new DataInputStream( new BufferedInputStream( bais ) );
                        }

                        byte[] outbuff = null;

                        in.readFully( outbuff );
                        outStr = new String( outbuff );
                } catch ( IOException ioe ) {
                        ioe.printStackTrace();
                } 
                        return outStr;
        }

        /**
         * reconvert a UTF encoded byte array into a String
         * 
         * @param buff
         *                input byte array
         * @return Description of the Return Value
         * @return converted String
         */
        public static String bytesToString( byte[] buff ) {
                return bytesToString( buff, false );
        }

        /**
         * build a set form an array of keys
         * 
         * @param keys
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static Set buildSet( String[] keys ) {
                Set s = new HashSet();

                s.addAll( Arrays.asList( keys ) );
                return s;
        }

        /**
         * build a set form an array of keys
         * 
         * @param keys
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static Set buildSetExcl( String[] keys, String[] excl ) {

                Set inc = buildSet( keys );
                Set rem = buildSet( excl );
                inc.removeAll( rem );
                return inc;
        }

        /**
         * Description of the Method
         * 
         * @param target
         *                Description of the Parameter
         * @param c
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static int charCount( String target, char c ) {
                int ct = 0;

                for (int i = 0; i < target.length(); i++) {
                        if (target.charAt( i ) == c) {
                                ct++;
                        }
                }
                return ct;
        }

        /**
         * Description of the Method
         * 
         * @param x
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String[] cloneArray( String[] x ) {
                return (String[]) x.clone();
        }

        /**
         * Description of the Method
         * 
         * @param x
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static double[] cloneArray( double[] x ) {
                return (double[]) x.clone();
        }
        
        public static double[] multiplyArray( double[] x, double m ){
                if( m == 1.0 ){
                        return x;       
                }
                int n = x.length;
                for( int i = 0; i < n; i++ ){
                        x[i] *= m;       
                }
                return x;
        }
        
        public static double[] divideArray( double[] x, double m ){
                
                if( m == 1.0 ){
                        return x;       
                }
                int n = x.length;
                for( int i = 0; i < n; i++ ){
                        x[i] /= m;       
                }
                return x;
        }

        /**
         * Description of the Method
         * 
         * @param x
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static boolean[] cloneArray( boolean[] x ) {
                return (boolean[]) x.clone();
        }

        /**
         * Description of the Method
         * 
         * @param x
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static int[] cloneArray( int[] x ) {
                return (int[]) x.clone();
        }

        /**
         * Description of the Method
         * 
         * @param x
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static char[] cloneArray( char[] x ) {
                return (char[]) x.clone();
        }

        /**
         * Description of the Method
         * 
         * @param x
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static long[] cloneArray( long[] x ) {
                return (long[]) x.clone();
        }

        /**
         * Description of the Method
         * 
         * @param s
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static Set cloneSet( Set s ) {
                Set news = null;

                if (s instanceof HashSet) {
                        news = new HashSet();
                } else {
                        news = new TreeSet();
                }
                news.addAll( s );
                return news;
        }

        /**
         * Description of the Method
         * 
         * @param s
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static List cloneList( List s ) {
                List newl = new ArrayList();

                newl.addAll( s );
                return newl;
        }

        // public static Object[] cloneArray( Object[] x ) throws
        // CloneNotSupportedException{
        // Object[] o = new Object[ x.length ];
        // for( int i = 0; i < x.length; i++ ){
        // o[ i ] = x[ i ].clone( );
        // }
        // return o;
        // }
        /**
         * @param prefix
         *                string stuck to front
         * @return The randomString value
         * @return a random Number, actually, with prefix stuck on the front
         */
        public static String getRandomString( String prefix ) {
                if (rand == null) {
                        rand = new Random();
                }
                return prefix + rand.nextInt();
        }

        /**
         * @return The randomString value
         * @return a random Number as a string
         */
        public static String getRandomString() {
                return getRandomString( "" );
        }

        /**
         * turn something like fred[1]=99;fred[2]=100;ff=10;xxx.xx = 20; into a
         * map, with the bits before the "=" as the keys and the bits afterwords
         * as the data. Arrays are turned into sub-maps keyed by the bit in
         * brackets, with the whole array keyed by (e.g. fred)
         * 
         * @param pr2
         *                a pr2 string, in the form "x=10;y=20;x[2]=1001" etc.
         * @return line split at ";"s with bit before "=" as key and bit after
         *         as data
         */
        public static Map pr2ToMap( String pr2 ) {
                String[] elems = pr2.split( ";" );
                Map m = new HashMap();
                for (int i = 0; i < elems.length; i++) {
                        String thisElem = elems[i].trim();
                        // test to make sure we ignore stray ";"s
                        if (thisElem.length() > 0) {
                                String[] v = thisElem.split( "=" );
                                String key = v[0].trim();
                                String data = v[1].trim();
                                // now, handle arrays
                                Matcher matcher = ARRAY_INDEX_EXTRACT.matcher( key );
                                if (matcher.find() && ( key.endsWith( "]" ) )) {
                                        String index = matcher.group( 2 );
                                        key = matcher.group( 1 );
                                        Map array = null;
                                        if (m.containsKey( key )) {
                                                array = (Map) m.get( key );
                                        } else {
                                                array = new LinkedHashMap();
                                        }
                                        array.put( index, data );
                                        m.put( key, array );
                                } else {
                                        m.put( key, data );
                                }
                        }
                }
                return m;
        }

        /**
         * classloader associated with the current thread.
         * 
         * @param fallbackClass
         * @return
         */
        public static ClassLoader getCurrentLoader( Object fallbackClass ) {
                ClassLoader loader = Thread.currentThread().getContextClassLoader();
                if (loader == null) {
                        loader = fallbackClass.getClass().getClassLoader();
                }
                return loader;
        }

        /**
         * make a url from a name, using the current thread's classloader.
         * 
         * @param fileName
         * @return
         */
        public static URL urlFromFileName( String fileName ) {
                ClassLoader loader = getCurrentLoader( Utils.class );
                URL url = null;
                if (fileName == null) {
                        return null;
                }
                if (loader == null) {
                        url = Utils.class.getResource( fileName );
                } else {
                        url = loader.getResource( fileName );
                }
                return url;
        }

        /**
         * open a text file for reading using URL mechanism. Should work even
         * with files in e.g. a JAR and .. opens as a file if not
         * 
         * @param fname
         *                Description of the Parameter
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         */
        public static BufferedReader openURLFile( String fname ) throws IOException {
                BufferedReader def = null;
                try {
                        URL defURL = urlFromFileName( fname );
                        if (defURL == null) { // fallback: take url name
                                                // literally.
                                defURL = new URL( fname );
                        }
                        // ClassLoader.getSystemResource( fname );
                        URLConnection u = defURL.openConnection();
                        InputStream ins = u.getInputStream();
                        InputStreamReader inp = new InputStreamReader( ins );
                        def = new BufferedReader( inp );
                } catch ( IOException ioe ) {
                        def = new BufferedReader( new FileReader( fname ) );

                }
                return def;
        }

        /**
         * open a text file for reading using URL mechanism. Should work even
         * with files in e.g. a JAR
         * 
         * @param fname
         *                Description of the Parameter
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         */
        public static BufferedReader openFile( String fname ) throws IOException {
                return new BufferedReader( new FileReader( fname ) );
        }

        public static String readTextFile( File f, boolean isURL ) throws IOException {
                return readTextFile( f.toString(), false );
        }

        /**
         * read a text file into a big string this version works(?) even within
         * jar/tar files
         * 
         * @param fname
         *                Description of the Parameter
         * @param isURL
         *                Description of the Parameter
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         */
        public static String readTextFile( String fname, boolean isURL ) throws IOException {
                BufferedReader def = null;

                if (isURL) {
                        def = openURLFile( fname );
                } else {
                        def = new BufferedReader( new FileReader( fname ) );
                }

                StringBuffer outBuff = new StringBuffer();
                String line;

                while ( ( line = def.readLine() ) != null ) {
                        outBuff.append( line );
                        outBuff.append( "\n" );
                }
                return outBuff.toString();
        }

        /**
         * count lines in a string this version works(?) even within
         * 
         * @param fname
         *                Description of the Parameter
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         */
        public static int lineCount( String fname ) throws IOException {
                String line;
                BufferedReader def = openURLFile( fname );
                int count = 0;

                while ( ( line = def.readLine() ) != null ) {
                        count++;
                }
                return count;
        }

        /**
         * substring returning "" or n --> end if start & stop out of range
         * 
         * @param in
         * @param start
         * @param stop
         * @return Description of the Return Value
         */
        public static String safeSubstring( String in, int start, int stop ) {
                int l = in.length();
                String s = "";

                if (start < l) {
                        if (stop < l) {
                                s = in.substring( start, stop );
                        } else {
                                s = in.substring( start );
                        }
                }
                return s;
        }

        /**
         * read lines in file but return result as an array
         * 
         * @param fname
         *                file to open jar/tar files
         * @param isURL
         *                Description of the Parameter
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         */
        public static String[] readTextFileA( String fname, boolean isURL ) throws IOException {

                BufferedReader def;
                if (isURL) {
                        def = openURLFile( fname );
                } else {
                        def = new BufferedReader( new FileReader( fname ) );
                }

                List l = new ArrayList();
                String line;
                int i = 0;
                while ( ( line = def.readLine() ) != null ) {
                        l.add( line );
                        if (( i % 1000 ) == 0) {
                                System.err.println( "line[" + i + "]=" + line );
                        }
                        i++;
                }
                System.err.println( "readTextFileA; created List " );
                String[] out = (String[]) l.toArray( new String[l.size()] );
                /**
                 * String[] out = new String[l.size()];
                 * 
                 * for ( int i = 0; i < l.size(); i++ ) { out[i] = ( String )
                 * l.get( i ); }
                 */
                return out;
        }

        /**
         * merge files lengthways e.g. bits of csv files
         * 
         * @param fnames
         *                names of files to merge
         * @param delim
         *                field delimiter (appended to end of each file except
         *                the last)
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         * @return array of strings, each line of which has line i of
         *         files[0..n] appended
         */
        public static String[] mergeTextFiles( String[] fnames, char delim ) throws IOException {
                int bufflen = lineCount( fnames[0] );
                StringBuffer[] sb = new StringBuffer[bufflen];

                for (int i = 0; i < bufflen; i++) {
                        sb[i] = new StringBuffer();
                }
                for (int fno = 0; fno < fnames.length; fno++) {
                        String[] one = readTextFileA( fnames[fno], false );

                        for (int i = 0; i < one.length; i++) {
                                sb[i].append( one[i] );
                                if (fno < fnames.length - 1) {
                                        sb[i].append( delim );
                                }
                        }
                }

                String[] out = new String[bufflen];

                for (int i = 0; i < bufflen; i++) {
                        out[i] = sb[i].toString();
                }
                return out;
        }

        /**
         * merge files lengthways e.g. bits of csv files with "," appended to
         * each but the last
         * 
         * @param fnames
         *                names of files to merge
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         * @return array of strings, each line of which has line i of
         *         files[0..n] appended
         */
        public static String[] mergeTextFiles( String[] fnames ) throws IOException {
                return mergeTextFiles( fnames, ' ' );
        }

        /**
         * does exactly that
         * 
         * @param inf
         *                input file name
         * @param outf
         *                output file name
         * @exception IOException
         *                    Description of the Exception
         */
        public static void copyFile( String inf, String outf ) throws IOException {
                BufferedReader in = new BufferedReader( new FileReader( inf ) );
                File outfile = new File( outf );

                BufferedWriter out = new BufferedWriter( new FileWriter( outf ) );
                String line;

                while ( ( line = in.readLine() ) != null ) {
                        out.write( line );
                        out.write( "\n" );
                }
                in.close();
                out.close();

        }

        /**
         * write some text out to a file and close the file
         * 
         * @param buff :
         *                what to write
         * @param fname :
         *                where to write it
         * @exception IOException
         *                    Description of the Exception
         */
        public static void blastOutFile( String buff, String fname ) throws IOException {
                blastOutFile( buff, new File( fname ) );
        }

        /**
         * write some text out to a file and close the file
         * 
         * @param buff :
         *                what to write
         * @param f
         *                Description of the Parameter
         * @exception IOException
         *                    Description of the Exception
         */
        public static void blastOutFile( String buff, File f ) throws IOException {
                BufferedWriter bw = new BufferedWriter( new FileWriter( f ) );

                bw.write( buff );
                bw.write( "\n" );
                bw.flush();
                bw.close();

        }

        /**
         * does exactly that
         * 
         * @param fileName
         *                Description of the Parameter
         * @param top
         *                Description of the Parameter
         * @param tail
         *                Description of the Parameter
         * @param isURL
         *                Description of the Parameter
         * @exception IOException
         *                    Description of the Exception
         */
        public static void topAndTail( String fileName, String top, String tail, boolean isURL ) throws IOException {
                String ins = readTextFile( fileName, isURL );
                StringBuffer sb = new StringBuffer();

                sb.append( top );
                sb.append( "\n" );
                sb.append( ins );
                sb.append( "\n" );
                sb.append( tail );
                sb.append( "\n" );

                BufferedWriter br = new BufferedWriter( new FileWriter( fileName ) );

                br.write( sb.toString() );
                br.flush();
                br.close();
        }
        
        public static double min( double[] v ){
                double mn = Double.MAX_VALUE;
                for( double val : v ){
                        if( val < mn ){
                                mn = val;
                        }
                }
                return mn;
        }

        public static double max( double[] v ){
                double mn = Double.MIN_VALUE;
                for( double val : v ){
                        if( val > mn ){
                                mn = val;
                        }
                }
                return mn;
        }
        
        static final double MIN_SENSIBLE_REAL = 0.0001;

        /**
         * Description of the Method
         * 
         * @param m
         *                Description of the Parameter
         * @param order
         *                Description of the Parameter
         * @param separator
         *                Description of the Parameter
         * @param icsInHeader
         *                Description of the Parameter
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         */
        public static String CDAToString( Map m, List order, char separator, boolean icsInHeader ) throws IOException {
                Writer bw = new StringWriter();
                int j = 0;
                int l = 0;
                List newOrder = new ArrayList();

                newOrder.addAll( order );
                // idiot check: delete things in order list not in map
                for (int i = 0; i < order.size(); i++) {
                        String key = (String) order.get( i );

                        if (!m.containsKey( key )) {
                                newOrder.remove( key );
                        }
                }

                double[][] x = new double[newOrder.size()][];
                // boolean[] isDouble = new boolean[ newOrder.size() ];
                for (int i = 0; i < newOrder.size(); i++) {
                        String key = (String) newOrder.get( i );
                        // if( doubles.contains( key ) ) isDouble[i] = true;
                        x[i] = (double[]) m.get( key );
                        if (icsInHeader) {
                                bw.write( "\"" );
                        }
                        bw.write( key );
                        if (icsInHeader) {
                                bw.write( "\"" );
                        }

                        if (i < order.size() - 1) {
                                bw.write( separator );
                        }
                }
                bw.write( "\n" );
                for (int r = 0; r < x[0].length; r++) {
                        for (int c = 0; c < x.length; c++) {
                                bw.write( Double.toString( x[c][r] ) );

                                if (c < ( x.length - 1 )) {
                                        bw.write( separator );
                                }
                        }
                        bw.write( "\n" );
                }
                return ( (StringWriter) bw ).getBuffer().toString();
        }

        /**
         * Description of the Method
         * 
         * @param outf
         *                Description of the Parameter
         * @param m
         *                Description of the Parameter
         * @param order
         *                Description of the Parameter
         * @param separator
         *                Description of the Parameter
         * @exception IOException
         *                    Description of the Exception
         */
        public static void writeCDAMap( String outf, Map m, List order, char separator ) throws IOException {
                BufferedWriter bw = new BufferedWriter( new FileWriter( outf ) );
                bw.write( CDAToString( m, order, separator, false ) );
                bw.flush();
                bw.close();
        }

        /**
         * Description of the Method
         * 
         * @param outf
         *                Description of the Parameter
         * @param m
         *                Description of the Parameter
         * @param order
         *                Description of the Parameter
         * @exception IOException
         *                    Description of the Exception
         */
        public static void writeCDAMap( String outf, Map m, List order ) throws IOException {
                BufferedWriter bw = new BufferedWriter( new FileWriter( outf ) );

                bw.write( CDAToString( m, order, ',', false ) );
                bw.flush();
                bw.close();
        }

        /**
         * Description of the Method
         * 
         * @param a
         *                Description of the Parameter
         * @param b
         *                Description of the Parameter
         * @param tol
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static boolean nearlyEqual( double a, double b, double tol ) {
                double x = Math.abs( a - b );

                return x < tol;
        }

        /**
         * Description of the Method
         * 
         * @param a
         *                Description of the Parameter
         * @param b
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static boolean nearlyEqual( double a, double b ) {
                return nearlyEqual( a, b, TOL );
        }

        /**
         * The number x rounded up to the next tol if x is not a whole number.
         * So:
         * <ul>
         * <li>roundUp( 100.1, 10 ) == 110;
         * <li>roundUp( 100.0, [anything] ) == 100.0;
         * </ul>
         * 
         * @param x
         * @param next
         * @return the number x rounded up to the next tol.
         */
        public static Double roundUp( Double x, Double next ) {
                Double rd = Math.floor( x );
                if (rd != x) {
                        rd += next;
                }
                return rd;
        }

        /**
         * write the contents of a stream to a string
         * 
         * @param inp
         *                Description of the Parameter
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         */
        public static String captureStream( InputStream inp ) throws IOException {
                StringBuffer sb = new StringBuffer();
                String s = "";
                BufferedReader in = new BufferedReader( new InputStreamReader( inp ), 80000 );

                while ( ( s = in.readLine() ) != null ) {
                        sb.append( s );
                        sb.append( "\n" );
                }
                in.close();
                return sb.toString();
        }

        /**
         * includes the contents of a file at current point
         * 
         * @param sb
         *                buffer to append to
         * @param fname
         *                file to include
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         */
        public static StringBuffer includeFile( StringBuffer sb, String fname ) throws IOException {
                BufferedReader in = new BufferedReader( new FileReader( fname ) );
                String l;

                while ( ( l = in.readLine() ) != null ) {
                        sb.append( l );
                        sb.append( "\n" );
                }
                return sb;
        }

        /**
         * includes the contents of a file at current point
         * 
         * @param s
         *                string to append to
         * @param fname
         *                file to include
         * @return Description of the Return Value
         * @exception IOException
         *                    Description of the Exception
         */
        public static String includeFile( String s, String fname ) throws IOException {
                StringBuffer sb = new StringBuffer( s );

                includeFile( sb, fname );
                return sb.toString();
        }

        /**
         * includes the contents of a file at current point
         * 
         * @param fname
         *                file to include
         * @param bw
         *                Description of the Parameter
         * @exception IOException
         *                    Description of the Exception
         */
        public static void includeFile( BufferedWriter bw, String fname ) throws IOException {
                StringBuffer sb = new StringBuffer();

                includeFile( sb, fname );
                bw.write( sb.toString() );
        }

        /**
         * ** extracts the rightmost part of a string like "tax.fred.inc.axe"
         * getDepthString( "tax.fred.inc.axe" , 2 ) => inc.axe
         * 
         * @param s
         *                dot delimited string
         * @param depth
         *                Description of the Parameter
         * @return The depthString value
         * @return "tax.fred.inc.axe" , 2 ) => inc.axe , for instance
         */
        public static String getDepthString( String s, int depth ) {
                if (depth <= 0) {
                        return "";
                }
                String ch;
                Vector ch2 = new Vector();
                int p = s.length() - 1;
                int i = depth;

                while ( ( i > 0 ) && ( p >= 0 ) ) {
                        ch = s.substring( p, p + 1 );
                        if (ch.equals( "." )) {
                                i--;
                        }
                        if (i > 0) {
                                ch2.insertElementAt( ch, 0 );
                        }
                        p--;
                }

                StringBuffer sb = new StringBuffer();

                for (i = 0; i < ch2.size(); i++) {
                        sb.append( (String) ch2.elementAt( i ) );
                }
                return sb.toString();
        }

        /**
         * Description of the Method
         * 
         * @param ind
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String indent( int ind ) {
                StringBuffer sb = new StringBuffer();

                for (int i = 0; i < ind; i++) {
                        sb.append( "     " );
                }
                return sb.toString();
        }

        /**
         * Convert comma-delimied line to doubles with essentially no checking.
         * 
         * @param dataStr - string of comma-delimited strings each representing a real.
         * @return an array of reals, or {0} if the string is null or zero-length
         */
        public static double[] simpleLineToDoubles( String dataStr ) {
                if( dataStr == null ){
                        return new double[]{0};                
                }
                dataStr = dataStr.trim();
                if( dataStr.length() == 0 ){
                        return new double[]{0};                
                }
                String[] ss = dataStr.split( "," );
                int n = ss.length;
                double[] dd = new double[n];
                for (int i = 0; i < n; i++) {
                        dd[i] = Double.parseDouble( ss[i] );
                }
                return dd;
        }
        
        /**
         * Description of the Method
         * 
         * @param s
         *                Description of the Parameter
         * @param delim
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static double[] lineToDoubles( String s, String delim ) {
                StringTokenizer tok;

                if (delim == null) {
                        tok = new StringTokenizer( s );
                } else {
                        tok = new StringTokenizer( s, delim );
                }

                Vector v = new Vector();

                try {
                        while ( tok.hasMoreTokens() ) {
                                v.addElement( new Double( tok.nextToken() ) );
                        }
                } catch ( NumberFormatException nfe ) {
                        nfe.printStackTrace();
                }

                double[] x = new double[v.size()];

                for (int i = 0; i < v.size(); i++) {
                        x[i] = ( (Double) v.elementAt( i ) ).doubleValue();
                }
                return x;
        }

        /**
         * Description of the Method
         * 
         * @param ins
         *                Description of the Parameter
         * @param delim
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String[] lineToStrings( String ins, String delim ) {
                StringTokenizer tok;

                if (delim == null) {
                        tok = new StringTokenizer( ins );
                } else {
                        tok = new StringTokenizer( ins, delim );
                }

                Vector v = new Vector();

                while ( tok.hasMoreTokens() ) {
                        v.addElement( tok.nextToken() );
                }

                String[] s = new String[v.size()];

                for (int i = 0; i < v.size(); i++) {
                        s[i] = (String) v.elementAt( i );
                }
                return s;
        }

        /**
         * Description of the Method
         * 
         * @param ins
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String[] lineToStrings( String ins ) {
                return lineToStrings( ins, null );
        }

        /**
         * Description of the Method
         * 
         * @param ins
         *                Description of the Parameter
         * @param start
         *                Description of the Parameter
         * @param stop
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String stringsToLine( String[] ins, int start, int stop ) {
                StringBuffer sb = new StringBuffer();

                for (int i = start; i < stop; i++) {
                        sb.append( ins[i] );
                        if (i < ( stop - 1 )) {
                                sb.append( " " );
                        }
                }
                return sb.toString();
        }

        /**
         * Description of the Method
         * 
         * @param ins
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String stringsToLine( String[] ins ) {
                return stringsToLine( ins, 0, ins.length );
        }

        /**
         * cod-XML extractor; given an enumeration of some sort with: <FRED> 112
         * 223 </FRED> extracts an array ["112],"223" ]
         * 
         * @param e
         *                Description of the Parameter
         * @param start
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String[] linesBetween( Enumeration e, String start ) {
                Vector v = new Vector();
                String end = "/" + start;
                boolean done = false;
                String str;
                while ( !done ) {
                        str = (String) e.nextElement();
                        if (( !e.hasMoreElements() ) || ( str.indexOf( start ) >= 0 )) {
                                done = true;
                        }
                }
                done = false;
                while ( !done ) {
                        str = (String) e.nextElement();
                        if (( !e.hasMoreElements() ) || ( str.indexOf( end ) >= 0 )) {
                                done = true;
                        } else {
                                v.addElement( str );
                        }
                }
                String[] s = new String[v.size()];
                for (int i = 0; i < v.size(); i++) {
                        s[i] = (String) v.elementAt( i );
                }
                return s;
        }

        /**
         * Description of the Method
         * 
         * @param s
         *                Description of the Parameter
         * @param start
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static Vector vectorBetween( String s, String start ) {
                Vector v = new Vector();
                Enumeration e = stringToVector( s ).elements();
                String[] sa = linesBetween( e, start );

                for (int i = 0; i < sa.length; i++) {
                        v.add( sa[i] );
                }
                return v;
        }

        /**
         * Description of the Method
         * 
         * @param s
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static Vector stringToVector( String s ) {
                Vector v = new Vector();

                try {
                        BufferedReader sbi = new BufferedReader( new StringReader( s ) );
                        String l;

                        while ( ( l = sbi.readLine() ) != null ) {
                                v.addElement( l );
                        }
                        return v;
                } catch ( IOException ioe ) {
                        ioe.printStackTrace();
                } 
                return v;
        }

        /**
         * Description of the Method
         * 
         * @param sb
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static Vector stringToVector( StringBuffer sb ) {
                return stringToVector( sb.toString() );
        }

        /**
         * Description of the Method
         * 
         * @param e
         *                Description of the Parameter
         * @param start
         *                Description of the Parameter
         * @param delim
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static double[][] doublesBetween( Enumeration e, String start, String delim ) {
                String[] s = linesBetween( e, start );
                int l = lineToStrings( s[0], delim ).length;
                double[][] x = new double[s.length][l];
                double[] xx;

                for (int i = 0; i < s.length; i++) {
                        xx = lineToDoubles( s[i], delim );
                        for (int j = 0; j < xx.length; j++) {
                                x[i][j] = xx[j];
                        }
                }
                return x;
        }

        /**
         * Description of the Method
         * 
         * @param e
         *                Description of the Parameter
         * @param start
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static double[][] doublesBetween( Enumeration e, String start ) {
                return doublesBetween( e, start, null );
        }

        /**
         * Description of the Method
         * 
         * @param x
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static Double[][] dToD( double[][] x ) {
                Double[][] dx = new Double[x.length][x[0].length];

                for (int i = 0; i < x.length; i++) {
                        for (int j = 0; j < x[0].length; j++) {
                                dx[i][j] = new Double( x[i][j] );
                        }
                }
                return dx;
        }

        /**
         * Description of the Method
         * 
         * @param x
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static double[][] DTod( Double[][] x ) {
                double[][] dx = new double[x.length][x[0].length];

                for (int i = 0; i < x.length; i++) {
                        for (int j = 0; j < x[0].length; j++) {
                                dx[i][j] = x[i][j].doubleValue();
                        }
                }
                return dx;
        }

        /**
         * Description of the Method
         * 
         * @param b
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static int onCount( BitSet b ) {
                int c = 0;

                for (int i = 0; i < b.size(); i++) {
                        if (b.get( i )) {
                                c++;
                        }
                }
                return c;
        }

        /**
         * @param s
         *                string to be capitalised
         * @return Description of the Return Value
         * @return (e.g) stark => Stark
         */
        public static String capitalise( String s ) {
                if (( s == null ) || ( s.length() == 0 )) {
                        return s;
                }
                StringBuffer sb = new StringBuffer( s );
                char c = Character.toUpperCase( sb.charAt( 0 ) );

                sb.setCharAt( 0, c );
                return sb.toString();
        }

        /**
         * Description of the Method
         * 
         * @param b
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String bsToString( BitSet b ) {
                StringBuffer sb1 = new StringBuffer();
                StringBuffer sb2 = new StringBuffer();
                int i;

                for (i = 0; i < b.size(); i++) {
                        if (i < 10) {
                                sb1.append( " " );
                        }
                        if (i < 100) {
                                sb1.append( " " );
                        }
                        sb1.append( i );
                        if (b.get( i )) {
                                sb2.append( " 1 " );
                        } else {
                                sb2.append( " 0 " );
                        }
                }
                sb1.append( "\n" );
                sb1.append( sb2 );
                return sb1.toString();
        }

        /**
         * @param key
         * @param value
         * @return one key/value pair suitable for insertion in a url string
         */
        public static String makeUrlElement( String key, Object value ) {
                StringBuffer sb = new StringBuffer();
                if (value != null) {
                        try {
                                URLCodec codec = getURLEncoder();
                                sb.append( codec.encode( key ) );
                                sb.append( "=" );
                                sb.append( codec.encode( value.toString() ) );
                        } catch ( EncoderException e ) {
                                // logger.error("URL Encoding blew up " + e);
                        }
                }
                return sb.toString();
        }

        /**
         * Test if String s1 is the same as String s2, excluding any whitespace,
         * and puncuation.
         * 
         * @see http://java.sun.com/j2se/1.5.0/docs/api/java/util/regex/Pattern.html
         *      for the exact chars removed before testing.
         * 
         * @param s1
         * @param s2
         * @param ignorecase
         * @return true if s1 is the same as s2, excluding any whitespace (tabs
         *         and spaces), or punctuation
         */
        public static boolean sameExcludingWhitespace( String s1, String s2, boolean ignorecase ) {
                boolean eq = false;
                if (s1 == s2) { // both same string or both null
                        eq = true;
                } else if (( s1 == null ) || ( s2 == null )) { // one null
                        eq = false;
                } else {
                        String sn1 = s1.replaceAll( "\\s", "" ).replaceAll( "\\p{Punct}", "" );
                        String sn2 = s2.replaceAll( "\\s", "" ).replaceAll( "\\p{Punct}", "" );
                        if (ignorecase) {
                                eq = sn1.equalsIgnoreCase( sn2 );
                        } else {
                                eq = sn1.equals( sn2 );
                        }
                }
                return eq;
        }

        /**
         * using the threadlocal pattern; see (e&#046;g&#046;)
         * http://www.oreilly.com/catalog/javapt/chapter/ch04.html
         * 
         * @return a pattern to retrieve a URL encoder.
         */
        public static URLCodec getURLEncoder() {
                // Lazily initialized version. Get the thread local object
                URLCodec p = (URLCodec) GET_URL_ENCODE_POOL.get();
                if (p == null) {
                        // First time. So create a formatter and set the
                        // ThreadLocal
                        p = new URLCodec();
                        GET_URL_ENCODE_POOL.set( p );
                }
                return p;
        }

        /**
         * using the threadlocal pattern; see, for example:
         * http://www.oreilly.com/catalog/javapt/chapter/ch04.html.
         * 
         * @return a pattern to retrieve a URL encoder.
         */
        /*
         * public static DocumentFactory getDocumentFactory() { //Lazily
         * initialized version. Get the thread local object DocumentFactory p =
         * (DocumentFactory) GET_DOCUMENT_FACTORY_POOL.get(); if ( p == null ) {
         * //First time. So create a formatter and set the ThreadLocal p =
         * documentFactory = new DocumentFactory();
         * GET_DOCUMENT_FACTORY_POOL.set(p); } return p; }
         */
        /**
         * Bloch-like hiding of the constructor, since all the methods are
         * private.
         */
        private Utils () {
                throw new UnsupportedOperationException();
                // prevents calls from subclass
        }

        /**
         * Produce a simple text representation of a set, using the given
         * demiliter
         * 
         * @param delimiter -
         *                character to delimit each set element.
         * @param set
         *                The new toString value
         * @return a simple text representation of a set, using the given
         *         demiliter
         */
        public static String setToString( Collection set, char delimiter ) {
                return setToString( set, delimiter, "" );
        }

        public static String setToString( Collection set, String delimiter ) {
                return setToString( set, delimiter, "" );
        }

        public static String setToString( Collection set, char delimiter, String stringDelim ) {
                String s = String.valueOf( delimiter );
                return setToString( set, s, stringDelim );
        }

        /**
         * Produce a simple text representation of a set, using the given
         * demiliter
         * 
         * @param delimiter -
         *                character to delimit each set element.
         * @param set
         *                The new toString value
         * @param stringDelim
         *                delimit strings with this
         * @return a simple text representation of a set, using the given
         *         demiliter
         */
        public static String setToString( Collection set, String delimiter, String stringDelim ) {
                StringBuffer sb = new StringBuffer();
                for (Iterator iter = set.iterator(); iter.hasNext();) {
                        Object o = iter.next();
                        if (o instanceof String) {
                                sb.append( stringDelim );
                                sb.append( o );
                                sb.append( stringDelim );
                        } else {
                                sb.append( o );
                        }
                        if (iter.hasNext()) {
                                sb.append( delimiter );
                        }
                }
                return sb.toString();
        }

        /**
         * Produce an array of strings from the given set
         * 
         * @param set
         *                The new toString value
         * @return an array of strings from the given seta
         */
        public static String[] setToArray( final Set set ) {
                String[] s = null;
                if (set != null) {
                        int i = 0;
                        s = new String[set.size()];
                        for (Iterator iter = set.iterator(); iter.hasNext();) {
                                s[i] = iter.next().toString();
                                i++;
                        }
                }
                return s;
        }

        /**
         * Produce a simple text representation of a set, using a space
         * demiliter
         * 
         * @param set
         *                The new toString value
         * @return a simple text representation of a set, using the given
         *         demiliter
         */
        public static String setToString( final Set set ) {
                return setToString( set, ' ' );
        }

        /**
         * Create a Timestamp from day,month,year
         * 
         * @param day
         *                the day
         * @param month
         *                the month
         * @param year
         *                the year
         * @return The timestamp value
         */
        public static Timestamp getTimestamp( int day, int month, int year ) {
                Calendar expiry = Calendar.getInstance();
                expiry.clear();
                expiry.setLenient( false );
                expiry.set( Calendar.MONTH, month - 1 );
                expiry.set( Calendar.YEAR, year );
                expiry.set( Calendar.DAY_OF_MONTH, day );
                return new Timestamp( expiry.getTimeInMillis() );
        }

        /**
         * Create a Timestamp from day,month,year,hour,minute
         * 
         * @param day
         *                the day
         * @param month
         *                the month
         * @param year
         *                the year
         * @param hour
         *                the hour
         * @param minute
         *                the minute
         * @return The timestamp value
         */
        public static Timestamp getTimestamp( int day, int month, int year, int hour, int minute ) {
                Calendar expiry = Calendar.getInstance();
                expiry.clear();
                expiry.setLenient( false );
                expiry.set( Calendar.MONTH, month - 1 );
                expiry.set( Calendar.YEAR, year );
                expiry.set( Calendar.DAY_OF_MONTH, day );
                expiry.set( Calendar.HOUR_OF_DAY, hour );
                expiry.set( Calendar.MINUTE, minute );
                return new Timestamp( expiry.getTimeInMillis() );
        }

        /**
         * pad a string out
         * 
         * @param s
         *                input string
         * @param padChar
         *                char to pad out with
         * @param length
         *                new length
         * @param right
         *                pad to left/right
         * @return Description of the Return Value
         * @return string padded
         */
        public static String pad( String s, char padChar, int length, boolean right ) {
                int l = s.length();
                if (l >= length) {
                        return s;
                }
                StringBuffer sb = new StringBuffer( s );
                for (int i = l; i <= length; i++) {
                        if (right) {
                                sb.append( padChar );
                        } else {
                                sb.insert( 0, padChar );
                        }
                }
                return sb.toString();
        }

        /**
         * Invert the rows and columns of a matrix. Do I mean transpose? So each
         * row is a column and vice-versa.
         * 
         * @param x
         *                input n by m matrix
         * @return the matrix inverted (or transposed, as the case may be).
         */
        public static double[][] invert( double[][] x ) {
                double[][] y = new double[x[0].length][x.length];
                for (int i = 0; i < x.length; i++) {
                        for (int j = 0; j < x[0].length; j++) {
                                y[j][i] = x[i][j];
                        }
                }
                return y;
        }

        /**
         * pad a string left with blanks
         * 
         * @param s
         *                input string
         * @param len
         *                Description of the Parameter
         * @return Description of the Return Value
         * @return string padded left with blanks
         */
        public static String padLeft( String s, int len ) {
                return pad( s, ' ', len, false );
        }

        /**
         * pad a string right with blanks
         * 
         * @param s
         *                input string
         * @param len
         *                Description of the Parameter
         * @return Description of the Return Value
         * @return string padded left with blanks
         */
        public static String padRight( String s, int len ) {
                return pad( s, ' ', len, true );
        }

        /**
         * Does exactly what it says on the tin.
         * 
         * @param s -
         *                string to be checked.
         * @return true of the input string is not null and has a length of &gt;
         *         0
         */
        public static boolean notNullOrZero( final String s ) {
                return ( ( s != null ) && ( s.length() > 0 ) );
        }

        /**
         * Description of the Method
         * 
         * @param fname
         *                Description of the Parameter
         * @param data
         *                Description of the Parameter
         * @exception IOException
         *                    Description of the Exception
         */
        public static void writeFile( String fname, String data ) throws IOException {
                BufferedWriter br = new BufferedWriter( new FileWriter( fname ) );
                br.write( data );
                br.flush();
                br.close();
        }

        /**
         * make a directory path, ignoring any errors.
         * 
         * @param path
         *                (say: c:\fred\joe or /home/fred/joe)..
         */
        public static void mkDirs( String path ) {
                File f = new File( path );
                f.mkdirs();
        }

        /**
         * Build a JDOM document from an XML string FIXME: (a) to new static
         * class? (b) why no exceptions??
         * 
         * @param aRootElement
         *                a String of valid XML
         * @param validate -
         *                true if you want to validate your xml
         * @return a JDOM document
         */
        public static Document makeDom4JDocumentFromFile( String docFile, boolean validate ) throws DocumentException {
                SAXReader xmlReader = new SAXReader();
                xmlReader.setValidation( validate );
                File f = new File( docFile );
                Document doc = null;
                doc = xmlReader.read( f );
                return doc;
        }

        /**
         * Build a JDOM document from an XML string FIXME: (a) to new static
         * class? (b) why no exceptions??
         * 
         * @param aRootElement
         *                a String of valid XML
         * @param validate -
         *                true if you want to validate your xml
         * @return a JDOM document
         */
        public static Document makeDom4JDocumentFromUrl( URL url, boolean validate ) throws DocumentException {
                SAXReader xmlReader = new SAXReader();
                xmlReader.setValidation( validate );
                Document doc = null;
                doc = xmlReader.read( url );
                return doc;
        }

        /**
         * Build a JDOM document from an XML string FIXME: (a) to new static
         * class? (b) why no exceptions??
         * 
         * @param aRootElement
         *                a String of valid XML
         * @return a JDOM document
         */
        public static Document makeDom4JDocumentFromFile( String docFile ) throws DocumentException {
                return makeDom4JDocumentFromFile( docFile, false );
        }

        /**
         * 
         * @param docStr
         *                a String of valid XML
         * @param validate -
         *                true if you want to validate your xml
         * @return a DOM4J document
         */
        public static Document makeDom4JDocumentFromString( String docStr, boolean validate ) throws DocumentException {
                SAXReader xmlReader = new SAXReader();
                Document doc = null;
                xmlReader.setValidation( validate );
                StringReader rd = new StringReader( docStr );
                doc = xmlReader.read( rd );
                return doc;
        }

        /**
         * 
         * @param docStr
         *                a String of valid XML
         * @return a DOM4J document
         */
        public static Document makeDom4JDocumentFromString( String docStr ) throws DocumentException {
                return makeDom4JDocumentFromString( docStr, false );
        }

        /**
         * Move a file from fromName to toName. Copied from Java almanack, as
         * ever: http://javaalmanac.com/egs/java.io/MoveFile.html?l=rel.
         * 
         * @param fromName
         * @param toName
         * @return true if succeeded
         */
        public static boolean moveFile( String fromName, String toName ) {
                boolean ok = true;
                try {
                        Utils.copyTextFile( fromName, toName );
                        new File( fromName ).delete();
                } catch ( Exception e ) {
                        ok = false;
                }
                return ok;
        }

        /**
         * FIXME: make thread safe.
         * 
         * @param name
         * @return
         */
        /*
         * public static Element createElement( String name ) { Element elem =
         * getDocumentFactory().createElement(name); return elem; }
         */
        /**
         * 
         * @param which
         *                some String
         * @param pointOK -
         *                acceptable to have decimal point
         *                (DOUBLE,FLOAT,DECIMAL).
         * @return
         */
        public static boolean isFormattable( String which, boolean pointOK ) {
                boolean formattable = true;
                // FIXME: MATCH AGAINST SOME REGEPS HERE!
                if (nullOrZero( which )) {
                        formattable = false;
                } else {
                        try {
                                if (pointOK) {
                                        double x = Double.parseDouble( which );
                                } else {
                                        int i = Integer.parseInt( which );
                                }
                        } catch ( NumberFormatException ne ) {
                                formattable = false;
                        }
                }
                return formattable;
        }

        /**
         * Pretty print some DOM4J element
         * 
         * @param doc
         *                dome dom4j elements
         * @return element prettyprinted
         */
        public static String dom4jToString( Element doc ) {
                String out = "";
                try {
                        StringWriter writer = new StringWriter();
                        OutputFormat outformat = OutputFormat.createPrettyPrint();
                        // outformat.setEncoding(aEncodingScheme);
                        XMLWriter xwriter = new XMLWriter( writer, outformat );
                        xwriter.write( doc );
                        writer.flush();
                        out = writer.toString();
                } catch ( Exception e ) {
                        e.printStackTrace();
                }
                return out;
        }

        public static String domToString( org.w3c.dom.Element e ) {
                // Element de = (DOMElement) e;
                return e.toString();
        }

        /**
         * Convert a dom4j element to a dom element, using a bizzare and
         * probably very ineffient technique.
         * 
         * @param e
         * @return
         */
        /*
         * public static org.w3c.dom.Element dom4jToDom( Element e ) {
         * org.w3c.dom.Element out = null; if ( e != null ) { try {
         * org.dom4j.Document j4doc = getDocumentFactory().createDocument();
         * j4doc.setRootElement(e); DOMWriter writer = new DOMWriter();
         * org.w3c.dom.Document tmpdoc = writer.write(j4doc); out =
         * tmpdoc.getDocumentElement(); } catch (DocumentException ioe) {
         * ioe.printStackTrace(); } } return out; }
         */
        /**
         * A crude method to check a set of strings against some value. Used to
         * find the appropriate table to insert data into.
         * 
         * @param target
         *                Description of the Parameter
         * @param m
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static boolean matchArray( String target, String[] m ) {
                for (int i = 0; i < m.length; i++) {
                        if (target.indexOf( m[i] ) >= 0) {
                                return true;
                        }
                }
                return false;
        }

        /**
         * A crude method to check a set of strings against some value. Used to
         * find the appropriate table to insert data into.
         * 
         * @param target
         *                Description of the Parameter
         * @param m
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static int arrayIndex( String target, String[] m ) {
                int out = -1;
                for (int i = 0; i < m.length; i++) {
                        if (m[i].equals( target )) {
                                out = i;
                                break;
                        }
                }
                return out;
        }

        /**
         * Read the contents of a file into a String.
         * 
         * @param fname
         *                file to open
         * @return the contents as one String
         * @throws IOException
         */
        public static String readFile( String fname ) throws IOException {
                BufferedReader br = new BufferedReader( new FileReader( fname ) );
                String s = null;
                StringBuffer sb = new StringBuffer();
                while ( ( s = br.readLine() ) != null ) {
                        sb.append( s );
                        sb.append( "\n" );
                }
                sb.deleteCharAt( sb.length() - 1 ); // detele spurious
                                                        // newline at end
                return sb.toString();
        }

        /**
         * get a version of a string that's no longer than some length
         * 
         * @param s -
         *                a String
         * @param len -
         *                maximum length to use
         * @return Description of the Return Value
         */
        public static String truncatedTo( String s, int len ) {
                if (( s != null ) && ( s.length() > len )) {
                        s = s.substring( 0, len );
                }
                return s;
        }

        /**
         * Does exactly what it says on the tin.
         * 
         * @param s -
         *                string to be checked.
         * @return true of the input string is null or has a length of 0
         */
        public static boolean nullOrZero( final String s ) {
                return ( !notNullOrZero( s ) );
        }

        /**
         * Description of the Method 0
         * 
         * @param s1
         *                Description of the Parameter
         * @param s2
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static boolean intersects( Set s1, Set s2 ) {
                Set c1 = new HashSet( s1 );
                // Set c2 = new HashSet( s2 );
                c1.retainAll( s2 );
                return ( c1.size() > 0 );
        }

        /**
         * breaks a comma delimted ascii file into strings using CDATokeniser,
         * which handles (,,,) and ("xxx,sss") correctly
         * 
         * @param ins
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static List cdaToList( final String ins ) {
                CDATokeniser tok;

                tok = new CDATokeniser( ins );
                List v = new ArrayList();
                while ( tok.hasMoreTokens() ) {
                        v.add( tok.nextToken() );
                }
                return v;
        }

        /**
         * breaks a comma delimted ascii file into strings using CDATokeniser,
         * which handles (,,,) and ("xxx,sss") correctly
         * 
         * @param ins
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String[] cdaToStrings( final String ins ) {
                List l = cdaToList( ins );
                String[] s = new String[l.size()];
                for (int i = 0; i < l.size(); i++) {
                        s[i] = (String) l.get( i );
                }
                return s;
        }

        /**
         * convert an array of Strings to a comma-demilited ASCII string, with
         * more complex individual strings enclosed in quotes
         * 
         * @param s
         *                Strings to convert.
         * @return a comma-demilited ASCII string, with more complex individual
         *         strings enclosed in quotes
         */
        public static String stringsToCDA( final String[] s ) {
                StringBuffer sb = new StringBuffer();
                for (int i = 0; i < s.length; i++) {
                        String thisS = s[i].trim();
                        boolean needsIcs = ( ( thisS.indexOf( "," ) >= 0 ) || ( thisS.indexOf( " " ) >= 0 ) );
                        if (needsIcs) {
                                sb.append( "\"" );
                        }
                        sb.append( thisS );
                        if (needsIcs) {
                                sb.append( "\"" );
                        }
                        if (i < ( s.length - 1 )) {
                                sb.append( "," );
                        }
                }
                return sb.toString();
        }

        /**
         * Turn a string like "xx yy zz" into a set (with no duplicates). FIXME:
         * doesn't handle comma delimited ASCII properly.
         * 
         * @param delimiter
         *                the delimiter character
         * @param list
         *                a string like "xx yy zz"
         * @return into a set (with no duplicates)
         */
        public static Set listToSet( final String list, final char delimiter ) {
                String delimStr = new String( new char[] { delimiter } );
                String[] s = list.split( delimStr );
                Set set = new HashSet();
                set.addAll( Arrays.asList( s ) );
                return set;
        }

        public static Set arrayToSet( String[] elements ) {
                Set s = new HashSet();
                for (int i = 0; i < elements.length; i++) {
                        s.add( elements[i] );
                }
                return s;
        }

        /**
         * Turn a string like "xx yy zz" into a set (with no duplicates).
         * Delimiter is space
         * 
         * @param list
         *                a string like "xx yy zz"
         * @return into a set (with no duplicates)
         */
        public static Set listToSet( final String list ) {
                return listToSet( list, DEFAULT_DELIMITER );
        }

        /**
         * Description of the Method
         * 
         * @param s
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static Set copySet( final Set s ) {
                Set out = new HashSet();
                out.addAll( s );
                return out;
        }

        /**
         * Make a properly escaped string suitable for insertion in a url. This
         * is taken from here: http://www.rgagnon.com/javadetails/java-0306.html
         * and was written by S. Bayer.
         * 
         * @param string -
         *                input string
         * @return a properly escaped string suitable for insertion in a url.
         */
        public static String stringToHTMLString( String string ) {
                StringBuffer sb = new StringBuffer( string.length() );
                // true if last char was blank
                boolean lastWasBlankChar = false;
                int len = string.length();
                char c;
                for (int i = 0; i < len; i++) {
                        c = string.charAt( i );
                        if (c == ' ') {
                                // blank gets extra work,
                                // this solves the problem you get if you
                                // replace all
                                // blanks with &nbsp;, if you do that you loss
                                // word breaking
                                if (lastWasBlankChar) {
                                        lastWasBlankChar = false;
                                        sb.append( "&nbsp;" );
                                } else {
                                        lastWasBlankChar = true;
                                        sb.append( ' ' );
                                }
                        } else {
                                lastWasBlankChar = false;
                                //
                                // HTML Special Chars
                                if (c == '"') {
                                        sb.append( "&quot;" );
                                } else if (c == '&') {
                                        sb.append( "&amp;" );
                                } else if (c == '<') {
                                        sb.append( "&lt;" );
                                } else if (c == '>') {
                                        sb.append( "&gt;" );
                                } else if (c == '\n') {
                                        // Handle Newline (GKS: by ignoring it!
                                        // sb.append("&lt;br/&gt;");
                                } else {
                                        int ci = 0xffff & c;
                                        if (ci < 160) {
                                                // nothing special only 7 Bit
                                                sb.append( c );
                                        } else {
                                                // Not 7 Bit use the unicode
                                                // system
                                                sb.append( "&#" );
                                                sb.append( new Integer( ci ).toString() );
                                                sb.append( ';' );
                                        }
                                }
                        }
                }
                return sb.toString();
        }

        /**
         * truncates a string, but with the last 6 characters of len being taken
         * with an elipsis [...] if the string is over len chars long.
         * 
         * @param s
         *                input string
         * @param len -
         *                maximum length to display;
         * @return truncated string, but with the last 6 characters of len being
         *         taken with [...] if the string is over len chars long.
         */
        public static String nicelyTruncate( String s, int len ) {
                if (s.length() <= len) {
                        return s;
                }
                StringBuffer sb = new StringBuffer( s.substring( 0, len - 6 ) );
                sb.append( " [...]" );
                return sb.toString();
        }

        /**
         * Nicely truncates a string if longer than len, or pads it right to len
         * if shorter.
         * 
         * @param s
         *                input String
         * @param len -
         *                maximum length to display;
         * @return truncated string, but with the last 6 characters of len being
         *         taken with [...] if the string is over len chars long.
         */
        public static String toExactLength( String s, int len ) {
                return padRight( nicelyTruncate( s, len ), len );
        }

        /*
         * Replace all instances of a String in a String. @param s String to
         * alter. @param f String to look for. @param r String to replace it
         * with, or null to just remove it.
         */
        /**
         * Description of the Method
         * 
         * @param s
         *                Description of the Parameter
         * @param f
         *                Description of the Parameter
         * @param r
         *                Description of the Parameter
         * @return Description of the Return Value
         */
        public static String replace( String s, String f, String r ) {
                if (s == null) {
                        return s;
                }
                if (f == null) {
                        return s;
                }
                if (r == null) {
                        r = "";
                }
                int index01 = s.indexOf( f );
                while ( index01 != -1 ) {
                        s = s.substring( 0, index01 ) + r + s.substring( index01 + f.length() );
                        index01 += r.length();
                        index01 = s.indexOf( f, index01 );
                }
                return s;
        }

        /**
         * Copy a text file from the 'from' location to the 'to' location
         * 
         * @param file_to_copy
         *                from The complete file system path (+ name) of the
         *                file that needs to be copied.
         * @param destination
         *                The complete file system path (+name) of the new file.
         * @return true or false
         * @exception java.io.IOException
         */
        public static boolean copyTextFile( String file_to_copy, String destination ) throws IOException {
                File inputTextFile = new File( file_to_copy );
                File outputTextFile = new File( destination );
                FileReader t_in = new FileReader( inputTextFile );
                FileWriter t_out = new FileWriter( outputTextFile );
                int c;
                while ( ( c = t_in.read() ) != -1 ) {
                        t_out.write( c );
                }
                // tidy up
                t_in.close();
                t_out.close();
                return true;
        }

        /**
         * Copy a binary file from the 'from' location to the 'to' location
         * 
         * @param file_to_copy
         *                from The complete file system path (+ name) of the
         *                file that needs to be copied.
         * @param destination
         *                The complete file system path (+name) of the new file.
         * @return true or false
         * @exception java.io.IOException
         */
        public static boolean copyBinaryFile( String file_to_copy, String destination ) throws IOException {
                InputStream in = new DataInputStream( new FileInputStream( file_to_copy ) );
                OutputStream out = new DataOutputStream( new FileOutputStream( destination ) );
                int max = 1024;
                byte[] buff = new byte[max];
                /*
                 * while(in.read(buff) != -1){ out.write(buff); }
                 */
                int len;
                while ( ( len = in.read( buff ) ) != -1 ) {
                        out.write( buff, 0, len );
                }
                // tidy up
                in.close();
                out.close();
                return true;
        }
        
        public static void writeFileInBinary( String fname, byte[] data ) throws IOException {
                OutputStream br = new DataOutputStream( new FileOutputStream( fname ) );
                br.write(data);
                br.flush();
                br.close();
        }


        /**
         * Is the value which contained in the mask integer target?
         * 
         * @return true if (( in &amp; which ) == which)
         */
        public static boolean isIncluded( int target, int which ) {
                return ( ( target & which ) == which );
        }

        /**
         * Removes any spaces from the passed string.
         * 
         * @return
         */
        public static String removeSpaces( String value ) {
                if (value == null)
                        return null;
                String trimmedValue = value.trim();
                if (trimmedValue.indexOf( DEFAULT_DELIMITER ) > -1) {
                        StringBuffer sb = new StringBuffer();
                        for (int i = 0; i < trimmedValue.length(); i++) {
                                char temp = trimmedValue.charAt( i );
                                if (temp != DEFAULT_DELIMITER) {
                                        sb.append( temp );
                                }
                        }
                        return sb.toString();
                }
                return trimmedValue;
        }

        /**
         * 
         * @return String representation of all our System properties.
         */
        public static String showSystemProperties() {
                Properties sysp = System.getProperties();
                return showProperties( sysp );
        }

        /**
         * 
         * @param sysp
         * @return
         */
        public static String showProperties( Properties sysp ) {
                StringBuffer sb = new StringBuffer();
                for (Enumeration syse = sysp.propertyNames(); syse.hasMoreElements();) {
                        String name = (String) syse.nextElement();
                        String value = sysp.getProperty( name );
                        sb.append( name + " = " + value + "\n" );
                }
                return sb.toString();
        }

        /**
         * Add a group of properties to the system properties.
         * 
         * @param propertyDir
         *                directory holding some properities file.
         * @param propertyFile
         *                name of the file, minus the ".properties".
         * @throws IOException.
         */
        public static void addProperties( String propertyDir, String propertyFile ) throws IOException {
                String propertiesFile = propertyDir + File.separator + propertyFile + ".properties";
                FileInputStream inputStream = new FileInputStream( propertiesFile );
                Properties testProps = new Properties();
                testProps.load( inputStream );
                System.getProperties().putAll( testProps );
                inputStream.close();
        }

        /**
         * Make a fully qualified name of a properties file in the directory
         * pointed to by "wcgproperties".
         * 
         * @param name -
         *                body of the file name
         * @return (for example) c:\fred\fred\[name].properties.
         */
        public static String getPropFileName( String name ) {
                String propdir = System.getProperty( DEFAULT_PROPERTIES_DIR_ID );
                if (propdir == null) {
                        propdir = "";
                }
                return propdir + File.separatorChar + name + ".properties";
        }

        /**
         * Load properties from a given properties file in the directory pointed
         * to by DEFAULT_PROPERTIES_DIR_ID.
         * 
         * @param name -
         *                body of the props file name, minus the "properties"
         *                extension, which is always added.;
         * @return a loaded props file.
         */
        public static Properties loadPropertiesFrom( String name ) throws IOException, FileNotFoundException {
                Properties props = new Properties();
                String pfname = getPropFileName( name );
                InputStream inputStream = new FileInputStream( pfname );
                props.load( inputStream );
                inputStream.close();
                return props;
        }

        /**
         * Try to delete the named file. Dies silently
         * 
         * @param fname
         * @return true if deleted.
         */
        public static boolean deleteFile( String fname ) {
                boolean ok = true;
                try {
                        File f = new File( fname );
                        ok = f.delete();
                } catch ( Exception ioe ) {
                        ok = false; // die quietly
                }
                return ok;
        }

        /**
         * Nicked from java almanack: stop the current thread for n seconds.
         * 
         * @param secs -
         *                seconds to wait (will always wait a minimum of one).
         */
        public static void waitNSeconds( int secs ) {
                try {
                        secs = Math.min( secs, 1 );
                        long numMillisecondsToSleep = secs * 1000; // 2
                                                                        // seconds
                        Thread.sleep( numMillisecondsToSleep );
                } catch ( InterruptedException e ) {
                        ; // nothing sensible to do
                }
        }

        /**
         * Print a stack trace as a String from the current position.
         * 
         * @return a stack trace as a String from the current position.
         */
        public static String stackTrace() {
                String trace = "";
                try {
                        throw new Exception();
                } catch ( Exception e ) {
                        trace = printStackTrace( e );
                }
                return trace;
        }

        /**
         * 
         * @param e
         * @return
         */
        public static String printStackTrace( Exception e ) {
                StringBuffer sb = new StringBuffer();
                StackTraceElement[] lines = e.getStackTrace();
                for (int i = 1; i < lines.length; i++) {
                        sb.append( "call[ " );
                        sb.append( i );
                        sb.append( " ] = " );
                        sb.append( lines[i] );
                        sb.append( "\n" );
                }
                return sb.toString();
        }

        /**
         * Parse a string with key/value pairs onto a string,string map. Does
         * practically no checking.
         * 
         * @param input
         *                input string
         * @param entryDelim -
         *                delimiter between each pair
         * @param kvDelim -
         *                delimiter between key/value.
         * @return Map
         */
        public static Map<String, String> stringToMap( String input, String entryDelim, String kvDelim ) {
                Map<String, String> out = new HashMap();
                String[] split = input.split( entryDelim );
                for (String ent : split) {
                        if (notNullOrZero( ent )) {
                                String[] kv = ent.split( kvDelim );
                                if (( kv.length == 2 ) && ( notNullOrZero( kv[0] ) )) {
                                        out.put( kv[0], kv[1] );
                                }
                        }

                }
                return out;
        }

        /**
         * Throw a null pointer exception with a message. Uses a vararg (see:
         * http://www.linux-mag.com/2004-06/jdk15_01.html, for example).
         * 
         * @param toTest -
         *                as a vararg - array or arguments of things that can't
         *                be null
         * @throws NullPointerException
         */
        public static void checkNotNull( Object ... toTest ) throws NullPointerException {
                for (Object o : toTest) {
                        if (o == null) {
                                throw new NullPointerException( "you supplied a null " + o.getClass() );
                        }
                }
        }

}
