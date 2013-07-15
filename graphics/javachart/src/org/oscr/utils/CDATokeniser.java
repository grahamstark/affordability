package org.oscr.utils;
import java.util.Iterator;
import java.lang.StringBuffer;
import java.util.Vector;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;
/**
 * Analog of StringTokenizer for comma-delimited ascii data assuming that ,,, = 3 blanks; "xx,xxx" = one word
 * 
 * @author Graham Stark created: 5/12/1999 
 */
public class CDATokeniser implements Iterator {

        Vector tokens = new Vector();

        int pos, length;

        /**
         * @return total number of tokens in current tokenise
         */
        public int getLength() {
                return tokens.size();
        }

        /**
         * @return from Iterator interface.
         */
        public void remove() {
                tokens.remove( pos );
        }

        // these are the analogs of StringTokenizer */

        /**
         * @return any more strings in the enumeration
         */
        public boolean hasMoreTokens() {
                return this.hasNext();
        }

        /**
         * @return next string in the enumeration
         */
        public String nextToken() {
                return (String) this.next();
        }

        // these implement Enumeration
        /**
         * @return any more objects in the enumeration
         */
        public boolean hasNext() {
                return pos < length;
        }

        /**
         * @return next objects in the enumeration
         */
        public Object next() {
                Object o = tokens.elementAt( pos );
                pos++;
                return o;
        }

        /**
         * The main program for the CDATokeniser class
         * 
         * @param args
         *                The command line arguments
         */
        public static void main( String[] args ) {
                int i = 0;
                String line;
                String s;
                try {
                        BufferedReader inf = new BufferedReader( new FileReader( args[0] ) );
                        while ( (line = inf.readLine()) != null ) {
                                CDATokeniser tok = new CDATokeniser( line );
                                i = 0;
                                while ( tok.hasMoreTokens() ) {
                                        s = tok.nextToken();
                                        i++;
                                }
                        }
                }
                /*
                 * endtry
                 */
                catch ( IOException ioe ) {
                        ioe.printStackTrace();
                }
                /*
                 * endcatch
                 */
        }

        /**
         * constructor
         * 
         * @param str
         *                Description of the Parameter
         */
        public CDATokeniser( String str ) {
                int ptr = 0;
                char ch;
                int len = str.length();
                boolean addLast = true;
                StringBuffer buff = new StringBuffer();
                // logger.debug( " len = " + len );
                while ( ptr < len ) {
                        ch = str.charAt( ptr );
                        // logger.debug( " on ch=|"+ch+"| ptr="+ptr );

                        switch ( ch ) {
                        case ',':
                                tokens.addElement( buff.toString() );
                                // logger.debug( " adding " + buff.toString() + " to tokens " );
                                buff = new StringBuffer();
                                break;
                        case '"':
                                // buff.append( ch );
                                do {
                                        ptr++;
                                        if ( ptr < len ) {
                                                ch = str.charAt( ptr );
                                                if ( ch != '"' ) {
                                                        buff.append( ch );
                                                }
                                        } else {
                                                addLast = false;
                                        }
                                } while ( (ch != '"') && (ptr < len) );
                                tokens.addElement( buff.toString() );
                                // logger.debug( " adding " + buff.toString() + " to tokens " );
                                buff = new StringBuffer();
                                do {
                                        ptr++;
                                        if ( ptr < len ) {
                                                ch = str.charAt( ptr );
                                        } else {
                                                addLast = false;
                                        }
                                } while ( (ptr < len) && (ch != ',') );
                                break;
                        default:
                                buff.append( ch );
                        }
                        ptr++;
                }
                /*
                 * endwhile
                 */
                if ( addLast ) {
                        tokens.addElement( buff.toString() );
                }
                length = tokens.size();
                pos = 0;
        }
}