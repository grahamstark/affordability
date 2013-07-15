package org.oscr.phunpack;
/**
 * This contsructs a "phunpack" for an ADRS run, comprising all the charts,
 * the parameters and an index file, all zipped up.
 * 
 */
import java.util.zip.*;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.logging.Logger;

import org.oscr.charts.ChartDriver;
import org.oscr.data.CommonChartDetails;
import org.oscr.fop.PDFGenerator;

public class Driver{

        private static final int OneK = 1024;
        private static final int ZIP_BUFFER_SIZE = OneK * OneK * 48; // 48MB   

        
        private static Logger logger = Logger.getLogger( "PhunpackDriver" );
        
        private static CommonChartDetails makeOSCRCommonChartDetails(){
                CommonChartDetails commonDetails = new CommonChartDetails();
                commonDetails.setColour( new int[]{ 0xffffff, 0xeeffee, 0xb53a30, 0x165b9b } );
                commonDetails.setFont( "gill_sans" );
                commonDetails.setFooter( "copyright(c) OSCR/Virtual Worlds 2008" );
                commonDetails.setFullHeight( 500 );
                commonDetails.setFullWidth( 800 );
                commonDetails.setThumbnailHeight( 70 );
                commonDetails.setThumbnailWidth( 100 );
                return commonDetails;
        }
        
        private static final CommonChartDetails OSCR_CHART_DETAILS = makeOSCRCommonChartDetails();
          
        
        public static void visitAllDirsAndFiles( File dir, boolean useSVG ) throws IOException{
                if( dir.isDirectory() ) {
                    String[] children = dir.list();
                    for (int i=0; i<children.length; i++) {
                        visitAllDirsAndFiles( new File(dir, children[i]), useSVG );
                    }
                } else {
                        String directory = dir.getParent();
                        String fname = dir.getName();
                        System.out.println( "looking at file " + fname + " directory " + directory );
                        if( fname.compareTo( "oscr_charts_driver.txt") == 0){
                                System.out.println( "got dir as " + directory );
                                ChartDriver.sqlLessGenerateAllCharts( OSCR_CHART_DETAILS, directory, dir.getAbsolutePath(), useSVG );
                        } 
                        /** FIXME: fix the docbook_insert before enabling this
                        else if ( fname.compareTo( "docbook_insert.xml" ) == 0){
                                System.out.println( "creating PDF" );
                                PDFGenerator.generatePDF( 
                                       dir.getAbsolutePath(),
                                       PDFGenerator.getStylesheetName(),
                                       directory+File.separator+"report.pdf" );
                                                                
                        }
                        */
                }
        }
        

        /**
         * @param args
         */
        public static void main( String[] args ) {
                try {
                        File head = new File( args[0] );
                        boolean useSVG = "1".equals( args[1] ); 
                        visitAllDirsAndFiles( head, useSVG );
                } catch ( IOException ioe ) {
                        ioe.printStackTrace();
                } 
        }

}
