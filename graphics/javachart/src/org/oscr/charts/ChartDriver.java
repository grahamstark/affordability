/**
 * This bulk-creates charts using chart definitions produced by the php model driver 
 * code, written to a text file.
 *   
 */
package org.oscr.charts;


import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.oscr.data.ChartDataCommons;

import org.oscr.data.ChartDataDAO;
import org.oscr.data.CommonChartDetails;
import org.oscr.data.CommonChartDetailsDAO;


/**
 * @author graham_s
 * 
 */
public class ChartDriver{
        
        
        
        public static Logger logger =
          Logger.getLogger(ChartDriver.class.getName());
          


        public static void sqlLessGenerateAllCharts( CommonChartDetails commonDetails, String workingDir, String fileName, boolean createSVG ) throws IOException {

                BufferedReader br = new BufferedReader( new FileReader( fileName ) );
                
                BufferedWriter htmlFile = new BufferedWriter( new FileWriter( workingDir + File.separator + "graphics_index.html"));
                
                htmlFile.write( ChartCommons.HTML_FILE_HEADER );
                
                ChartDataDAO chartDataDAO = new FileChartDataDAOImpl( br );
         
                int chartNum = 0;

                ChartData data = null;
                do {
                        data = chartDataDAO.loadChartData();
                        if (data != null) {
                                logger.info( "writing " + data.getFilename());
                                JFreeChartLibrary.writeChart( commonDetails, data, createSVG );
                                htmlFile.write( ChartCommons.descriptionToHTML( chartNum, data, createSVG ) );
                                chartNum++;
                        }
                } while ( data != null );
                br.close();
                htmlFile.write( "</table></body>\n</html>\n" );
                htmlFile.close();
        }
          
        
        /**
         * As above, but writes a progrss indicator to the sql table pointed to by ChartDataCommon
        */
        public static void generateAllCharts( String whichDetails, String workingDir, String runId, String fileName, boolean createSVG ) throws SQLException, IOException {

                CommonChartDetailsDAO commonDao = new CommonChartDetailsDAO();
                CommonChartDetails commonDetails = commonDao.load( whichDetails );

                Connection con = ChartDataCommons.makeConnection();

                PreparedStatement ps = con.prepareStatement( ChartDataCommons.UPDATE_COUNT_STMT );
                ps.setString( 2, runId );

                PreparedStatement statePs = con.prepareStatement( ChartDataCommons.UPDATE_STATE_STMT );                
                statePs.setString( 1, ChartDriverConstants.CHARTS_LOADING );
                statePs.setString( 2, runId );
                statePs.execute();
                
                BufferedReader br = new BufferedReader( new FileReader( fileName ) );
                
                BufferedWriter htmlFile = new BufferedWriter( new FileWriter( workingDir + File.separator + "graphics_index.html"));
                
                htmlFile.write( ChartCommons.HTML_FILE_HEADER );
                
                ChartDataDAO chartDataDAO = new FileChartDataDAOImpl( br );
         
                int chartNum = 0;

                ChartData data = null;
                do {
                        data = chartDataDAO.loadChartData();
                        if (data != null) {
                                logger.info( "writing " + data.getFilename());
                                
                                boolean chartWritten = JFreeChartLibrary.writeChart( commonDetails, data, createSVG );
                                if( chartWritten ){
                                        htmlFile.write( ChartCommons.descriptionToHTML( chartNum, data, createSVG ) );
                                        chartNum++;
                                        if (( chartNum % 10 ) == 0) {
                                                ps.setInt( 1, chartNum );
                                                ps.execute();
                                        }
                                }
                        }
                } while ( data != null );
                br.close();
                statePs.setString( 1, ChartDriverConstants.RESULTS_AND_CHARTS_LOADED );
                statePs.execute();
                htmlFile.write( "</table></body>\n</html>\n" );
                htmlFile.close();
        }
        
        

        /**
         * 
         * @param args
         *                0 - runId; 1 command file name
         */
        public static void main( String[] args ) {
                try {
                        String workingId = args[0];
                        FileHandler handler = new FileHandler( ChartDriverConstants.LOGFILE );
                        logger.addHandler( handler );
                        logger.setLevel( Level.ALL );
                        
                        String runId = args[1];
                        String fileName = args[2];
                        boolean createSVG = "1".equals(args[3]); 
                        generateAllCharts( "oscr", workingId, runId, fileName, createSVG );
                } catch ( IOException ioe ) {
                        logger.severe( "IO Error in logger called with " + args[1] + " " + args[2] );
                        for(StackTraceElement ste : ioe.getStackTrace() ){
                                logger.severe( ste.toString() );
                        }
                } catch ( SQLException sqle ) {
                        logger.severe( "IO Error in logger called with " + args[1] + " " + args[2] );
                        for(StackTraceElement ste : sqle.getStackTrace() ){
                                logger.severe( ste.toString() );
                        }
                }
        }
}
