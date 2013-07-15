package org.oscr.servlet;

/**
 * Methods to retrieve 
 * 
 * $Date: $
 * $Author: $
 * $Revision: $
 */
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.oscr.charts.ChartData;
import org.oscr.charts.ChartDriverConstants;
import org.oscr.charts.FileChartDataDAOImpl;
import org.oscr.charts.ShortOrLongRun;
import org.oscr.data.ChartDataCommons;
import org.oscr.data.ChartDataDAO;
import org.oscr.data.SleepycatCommons;

import com.sleepycat.je.Database;
import com.sleepycat.je.DatabaseException;

public class ChartServletCommons{

        public static final String WORK_DIR_ROOT = "/home/graham_s/VirtualWorlds/projects/oscr/model/";

        public static final String APACHE_ROOT = "/var/www/adrs/";
        
        public static final String HELP_FILE_LOCATION = APACHE_ROOT + "/helpfiles/en/";
        
        /**
         * Get a single chart data class for one chart from a given run.
         * 
         * @param runId -
         *                This server as the name of the sleepycat database (one
         *                per run), with a common environment.
         * @param filename -
         *                the chart (minus the png extension) - this serves as
         *                the key for one ChartData item in the db for that run.
         * @return a ChartData class or null
         * @throws IOException
         */
        public static ChartData getChartData( String runId, String filename ) throws IOException {
                Database db = SleepycatCommons.getDatabase( runId, false );
                ChartData oneChart = null;
                try {
                        Map<String, ChartData> map = SleepycatCommons.getChartDataMap( db );
                        oneChart = map.get( filename );
                        db.close();
                } catch ( DatabaseException dbe ) {
                        throw new IOException( dbe );
                }
                return oneChart;
        }
        
        /**
         * fake a primary key for this run's results
        */
        public static String makeUniqueRunIdString( String runId, String model, String uid, String runType ){
                String s = s = "rid_" + runId + "_model__" + model +"_uid__" + uid;
                if( runType != null ){
                       s +="_rtype_"+runType;
                }
                return s;
        }

        /**
         * Write all chart details for a run from the php chart descriptions
         * text file into a quick-lookup sleepycat database. Each run has its
         * own database with a shared environment.
         * 
         * @param workDir
         * @param shortOrLong
         * @param fakePrimaryKey
         * @return
         * @throws IOException
         */
        public static Map<String, ChartData> sleepyLoadChartDetailsForRun( String workDir, ShortOrLongRun shortOrLong, String runId, String fakePrimaryKey ) throws IOException {
                Database db = SleepycatCommons.getDatabase( fakePrimaryKey, true );
                Map<String, ChartData> map = null;
                Logger logger = Logger.getLogger( "sleepyLoadChartDetailsForRun" );
                try {
                        map = SleepycatCommons.getChartDataMap( db );
                        String imageFile = WORK_DIR_ROOT + workDir + "/results/" + shortOrLong + "/images/" + runId + "/image_driver.txt";
                        BufferedReader reader = new BufferedReader( new FileReader( imageFile ) );
                        ChartDataDAO chartDataDAO = new FileChartDataDAOImpl( reader );
                        ChartData oneChart = null;
                        int chartNum = 0;
                        do {
                                oneChart = chartDataDAO.loadChartData();
                                if (oneChart != null) {
                                        map.put( oneChart.getFilename(), oneChart );
                                        chartNum++;
                                }
                                if (( chartNum % 10 ) == 0) {
                                        logger.fine( "charts processed " + chartNum );
                                }
                        } while ( oneChart != null );
                        db.close();
                        reader.close();
                } catch ( DatabaseException dbe ) {
                        throw new IOException( dbe );
                }
                return map;
        }

        /**
         * -- NO LONGER USED -- write all the php chart details into a hashmap.
         * 
         * @param workDir
         * @param shortOrLong
         * @param runId
         * @return
         * @throws IOException
         */
        public static Map<String, ChartData> loadChartDetailsForRun( String workDir, ShortOrLongRun shortOrLong, String runId ) throws IOException {
                Map<String, ChartData> data = new HashMap<String, ChartData>();
                Logger logger = Logger.getLogger( "loadChartDetailsForRun" );
                
                try {
                        Connection con = ChartDataCommons.makeConnection();
                        PreparedStatement ps = con.prepareStatement( ChartDataCommons.UPDATE_COUNT_STMT );
                        ps.setString( 2, runId );
                        PreparedStatement statePs = con.prepareStatement( ChartDataCommons.UPDATE_STATE_STMT );
                        statePs.setString( 1, ChartDriverConstants.CHARTS_LOADING );
                        statePs.setString( 2, runId );
                        statePs.execute();

                        String imageFile = WORK_DIR_ROOT + workDir + "/results/" + shortOrLong + "/images/" + runId + "/image_driver.txt";
                        BufferedReader reader = new BufferedReader( new FileReader( imageFile ) );
                        ChartDataDAO chartDataDAO = new FileChartDataDAOImpl( reader );
                        ChartData oneChart = null;
                        int chartNum = 0;
                        do {
                                oneChart = chartDataDAO.loadChartData();
                                if (oneChart != null) {
                                        data.put( oneChart.getFilename(), oneChart );
                                        chartNum++;
                                }
                                if (( chartNum % 10 ) == 0) {
                                        logger.finest( "charts processed " + chartNum );
                                        ps.setInt( 1, chartNum );
                                        ps.execute();
                                }

                        } while ( oneChart != null );
                        reader.close();
                        statePs.setString( 1, ChartDriverConstants.RESULTS_AND_CHARTS_LOADED );
                        statePs.execute();
                        statePs.close();
                        ps.close();
                } catch ( SQLException sqle ) {
                        throw new IOException( sqle );
                }
                return data;
        }
}
