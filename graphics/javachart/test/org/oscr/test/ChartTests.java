/**
 * 
 */
package org.oscr.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.oscr.charts.ChartData;
import org.oscr.charts.ChartDriver;
import org.oscr.charts.ChartUtils;
import org.oscr.charts.FileChartDataDAOImpl;
import org.oscr.charts.ShortOrLongRun;
import org.oscr.data.ChartDataDAO;
import org.oscr.data.CommonChartDetails;
import org.oscr.data.CommonChartDetailsDAO;
import org.oscr.data.SleepycatCommons;
import org.oscr.servlet.ChartServletCommons;

import com.sleepycat.je.Database;
import com.sleepycat.je.DatabaseException;

/**
 * @author graham_s
 * 
 */
public class ChartTests extends junit.framework.TestCase{
        private CommonChartDetailsDAO dao;

        private static final String OSCR_DIR = "/var/www/oscr/charge_1/"; // 
        private static final String WORKING_DIR = "/home/graham_s/VirtualWorlds/projects/adrs/javacharts/javachart/working/";

        private ChartDataDAO chartDataDAO;
        
        private static final boolean USE_SVG = true;

        private BufferedReader br;

        private static final String DEFAULT_RUN_ID = "default_run_id";

        private static final String TEST_CHART_FILE =  OSCR_DIR + "oscr_charts_driver.txt";
 
        @Test
        public void testUtils() {
                int[] years = ChartUtils.range( 2003, 2006 );
                assertEquals( "1st year should be:", years[0], 2003 );
                assertEquals( "last year should be:", years[years.length - 1], 2006 );
                double[] x = new double[] { 1, 2, 3, 4 };
                assertEquals( "sum :", 10.0, ChartUtils.discountedSum( x ) );
                assertEquals( "avg :", 2.5, ChartUtils.average( x ) );
        }

        /**
         * @throws java.lang.Exception
         */
        @Before
        public void setUp() throws Exception {
                this.dao = new CommonChartDetailsDAO();
                //this.br = new BufferedReader( new FileReader( TEST_CHART_FILE ) );
                //this.chartDataDAO = new FileChartDataDAOImpl( br );
        }

        /**
         * @throws java.lang.Exception
         */
        @After
        public void tearDown() throws Exception {
                //this.br.close();
        }

        //  @Test
        public void xxtestSleepy() {
                Database db = SleepycatCommons.getDatabase( "THIS_IS_A_TEST_3", true );
                try {
                        // StoredMap map = SleepycatCommons.getChartDataMap( db
                        // );
                        Map<String, ChartData> map = SleepycatCommons.getChartDataMap( db );
                        int i = 0;
                        BufferedReader reader = new BufferedReader( new FileReader( TEST_CHART_FILE ) );
                        ChartDataDAO chartDataDAO = new FileChartDataDAOImpl( reader );
                        ChartData oneChart = null;
                        do {
                                oneChart = chartDataDAO.loadChartData();
                                if (oneChart != null) {
                                        map.put( oneChart.getFilename(), oneChart );
                                }
                                i++;
                        } while ( ( oneChart != null ) && ( i < 10000 ) );
                        System.out.println( "read " + i + " entries " );
                        db.close();

                        Database db2 = SleepycatCommons.getDatabase( "THIS_IS_A_TEST_2", false );
                        Map<String, ChartData> map2 = SleepycatCommons.getChartDataMap( db2 );
                        for (String key : map2.keySet()) {
                                System.out.println( "key " + key );
                                oneChart = map2.get( key );
                        }
                        System.out.println( "all databases " + SleepycatCommons.getAllDatabaseNames().toString() );
                        ChartData chartData = ChartServletCommons.getChartData( "THIS_IS_A_TEST_2", "ineq_lorenz_race_white_2006_thumb" );
                        assertNotNull( chartData );
                } catch ( DatabaseException dbe ) {
                        dbe.printStackTrace();
                } catch ( IOException ioe ) {
                        ioe.printStackTrace();

                }
        }
        
        public static void visitAllDirsAndFiles(File dir) throws Exception{
                if( dir.isDirectory() ) {
                    String[] children = dir.list();
                    for (int i=0; i<children.length; i++) {
                        visitAllDirsAndFiles(new File(dir, children[i]));
                    }
                } else {
                        System.out.println( "looking at file " + dir.getName());
                        if( dir.getName().compareTo( "oscr_charts_driver.txt") == 0){
                                String directory = dir.getParent();
                                System.out.println( "got dir as " + directory );
                                ChartDriver.generateAllCharts( "oscr", directory, DEFAULT_RUN_ID, dir.getAbsolutePath(), USE_SVG );
                        }
                }
            }

        
        // @Test
        public void testCreateAllCharts(){
                File f = new File( "/var/www/oscr/");
                
                try {
                        visitAllDirsAndFiles( f );
                } catch ( Exception ioe ) {
                        ioe.printStackTrace();
                        fail( "chart creation failed" );
                }
                
                
        }



        // @Test
        public void xxtestCreateCharts() {

                try {
                        ChartDriver.generateAllCharts( "default", WORKING_DIR, DEFAULT_RUN_ID, TEST_CHART_FILE, USE_SVG );
                } catch ( Exception ioe ) {
                        ioe.printStackTrace();
                        fail( "chart creation failed" );
                }
        }

        // @Test
        public void xxtestChartDataDAO() {
                try {

                        Map<String, ChartData> data = ChartServletCommons.loadChartDetailsForRun( "wd__1", ShortOrLongRun.subsets, "default_run_id" );
                } catch ( IOException ioe ) {
                        fail( "chartDataDAO dao sql failed" );
                }
        }

        @Test
        public void testCommonDAO() {
                try {
                        CommonChartDetails details = dao.load( "oscr");
                        System.out.println( "footer::" + details.getFooter() );
                        int[] cols = details.getColour();
                        for (int col : cols) {
                                System.out.println( "colour" + col );
                        }
                } catch ( Exception e ) {
                        e.printStackTrace();
                        fail( "dao sql failed" );
                }
        }

        /**
         * Test method for
         * {@link org.oscr.charts.ChartDriver#main(java.lang.String[])}.
         */
        @Test
        public void testMain() {
                ;
        }

}
