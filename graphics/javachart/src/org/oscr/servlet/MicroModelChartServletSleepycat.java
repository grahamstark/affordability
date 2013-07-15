package org.oscr.servlet;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.io.IOException;
import java.io.File;
import java.util.Set;
import java.util.HashSet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.oscr.charts.ChartData;
import org.oscr.charts.ChartLibrary;
import org.oscr.charts.ShortOrLongRun;
import org.oscr.data.ChartDataDAO;
import org.oscr.data.CommonChartDetails;
import org.oscr.data.CommonChartDetailsDAO;
import org.oscr.data.SleepycatCommons;
import org.oscr.utils.Utils;



import java.sql.SQLException;

/**
 * this servlet drives graphs produced using the JFreeChart classes and
 * Jocelyn's/BBC's patent embedded URL graphics.
 * 
 * @author graham_s@ifs.org.uk
 * @author popx@ifs.org.uk
 */
public class MicroModelChartServletSleepycat extends HttpServlet{
        
        private static Set<String> ALL_KNOWN_SAVED_IMAGE_SETS = new HashSet<String>();

        private static Logger logger = Logger.getLogger( "MicroModelChartServletSleepycat" );

        private static CommonChartDetails commonChartDetails = getCommonChartDetails( "default");
        
        // singleto method
        private static CommonChartDetails getCommonChartDetails( String whichCommons ){
                if( commonChartDetails == null ){
                        try{
                                CommonChartDetailsDAO commonDao = new CommonChartDetailsDAO();
                                commonChartDetails = commonDao.load( whichCommons );
                        } catch( SQLException sqle ){
                                logger.severe( Utils.printStackTrace( sqle ));                                       
                        }
                }
                return commonChartDetails;
        }


        public synchronized void doGet( HttpServletRequest req, HttpServletResponse res ) throws ServletException, IOException {
                byte[] buff = null;
                String queryString = req.getQueryString();
                Object cachedO = Caches.CHARTS_CACHE.get( queryString );

                String uid = req.getParameter( "uid" );
                String model = req.getParameter( "model" );
                String runId = req.getParameter( "rid" );
                String runType = req.getParameter( "rtype" );
                boolean deleteRunsDB = "1".equals( req.getParameter( "delete_run" ));
                String sleepyUniqueRunId = ChartServletCommons.makeUniqueRunIdString( runId, model, uid, runType );
                
                if( deleteRunsDB ){
                        SleepycatCommons.removeDatabase( sleepyUniqueRunId );       
                } else {
                        String workDir = "wd__" + uid + File.separator + model;
                        String chartId = req.getParameter( "chart_id" );
                        String longOrShort = req.getParameter( "rtype" );
                        ShortOrLongRun sr = "short_run".equals( longOrShort ) ? ShortOrLongRun.subsets : ShortOrLongRun.full;
                        String thumbStr = req.getParameter( "thumbnail" );
                        boolean immediateExit = "1".equals( req.getParameter( "dummy" ));
                        boolean isThumbnail = "1".equals( thumbStr );
                        logger.info( "received chart request: workDir= |"+workDir+"| chartId = |"+chartId+ "| thumbStr = |" + thumbStr + "| " );
                        if( isThumbnail ){
                                chartId += "_thumb"; 
                        }
                        if (cachedO != null) {
                                buff = (byte[]) cachedO;
                                logger.finest( "found chart in cache! " );
                        } else {
                                logger.finest( "missing chart from cache! " );
                                if( ! ALL_KNOWN_SAVED_IMAGE_SETS.contains( sleepyUniqueRunId )){
                                        ChartServletCommons.sleepyLoadChartDetailsForRun( workDir, sr, runId, sleepyUniqueRunId );
                                        ALL_KNOWN_SAVED_IMAGE_SETS =  SleepycatCommons.getAllDatabaseNames();
                                }
        
                                ChartData chartData = ChartServletCommons.getChartData( sleepyUniqueRunId, chartId ); 
                                
                                buff = ChartLibrary.drawChart( commonChartDetails, chartData );
                                if (buff != null) {
                                        res.setContentType( "image/png" );
                                        Caches.CHARTS_CACHE.put( queryString, buff );
                                }
                        }
                        res.getOutputStream().write( buff );
                        res.getOutputStream().close();
                }
        }
}