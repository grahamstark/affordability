package org.oscr.servlet;
/**
 * NOT USED - see sleepycat version instead
 */
import java.io.IOException;
import java.sql.SQLException;
import java.util.Map;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.oscr.charts.ChartData;
import org.oscr.charts.ChartLibrary;
import org.oscr.charts.ShortOrLongRun;
import org.oscr.data.CommonChartDetails;
import org.oscr.data.CommonChartDetailsDAO;
import org.oscr.utils.Utils;

/**
 * this servlet drives graphs produced using the JFreeChart classes and
 * Jocelyn's/BBC's patent embedded URL graphics.
 * 
 * @author graham_s@ifs.org.uk
 * @author popx@ifs.org.uk
 */
public class MicroModelChartServlet extends HttpServlet{


        private static Logger logger = Logger.getLogger( "MicroModelChartServlet" );

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

                String chartId = req.getParameter( "chart_id" );
                String workDir = req.getParameter( "wd" );
                String runId = req.getParameter( "rid" );
                String longOrShort = req.getParameter( "rtype" );
                ShortOrLongRun sr = "short_run".equals( longOrShort ) ? ShortOrLongRun.subsets : ShortOrLongRun.full;
                String thumbStr = req.getParameter( "thumbnail" );
                boolean immediateExit = "1".equals( req.getParameter( "dummy" ));
                boolean isThumbnail = "1".equals( thumbStr );
                if( isThumbnail ){
                        chartId += "_thumb";
                }
                if (cachedO != null) {
                        buff = (byte[]) cachedO;
                        logger.finest( "found chart in cache! " );
                } else {
                        logger.finest( "missing chart from cache! " );
                        Map<String, ChartData> thisRunsCharts = Caches.MICRO_CHART_LIST_CACHE.get( runId );
                        if (thisRunsCharts == null) {
                                logger.finest( "chart lists not in cache! " );
                                thisRunsCharts = ChartServletCommons.loadChartDetailsForRun( workDir, sr, runId );
                                Caches.MICRO_CHART_LIST_CACHE.put( runId, thisRunsCharts );
                        }
                        if( immediateExit ){
                                return;
                        }
                         
                        ChartData chartData = thisRunsCharts.get( chartId );
                        
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