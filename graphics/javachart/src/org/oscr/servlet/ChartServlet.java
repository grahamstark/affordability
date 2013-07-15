package org.oscr.servlet;

import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.oscr.charts.ChartData;
import org.oscr.charts.ChartLibrary;
import org.oscr.data.ChartDataDAO;
import org.oscr.data.CommonChartDetails;
import org.oscr.data.CommonChartDetailsDAO;
import org.oscr.utils.LRUCache;
import org.oscr.utils.Utils;

/**
 * this servlet drives graphs produced using the JFreeChart classes and Jocelyn's/BBC's patent embedded URL graphics.
 * 
 * @author graham_s@ifs.org.uk
 * @author popx@ifs.org.uk
 */
public class ChartServlet extends HttpServlet {
        
        
        private static Logger logger = Logger.getLogger( "ChartServlet" );
        
        private static CommonChartDetails commonChartDetails = getCommonChartDetails( "default");
        
               // singleto method
        private static CommonChartDetails getCommonChartDetails( String whichDetails ){
                if( commonChartDetails == null ){
                        try{
                                CommonChartDetailsDAO commonDao = new CommonChartDetailsDAO();
                                commonChartDetails = commonDao.load( whichDetails );
                        } catch( SQLException sqle ){
                                logger.severe( Utils.printStackTrace( sqle ));                                       
                        }
                }
                return commonChartDetails;
        }

        public synchronized void doGet( HttpServletRequest req, HttpServletResponse res ) throws ServletException,
                        IOException {
                byte[] buff = null;
                String queryString = req.getQueryString();
                Object cachedO = Caches.CHARTS_CACHE.get( queryString );
                if ( cachedO != null ) {
                        buff = (byte[]) cachedO;
                        logger.finest( "found in cache! " );
                } else {
                        logger.finest( "missing from cache! " );
                        ChartDataDAO dao = new ServletChartDataDAOImpl( req );
                        ChartData chartData = dao.loadChartData(); 
                        buff = ChartLibrary.drawChart( commonChartDetails, chartData );
                        if ( buff != null ) {
                                res.setContentType( "image/png" );
                                Caches.CHARTS_CACHE.put( queryString, buff );
                        }
                }
                res.getOutputStream().write( buff );
                res.getOutputStream().close();
        }
}