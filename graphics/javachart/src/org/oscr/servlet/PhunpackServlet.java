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
import org.oscr.phunpack.Driver;
import org.oscr.utils.Utils;
import org.oscr.charts.ChartCommons;




import java.sql.SQLException;

/**
 * This returns an enormous zip file with all the charts and some gain-lose and gini files from a micro model run.
 * 
 * @author graham_s@ifs.org.uk
 * @author popx@ifs.org.uk
 */
public class PhunpackServlet extends HttpServlet{

        private static Logger logger = Logger.getLogger( "PhunpackServlet" );

        public synchronized void doGet( HttpServletRequest req, HttpServletResponse res ) throws ServletException, IOException {
                String userId = req.getParameter( "uid" );
                String chartId = req.getParameter( "chart_id" );
                String uid = req.getParameter( "uid" );
                String runId = req.getParameter( "rid" );
                String longOrShort = req.getParameter( "rtype" );
                String model = req.getParameter( "model" );
                
                String workDir = "wd__" + uid + File.separator + model;
                ShortOrLongRun sr = "short_run".equals( longOrShort ) ? ShortOrLongRun.subsets : ShortOrLongRun.full;
                
                String sleepyUniqueRunId = ChartServletCommons.makeUniqueRunIdString( runId, model, uid, longOrShort );
                
                workDir = ChartServletCommons.WORK_DIR_ROOT + File.separator + workDir + File.separator;
                String outputDir = workDir + "/results/" + sr + File.separator;
                
                byte[] buff = Driver.makePhunpack( 
                                            "default",
                                            workDir, 
                                            outputDir, 
                                            sleepyUniqueRunId, 
                                            model, 
                                            ChartServletCommons.HELP_FILE_LOCATION,
                                            ChartCommons.CREATE_SVG );
                res.setContentType("application/zip");
                res.setHeader("Content-Disposition", "attachment; filename=information_pack_" + model + "_" + runId + "_" + longOrShort + ".zip" );
                res.getOutputStream().write( buff );
                res.getOutputStream().close();
        }
}