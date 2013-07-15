package org.oscr.phunpack;
/**
 * This contsructs a "phunpack" for an ADRS run, comprising all the charts,
 * the parameters and an index file, all zipped up.
 * 
 */
import java.io.*;
import java.util.zip.*;
import com.sleepycat.je.Database;
import com.sleepycat.je.DatabaseException;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Logger;

import java.sql.SQLException;

import org.oscr.charts.ChartCommons;
import org.oscr.charts.ChartData;
import org.oscr.charts.ChartDriver;
import org.oscr.charts.ChartLibrary;
import org.oscr.data.CommonChartDetails;
import org.oscr.data.CommonChartDetailsDAO;
import org.oscr.data.SleepycatCommons;
import org.oscr.servlet.ChartServletCommons;

class OnlyThisType implements FilenameFilter{
        private String type;
        
        OnlyThisType( String type ){
                this.type = "."+type;
        }
        
        public boolean accept( File dir, String s ){
                return ( s.endsWith( this.type ));
        }
        
}

public class DriverFullPhunpack{

        private static Logger logger = Logger.getLogger( "PhunpackDriver" );

        private static final int OneK = 1024;
        private static final int ZIP_BUFFER_SIZE = OneK * OneK * 48; // 48MB   

        private static final boolean USE_SVG = true;        
        /**
         * Load a file into the zip stream using the filename as the zipfile entryname.
         * 
         * @param zipFile - the zipfile, as a stream
         * @param fileToLoad - file to load
         */
        private static void loadOneFile( ZipOutputStream zipFile, File fileToLoad ) {
                loadOneFile( zipFile, fileToLoad, fileToLoad.getName() );
        }
        
        /**
         * Load a file into the zip stream.
         * 
         * @param zipFile - the zipfile, as a stream
         * @param fileToLoad - file to load
         * @param entryName - name to add to the zip file as
         */
        private static void loadOneFile( ZipOutputStream zipFile, File fileToLoad, String entryName ) {
                try {
                        FileInputStream in = new FileInputStream( fileToLoad.getAbsolutePath() );
                        zipFile.putNextEntry( new ZipEntry( entryName ) );
                        logger.finest( "added fileToLoad "+fileToLoad.getName() );
                        int len;
                        byte[] buf = new byte[ OneK ];
                        while ( ( len = in.read( buf ) ) > 0 ) {
                                zipFile.write( buf, 0, len );
                        }
                        zipFile.closeEntry();
                } catch ( IOException ioe ) {
                        logger.warning( "Warning: file |" + fileToLoad.getAbsolutePath() + "| was not written to zipfile " );
                }
               
        }
        
        private static void loadAllGraphics( String whichCommons, Writer htmlFile, ZipOutputStream zipFile, String sleepyUniqueRunId, boolean isSvg ) throws IOException{
                try{
                        CommonChartDetailsDAO commonDao = new CommonChartDetailsDAO();
                        CommonChartDetails commonChartDetails = commonDao.load( whichCommons );
                        Database db = SleepycatCommons.getDatabase( sleepyUniqueRunId, false );
                        Map<String,ChartData> map = SleepycatCommons.getChartDataMap( db );
                        SortedSet<String> keys = new TreeSet<String>(map.keySet());
                        int numCharts = 0;
                        for( String filename : keys ){
                                ChartData chartData = map.get( filename );
                                numCharts++;
                                htmlFile.write( ChartCommons.descriptionToHTML( numCharts, chartData, isSvg )); 
                                byte[] buff = ChartLibrary.drawChart( commonChartDetails, chartData );
                                zipFile.putNextEntry( new ZipEntry( filename+".png" ) );
                                zipFile.write( buff, 0, buff.length );
                                zipFile.closeEntry();
                        }
                        db.close();
                } catch (DatabaseException dbe) {
                        throw new IOException( dbe );
                } catch (SQLException sqle) {
                        throw new IOException( sqle );
                }
        }
        
  
        public static byte[] makePhunpack(  String whichCommons,
                                            String rootDir, 
                                            String outputDir, 
                                            String sleepyUniqueRunId, 
                                            String model, 
                                            String helpDir,
                                            boolean isSvg ) throws IOException {
                ByteArrayOutputStream byteStream = new ByteArrayOutputStream( ZIP_BUFFER_SIZE );
                ZipOutputStream zip = new ZipOutputStream( byteStream );
                String directory = "";
                BufferedWriter htmlFile = new BufferedWriter( new FileWriter( rootDir + File.separator + "graphics_index.html"));
                htmlFile.write( ChartCommons.HTML_FILE_HEADER );
                loadAllGraphics( whichCommons, htmlFile, zip, sleepyUniqueRunId, isSvg ); 
                htmlFile.write( "</table></body>\n</html>\n" );
                htmlFile.close();
                System.out.println( "got dir as " + directory );
                
                
                loadOneFile( zip, new File( rootDir +File.separatorChar + "graphics_index.html" ));
                loadOneFile( zip, new File( outputDir + File.separatorChar +model.toUpperCase() + "_ginis_and_lorenz.txt" ), "inequality.csv" );
                loadOneFile( zip, new File( outputDir + File.separatorChar +model.toUpperCase() + "_gainers_and_losers.txt" ), "gainers_losers.csv" );
                loadOneFile( zip, new File( rootDir + File.separatorChar + "all_params.html" ) );
                loadOneFile( zip, new File( helpDir + File.separatorChar + "table_explanation.txt" ));
                loadOneFile( zip, new File( helpDir + File.separatorChar + "phunpack_index.html" ), "index.html");
                zip.close();
                
                return byteStream.toByteArray();
        }

        /**
         * @param args
         */
        public static void main( String[] args ) {
                try {
                        ChartDriver.generateAllCharts( args[1], 
                                                args[2], //directory,
                                                args[3], //DEFAULT_RUN_ID, 
                                                args[4],
                                                USE_SVG ); // dir.getAbsolutePath() );
                        
                        //boolean isSvg = ! (args[6] != "svg");
                        //byte[] out = makePhunpack( args[0], args[1], args[2], args[3], args[4], args[5], isSvg );
                } catch ( IOException ioe ) {
                        ioe.printStackTrace();
                } catch ( SQLException sqle ){
                        sqle.printStackTrace();
                }

        }

}
