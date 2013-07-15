package org.oscr.charts;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.logging.Logger;

import org.oscr.data.ChartDataDAO;
import org.oscr.utils.Utils;


public class FileChartDataDAOImpl implements ChartDataDAO{

        private BufferedReader br;
        
        private static Logger logger = Logger.getLogger( "FileChartDataDAOImpl" );


        public FileChartDataDAOImpl( BufferedReader br ) {
                this.br = br;
        }

        public ChartData loadChartData() throws IOException {
                String filename = br.readLine();
                if (filename == null) {
                        return null;
                }
                if( filename.trim().length() == 0 ){
                        return null;                        
                }
                
                ChartData cd = new ChartData();
                cd.setFilename( filename.trim() );
                logger.finest( "on filename" + cd.getFilename() );
                cd.setTitle( br.readLine().trim() );
                cd.setSubTitle( br.readLine().trim() );
                cd.setModelName( br.readLine().trim() );
                cd.setXAxisLabel1( br.readLine().trim() );
                cd.setYAxisLabel1( br.readLine().trim() );
                cd.setStartYear( Integer.parseInt( br.readLine() ) );
                cd.setEndYear( Integer.parseInt( br.readLine() ) );
                ChartType type = ChartType.valueOf( br.readLine().trim() );
                cd.setChartType( type );
                String upIsG = br.readLine().trim();
                cd.setUpIsGood( upIsG.equals( "1" ) );
                String isThumb = br.readLine().trim();
                cd.setIsThumbnail( isThumb.equals( "1" ) );
                PreOrPost pp = PreOrPost.valueOf( br.readLine().trim() );
                cd.setPreOrPost( pp );

                int numDataSeries = Integer.parseInt( br.readLine() );

                for (int i = 0; i < numDataSeries; i++) {
                        if (pp != PreOrPost.post) {
                                String dataTitle = br.readLine();
                                String colStr = br.readLine().substring( 1 );
                                int preColour = Integer.parseInt( colStr, 16 );
                                double[] preData = Utils.simpleLineToDoubles( br.readLine() );
                                cd.addPreItem( dataTitle, preColour, preData );
                        }
                        if (pp != PreOrPost.pre) {
                                String dataTitle = br.readLine();
                                String colStr = br.readLine().substring( 1 );
                                int postColour = Integer.parseInt( colStr, 16 );
                                double[] postData = Utils.simpleLineToDoubles( br.readLine() );
                                cd.addPostItem( dataTitle, postColour, postData );
                        }
                }
                int numLabels = Integer.parseInt( br.readLine().trim() );
                if (numLabels > 0) {
                        String[] labels = new String[numLabels];
                        for (int i = 0; i < numLabels; i++) {
                                labels[i] = br.readLine().trim();
                                // e.g. Polar charts explode if sent
                                // null or zero length strings
                                if(( labels[i]==null) || ( labels[i].length() == 0 )){
                                        labels[i] = " ";
                                }
                        }
                        cd.setLabels( labels );
                }
                return cd;
        }

}
