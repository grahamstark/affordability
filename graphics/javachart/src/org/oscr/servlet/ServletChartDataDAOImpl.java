package org.oscr.servlet;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.net.URLCodec;
import org.oscr.charts.ChartData;
import org.oscr.charts.ChartType;
import org.oscr.charts.PreOrPost;
import org.oscr.data.ChartDataDAO;
import org.oscr.utils.Utils;



public class ServletChartDataDAOImpl implements ChartDataDAO{

        private HttpServletRequest req;
        private URLCodec decoder;
         
        public ServletChartDataDAOImpl( HttpServletRequest req ) {
                this.req = req;
                this.decoder = Utils.getURLEncoder();
        }

        public ChartData loadChartData() throws IOException {
                ChartData cd = new ChartData();
                int numDataSeries = Integer.parseInt( req.getParameter( "num_series" ));
                // this is a Hack : jam on simple thumbnails for everything coming from the web
                // (and complex ones from everything from the driver file 
                cd.setIsOrionTypeThumbnail( false );
                try{
                        cd.setTitle( this.decoder.decode(req.getParameter( "title" ) ));
                        cd.setSubTitle(this.decoder.decode(req.getParameter( "subtitle" )));
                        cd.setModelName( this.decoder.decode(req.getParameter( "model" )));
                        cd.setXAxisLabel1( this.decoder.decode(req.getParameter( "xlab" )));
                        cd.setYAxisLabel1( this.decoder.decode(req.getParameter( "ylab" )));
                        cd.setStartYear( Integer.parseInt( req.getParameter( "start_year" ) ) );
                        cd.setEndYear( Integer.parseInt( req.getParameter( "end_year" ) ) );
                        ChartType type = ChartType.valueOf( req.getParameter( "chart_type" ));
                        cd.setChartType( type );
                        String upIsG = req.getParameter( "up_good" );
                        cd.setUpIsGood( "1".equals(upIsG ) );
                        String isThumb = req.getParameter( "is_thumb" );
                        cd.setIsThumbnail( "1".equals(isThumb));
                        PreOrPost pp = PreOrPost.valueOf( req.getParameter( "pre_or_post" ) );
                        cd.setPreOrPost( pp );
                        for (int i = 1; i <= numDataSeries; i++) {
                                int urlIndex = i+1;
                                if (pp != PreOrPost.post) {
                                        String dataTitle =  this.decoder.decode(req.getParameter( "dt_pre_"+i ));
                                        String colStr = req.getParameter( this.decoder.decode("col_pre_"+i) ).substring( 1 );
                                        int preColour = Integer.parseInt( colStr, 16 );
                                        double[] preData = Utils.simpleLineToDoubles( req.getParameter( "da_pre_"+i ));
                                        cd.addPreItem( dataTitle, preColour, preData );
                                }
                                if (pp != PreOrPost.pre) {
                                        String dataTitle =  this.decoder.decode(req.getParameter( "dt_post_"+i ));
                                        String colStr = req.getParameter( this.decoder.decode("col_post_"+i )).substring( 1 );
                                        int postColour = Integer.parseInt( colStr, 16 );
                                        double[] postData = Utils.simpleLineToDoubles( req.getParameter( "da_post_"+i ));
                                        cd.addPostItem( dataTitle, postColour, postData );
                                }
                        }                
                } catch ( DecoderException e ){
                        throw new IOException( e.getMessage() );       
                }
                return cd;
        }

}
