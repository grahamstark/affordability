package org.oscr.charts;

import java.io.IOException;
import java.util.logging.Logger;

import org.oscr.data.CommonChartDetails;
import org.oscr.utils.Utils;

import ChartDirector.AngularAxis;
import ChartDirector.Axis;
import ChartDirector.BaseChart;
import ChartDirector.Chart;
import ChartDirector.DrawArea;
import ChartDirector.Layer;
import ChartDirector.LegendBox;
import ChartDirector.LineLayer;
import ChartDirector.PieChart;
import ChartDirector.PolarChart;
import ChartDirector.PolarLineLayer;
import ChartDirector.PolarAreaLayer;
import ChartDirector.RadialAxis;
import ChartDirector.TextBox;
import ChartDirector.XYChart;



public class ChartLibrary{
        
        private static Logger logger = Logger.getLogger( "ChartLibrary" );

        
        public static final double Y_DATA_SAFETY_MARGIN = 1.10;

        public static final int TEXT_BOX_WIDTH = 40;

        // define( "chartDets.getThumbnailHeight()", 40 );
        public static final int TRIANGLE_MARGINS_X = 9;

        public static final int TRIANGLE_MARGINS_Y = 9;

        public static final int RED = 0x00EE5555;

        public static final int GREEN = 0x0055DD55;
        
        public static final int GREY =  0x00888888;

        public static final int SEMI_TRANSPARENT = 0x60000000;
                                                     
        
        // from my old Utils
        /**
         * 
         */
        public static boolean nearlyEqual( double a, double b, double tol ){
                if( a == b ) return true;
                double div = ( a == 0.0 ) ? b : a;
                double diff = Math.abs( a - b )/div;
                return diff < tol;                
        }
        /**
         * 
         * @param a
         * @param b
         * @return
         */
        public static boolean nearlyEqual( double a, double b ){
                return nearlyEqual( a, b, 0.00001 );
        }
        
        /**
         * Draw the thumbnail plots on the main output page, based on the Orion ones.
         * The plots consist
         * of a simple chart of aggregates by year, followed by text with the
         * final values for the pre/post series followed by a gain/lose arrow
         * indicator if there's been an aggregate change The size of the chart
         * is chartDets.getThumbnailWidth(), plus an allowance for the text and
         * the arrows. The chart is converted into png and returned as an array of bytes
         * 
         */
        private static byte[] makeOrionTypeMiniTimeSeriesPlot( CommonChartDetails chartDets, ChartData chartData ) throws IOException {
                double[] years = ChartUtils.doubleRange( chartData.getStartYear(), chartData.getEndYear() - 1 );
                // The graph body. The chart itself has all the axes turned off
                // and goes flush left;
                // getThumbnailWidth gives the width of the actual chart, then
                // we have TEXT_BOX_WIDTH
                // which is the space for the pre/post final numbers, then a
                // square section at the
                // end for the gain/lose pointer triangle - since it's square we
                // use the hight for its width.
                XYChart graph = new XYChart( chartDets.getThumbnailWidth() + chartDets.getThumbnailHeight() + TEXT_BOX_WIDTH, chartDets.getThumbnailHeight(), chartDets.getColour1() );
                
                graph.setAntiAlias( false, Chart.AntiAlias ); // jam on anti-aliasing for text
                graph.setPlotArea( 0, 0, chartDets.getThumbnailWidth() + TEXT_BOX_WIDTH, chartDets.getThumbnailHeight(), chartDets.getColour1(), -1, Chart.Transparent, Chart.Transparent,
                                Chart.Transparent );
                Axis xAxis = graph.xAxis();
                Axis yAxis = graph.yAxis();
                xAxis.setMargin( TEXT_BOX_WIDTH );
                yAxis.setWidth( 0 );
                // only 1 dataset, with pre and post values
                double[] preValues = chartData.getPreData().get( 0 ).getData();
                double[] postValues = chartData.getPostData().get( 0 ).getData();
                
                int dataSize = Math.min(preValues.length, postValues.length ) - 1;
                LineLayer layer1 = graph.addLineLayer( preValues, chartDets.getColour3(), "pre" );
                layer1.setXData( years );
                LineLayer layer2 = graph.addLineLayer( postValues, chartDets.getColour4(), "post" );
                layer2.setXData( years );
                double lastPre = preValues[dataSize];
                double lastPost = postValues[dataSize];
                // always start graph at zero, unless we're negative                
                double minY = Math.min( 0.0, chartData.getMinValue()*Y_DATA_SAFETY_MARGIN );
                yAxis.setLinearScale( minY, chartData.getMaxValue()*Y_DATA_SAFETY_MARGIN );
                
                // text boxes with the final year's value go next.
                // The one with the higher final value goes on top
                int box1Y = ( chartDets.getThumbnailHeight() / 2 ) - 14;
                int box2Y = ( chartDets.getThumbnailHeight() / 2 );
                int preHeight = 0;
                int postHeight = 0;
                /** changed so post is always on the bottom **/
                if (lastPre >= lastPost) {
                        postHeight = box1Y;
                        preHeight = box2Y;
                } else { // point up
                        postHeight = box2Y;
                        preHeight = box1Y;
                }
                                
                postHeight = box2Y;
                preHeight = box1Y;
                TextBox box1 = graph.addText( chartDets.getThumbnailWidth() + 1, preHeight, ChartUtils.miniRealFormat( lastPre ), chartDets.getFont(), 8, chartDets.getColour3() );
                TextBox box2 = graph.addText( chartDets.getThumbnailWidth() + 1, postHeight, ChartUtils.miniRealFormat( lastPost ), chartDets.getFont(), 8, chartDets.getColour4() );
                
                // Finally, draw the gain/lose pointer at the end as a simple
                // coloured triangle if there's been
                // an aggregate change.
                double sumPre = ChartUtils.discountedSum( preValues );
                double sumPost = ChartUtils.discountedSum( postValues );
                // We use the drawing primitives for this, so we get the
                // underlying drawArea from the chart
                // and paint a polygon on to it on the rhs, just after the text
                // boxes.
                // See ChartDirector docs for this.
                DrawArea drawArea = graph.makeChart3();
                if ( ! nearlyEqual(sumPre, sumPost)) {
                        // the width is the space at the end, less the margins
                        int triangWidth = chartDets.getThumbnailHeight() - ( 2 * TRIANGLE_MARGINS_X );
                        // height then follows from Pythagoras
                        int triangHeight = (int) Math.sqrt( ( 3.0 / 4.0 ) * ( (double) triangWidth * (double) triangWidth ) );
                        // center the triangle in the middle of the end square
                        int pointX = chartDets.getThumbnailWidth() + TEXT_BOX_WIDTH + ( triangWidth / 2 ) + TRIANGLE_MARGINS_X;
                        // x-positions of the base
                        int baseLeftX = chartDets.getThumbnailWidth() + TEXT_BOX_WIDTH + TRIANGLE_MARGINS_X;
                        int baseRightX = baseLeftX + triangWidth;
                        // Then the y-coords of the top and base of the triangle
                        // point up or down depending on the total values of the
                        // two series
                        // note y coords count downwards - so 6 is 6px down from
                        // top.
                        int pointY = 0;
                        int baseY = 0;
                        int triColour = 0;
                        if (sumPost < sumPre ) { // point the triangle downwards
                                pointY = triangHeight + TRIANGLE_MARGINS_Y;
                                baseY = TRIANGLE_MARGINS_Y;
                                triColour = RED; // chartData.upIsGood() ? RED : GREEN;                                
                        } else {
                                baseY = triangHeight + TRIANGLE_MARGINS_Y;
                                pointY = TRIANGLE_MARGINS_Y;
                                triColour = GREEN; // chartData.upIsGood() ? GREEN : RED;
                        }
                        int[] triangleX = new int[] { baseLeftX, baseRightX, pointX };
                        int[] triangleY = new int[] { baseY, baseY, pointY };

                        drawArea.polygon( triangleX, triangleY, triColour, triColour );
                }
                return drawArea.outPNG2();
        }
     
        /**
         * draws the main plots in the lorenz
         */

        private static byte[] makeLorenzPlot( CommonChartDetails chartDets, ChartData chartData ) throws IOException {
                String title = chartData.getTitle();
                title += " : year";
                XYChart graph = null;
                if (chartData.isThumbnail()) {
                        graph = new XYChart( chartDets.getThumbnailWidth(), chartDets.getThumbnailHeight(), chartDets.getColour1() );
                        graph.setPlotArea( 0, 0, chartDets.getThumbnailWidth(), chartDets.getThumbnailHeight(), chartDets.getColour1(), -1, Chart.Transparent, Chart.Transparent, Chart.Transparent );

                } else {
                        graph = new XYChart( chartDets.getFullWidth(), chartDets.getFullHeight(), chartDets.getColour2() );
                        graph.setPlotArea( 60, 40, chartDets.getFullWidth()-120, chartDets.getFullHeight()-80, chartDets.getColour1(), chartDets.getColour1(), Chart.Transparent, 0xC0C0C0C0,
                                        Chart.Transparent );
                }
                Axis xAxis = graph.xAxis();
                Axis yAxis = graph.yAxis();
                graph.setAntiAlias( false, Chart.AntiAlias );
                double[] preIncomes = chartData.getPreData().get( 1 ).getData();
                double[] prePopn = chartData.getPreData().get( 0 ).getData();
                double[] postIncomes = chartData.getPostData().get( 1 ).getData();
                double[] postPopn = chartData.getPostData().get( 0 ).getData();
                
                String seriesTitle = chartData.getPreData().get( 0 ).getTitle();
              
                LineLayer layer1 = graph.addLineLayer( preIncomes, chartDets.getColour3(), seriesTitle );
                layer1.setXData( prePopn );

                LineLayer layer2 = graph.addLineLayer( postIncomes, chartDets.getColour4(), chartData.getPostData().get( 0 ).getTitle() );
                layer2.setXData( postPopn );
                
                LineLayer layer3 = graph.addLineLayer( new double[] { 0, 1 }, GREY, "" );
                
                layer3.setXData( new double[] { 0, 1 } );
                yAxis.setLinearScale( 0.0, 1.0, 0.1, 0 );
                
                if (!chartData.isThumbnail()) {
                        yAxis.setLabelStyle( chartDets.getFont(), 8 );
                        xAxis.setLabelStyle( chartDets.getFont(), 8 );
                        graph.addTitle( chartData.getTitle(), chartDets.getFont(), 12 );
                        xAxis.setTitle( chartData.getXAxisLabel1(), chartDets.getFont(), 8 );
                        yAxis.setTitle( chartData.getYAxisLabel1(), chartDets.getFont(), 8 );

                        //graph.addText( 30, (int) ( (double) chartDets.getFullHeight() * 1.26 ), chartDets.getFooter(), chartDets.getFont(), 9.0 );
                        //graph.addText( (int) ( (double) chartDets.getFullWidth() * 0.95 ), (int) ( (double) chartDets.getFullHeight() * 1.16 ), "Model:" + chartData.getModelName(), chartDets
                        //                .getFont(), 9.0 );
                        graph.addLegend( chartDets.getFullWidth() + 105, 20, true, chartDets.getFont(), 8 );
                        graph.addText( 30, (int) ( (double) chartDets.getFullHeight() * 0.96 ), chartDets.getFooter(), chartDets.getFont(), 9.0 );
                        graph.addText( (int) ( (double) chartDets.getFullWidth() * 0.90 ), (int) ( (double) chartDets.getFullHeight() * 0.96 ), "Model: "  + chartData.getModelName(), chartDets.getFont(), 9.0 );
                        
                        
                } else {
                        yAxis.setColors( Chart.Transparent );
                        xAxis.setColors( Chart.Transparent );
                        graph.setBorder( Chart.Transparent );
                }
                logger.finest( "writing chart to "+chartData.getFilename());
                return graph.makeChart2( Chart.PNG );
        }
        
        private static byte[] makeMiniTimeSeriesPlot( CommonChartDetails chartDets, ChartData chartData ) throws IOException {
                double[] years = ChartUtils.doubleRange( chartData.getStartYear(), chartData.getEndYear() );
                String[] yearStr = ChartUtils.stringRange( chartData.getStartYear(), chartData.getEndYear() );
                int nYears = years.length;
                XYChart graph = new XYChart( chartDets.getThumbnailWidth(), chartDets.getThumbnailHeight(), chartDets.getColour2() );
                graph.setAntiAlias( false, Chart.AntiAlias );                
                graph.setPlotArea( 1, 1, chartDets.getThumbnailWidth(), chartDets.getThumbnailHeight(), chartDets.getColour1(), chartDets.getColour1(), Chart.Transparent, 0xC0C0C0C0, Chart.Transparent );
                Axis xAxis = graph.xAxis();
                Axis yAxis = graph.yAxis();
                double maxY = Double.MIN_VALUE;
                for (ChartDataItem data : chartData.getPreData()) {
                        Layer layer1 = graph.addLineLayer( data.getData(), data.getColour(), "" );
                }
                for (ChartDataItem data : chartData.getPostData()) {
                        Layer layer1 = graph.addLineLayer( data.getData(), data.getColour(), "" );
                }
                // set the format based on the most extreme value, positive or negative
                maxY = Math.max( maxY, chartData.getMaxValue() );
                maxY = Math.max( maxY, Math.abs(chartData.getMinValue()) );
                // always start graph at zero, unless we're negative                
                xAxis.setLinearScale( 0, nYears, yearStr );
                
                yAxis.setWidth( 0 );
                xAxis.setWidth( 0 );
                // always start graph at zero, unless we're negative                
                double minY = Math.min( 0.0, chartData.getMinValue()*Y_DATA_SAFETY_MARGIN );
                yAxis.setLinearScale( minY, chartData.getMaxValue()*Y_DATA_SAFETY_MARGIN );
                
                /**
                if( maxY > 1000.0 ){
                        yAxis.setLabelFormat( "{value|0,}" );
                } else if ( maxY > 1.0 ) {
                        yAxis.setLabelFormat( "{value|1,.}" );
                } else {
                        yAxis.setLabelFormat( "{value|2,.}" );
                }
                yAxis.setLabelStyle( chartDets.getFont(), 8 );
                xAxis.setLabelStyle( chartDets.getFont(), 8 );
                **/
                return graph.makeChart2( Chart.PNG );
         }

        /**
         * draws the main plots in the Macro pop-up windows.
         */

        private static byte[] makeMaxiTimeSeriesPlot( CommonChartDetails chartDets, ChartData chartData )  throws IOException {
                double[] years = ChartUtils.doubleRange( chartData.getStartYear(), chartData.getEndYear() );
                String[] yearStr = ChartUtils.stringRange( chartData.getStartYear(), chartData.getEndYear() );
                int nYears = years.length;
                
                XYChart graph = new XYChart( chartDets.getFullWidth(), chartDets.getFullHeight(), chartDets.getColour2() );
                graph.setAntiAlias( false, Chart.AntiAlias );
                
                graph.setPlotArea( 60, 40, chartDets.getFullWidth()-120, chartDets.getFullHeight()-90, chartDets.getColour1(), chartDets.getColour1(), Chart.Transparent, 0xC0C0C0C0, Chart.Transparent );
                Axis xAxis = graph.xAxis();
                Axis yAxis = graph.yAxis();
                double maxY = Double.MIN_VALUE;
                // set the format based on the most extreme value, positive or negative
                maxY = Math.max( maxY, chartData.getMaxValue() );
                maxY = Math.max( maxY, Math.abs(chartData.getMinValue()) );
                double minY = Math.min( 0.0, chartData.getMinValue()*Y_DATA_SAFETY_MARGIN );
  
                xAxis.setLinearScale( 0, nYears, yearStr );
                String units = "";
                double divisor = 1.0;
                /*if( maxY > 1000000 ){
                        yAxis.setLabelFormat( "{value|1,}" );
                        divisor = 1000000.0;
                        units = "(mn)";
                        maxY /= 1000000.0;
                        minY /= 1000000.0;
                } else */
                if( maxY > 1000.0 ){
                        yAxis.setLabelFormat( "{value|0,}" );
                } else if ( maxY > 1.0 ) {
                        yAxis.setLabelFormat( "{value|1,.}" );
                } else {
                        yAxis.setLabelFormat( "{value|2,.}" );
                }
                if( maxY < 1000000.0 ){
                        yAxis.setLabelStyle( chartDets.getFont(), 8 );
                } else {
                        yAxis.setLabelStyle( chartDets.getFont(), 6 );                        
                }
                // always start graph at zero, unless we're negative                
                
                for (ChartDataItem data : chartData.getPreData()) {
                        double[] v = data.getData();
                        v = Utils.divideArray( v, divisor );       
                        Layer layer1 = graph.addLineLayer( v, data.getColour(), data.getTitle() );// + " : pre" );
                }
                for (ChartDataItem data : chartData.getPostData()) {
                        double[] v = data.getData();
                        v = Utils.divideArray( v, divisor );
                        Layer layer1 = graph.addLineLayer( v, data.getColour(), data.getTitle() ); //+ " : post" );
                }
                
                yAxis.setLinearScale( minY, maxY*Y_DATA_SAFETY_MARGIN );
                TextBox xAxisStyle = xAxis.setLabelStyle( chartDets.getFont(), 8 );
                if( nYears > 18 ){  // slant to right if not enough space for everything. Should probably have num depend on chart size
                        xAxisStyle.setFontAngle( 45.0 ); 
                }
                graph.addTitle( chartData.getTitle(), chartDets.getFont(), 12 );
                graph.addTitle( chartData.getSubTitle(), chartDets.getFont(), 10 );
                xAxis.setTitle( chartData.getXAxisLabel1(), chartDets.getFont(), 8 );
                yAxis.setTitle( chartData.getYAxisLabel1()+units, chartDets.getFont(), 8 );
                graph.addText( 30, (int) ( (double) chartDets.getFullHeight() * 0.96 ), chartDets.getFooter(), chartDets.getFont(), 8.0 );
                graph.addText( (int) ( (double) chartDets.getFullWidth() * 0.90 ), (int) ( (double) chartDets.getFullHeight() * 0.96 ), "Model:"  + chartData.getModelName(), chartDets.getFont(), 8.0 );
                graph.addLegend( chartDets.getFullWidth()-100, 20, true, chartDets.getFont(), 8 );
                return graph.makeChart2( Chart.PNG );
                
        }

        private static byte[] drawUnivariateChart( CommonChartDetails chartDets, ChartData chartData ) throws IOException {
                ChartType type = chartData.getChartType();
                int height = 0;
                int width = 0;
                if (!chartData.isThumbnail()) {
                        height = chartDets.getFullHeight();
                        width = chartDets.getFullWidth();
                } else {
                        height = chartDets.getThumbnailHeight();
                        width = chartDets.getThumbnailWidth();
                }
                double yAxisFontSize = width > 400 ? 9.0 : 8.0;

                int colour3_semiTransparent = chartDets.getColour3() | SEMI_TRANSPARENT;
                int colour4_semiTransparent = chartDets.getColour4() | SEMI_TRANSPARENT;
                int centre = height / 2;
                int radarWidth = height / 3;
                BaseChart graph = null;
                switch ( type ) {
                case radar:
                        width = height; // square
                        graph = new PolarChart( width, height, chartDets.getColour2() );
                        ( (PolarChart) graph ).setPlotArea( centre, centre, radarWidth, chartDets.getColour1() );
                        break;
                case pie:
                        width = height;
                        graph = new PieChart( width, height, chartDets.getColour1() );
                        if (!chartData.isThumbnail()) {
                                ( (PieChart) graph ).set3D();
                        }
                        ( (PieChart) graph ).setPieSize( centre, centre, radarWidth );
                        break;
                case bar:
                        graph = new XYChart( width, height, chartDets.getColour2() );
                        ( (XYChart) graph ).yAxis().setLabelFormat( "{value|0,}" );
                        if (!chartData.isThumbnail()) {
                                ( (XYChart) graph ).setPlotArea( (int) ( (double) width * 0.15 ), (int) ( (double) height * 0.07 ), (int) ( (double) width * 0.80 ), (int) ( (double) height * 0.80 ),
                                                chartDets.getColour1() );
                                ( (XYChart) graph ).yAxis().setTitle( chartData.getYAxisLabel1(), chartDets.getFont(), 8 );
                        } else {
                                ( (XYChart) graph ).setPlotArea( 0, 0, width, height, chartDets.getColour1() );
                        }
                        break;
                default: // some other chart we can't actually handle
                        return new byte[0];
                }
                graph.setAntiAlias( false, Chart.AntiAlias );
                if (!chartData.isThumbnail()) {
                        graph.addTitle( chartData.getTitle(), chartDets.getFont(), 12 );
                        graph.addTitle( chartData.getSubTitle(), chartDets.getFont(), 10 );
                        if( type != ChartType.pie ){
                                LegendBox legend = graph.addLegend( height - 40, 20, true, chartDets.getFont(), 8 );
                                legend.setAlignment( Chart.TopRight );
                                legend.setBackground( chartDets.getColour2(), Chart.Transparent, 1 );
                        }
                        if (type == ChartType.bar) {
                                graph.addText( 30, (int) ( (double) height * 0.95 ), chartDets.getFooter(), chartDets.getFont(), 8 );
                                graph.addText( (int) ( (double) width * 0.80 ), (int) ( (double) height * 0.95 ), "Model: " + chartData.getModelName(), chartDets.getFont(), 8 );
                        } else {
                                graph.addText( 10, (int) ( (double) height * 0.95 ), chartDets.getFooter(), chartDets.getFont(), 8 );
                                graph.addText( (int) ( (double) width * 0.70 ), (int) ( (double) height * 0.95 ), "Model: " + chartData.getModelName(), chartDets.getFont(), 8 );
                        }
                }

                if (type == ChartType.radar) {
                        // always use both systems as shares
                        ChartDataItem pre = chartData.getPreData().get( 0 );
                        ChartDataItem post = chartData.getPostData().get( 0 );
                        double[] preValues = ChartUtils.toShares( pre.getData() );
                        double[] postValues = ChartUtils.toShares( post.getData() );
                        if( ChartUtils.unsuitableForRadarChart( postValues, preValues )){
                                return new byte[0]; // these charts die with no data
                        }
                        
                        PolarChart pgraph = (PolarChart) graph;
                        PolarAreaLayer area1 = pgraph.addAreaLayer( preValues, colour3_semiTransparent, "Base Scenario" );
                        PolarLineLayer line1 = pgraph.addLineLayer( preValues, chartDets.getColour3() );
                        line1.setLineWidth( 1 );
                        PolarAreaLayer area2 = pgraph.addAreaLayer( postValues, colour4_semiTransparent, "Alt Scenario" );
                        PolarLineLayer line2 = pgraph.addLineLayer( postValues, chartDets.getColour4() );
                        line2.setLineWidth( 1 );
                        AngularAxis angularAxisObj = pgraph.angularAxis();
                        RadialAxis radialAxisObj = pgraph.radialAxis();
                        TextBox textBox = angularAxisObj.setLabels( chartData.getLabels() );
                        textBox.setFontAngle( 40.0 ); // slant the text so it doesn't bunch up
                       // Set the labels to the angular axis as spokes.
                        if (chartData.isThumbnail()) { // FIXME: can't turn
                                // these numbers off!
                                radialAxisObj.setLabelStyle( chartDets.getFont(), 0, Chart.Transparent );
                                radialAxisObj.setWidth( 0 );
                        }
                } else if (type == ChartType.pie) {
                        // only use sys1 or sys2
                        PieChart pgraph = (PieChart) graph;
                        if (chartData.getPreOrPost() == PreOrPost.pre) {
                                ChartDataItem pre = chartData.getPreData().get( 0 );
                                pgraph.setData( pre.getData(), chartData.getLabels() );
                        } else {
                                ChartDataItem post = chartData.getPostData().get( 0 );
                                pgraph.setData( post.getData(), chartData.getLabels() );
                        }
                        if (chartData.isThumbnail()) {
                                pgraph.setLabelStyle( chartDets.getFont(), 0, Chart.Transparent ); // all
                                // labels
                                // off
                                // in
                                // thumbnail
                        } else {
                                TextBox textBox = pgraph.setLabelStyle( chartDets.getFont() );
                                textBox.setFontAngle( 20.0 ); // slant the text so it doesn't bunch up
                                
                        }
                } else if (type == ChartType.bar) {
                        ChartDataItem pre = chartData.getPreData().get( 0 );
                        ChartDataItem post = chartData.getPostData().get( 0 );
                        
                        XYChart xgraph = (XYChart) graph;
                        Axis yAxis = xgraph.yAxis();
                                        String units = "";
                        double maxY = Double.MIN_VALUE;
                                        // set the format based on the most extreme value, positive or negative
                        maxY = Math.max( maxY, chartData.getMaxValue() );
                        maxY = Math.max( maxY, Math.abs(chartData.getMinValue()) );
                        double minY = Math.min( 0.0, chartData.getMinValue()*Y_DATA_SAFETY_MARGIN );
                        double divisor = 1.0;
                        if( maxY > 1000000 ){
                                yAxis.setLabelFormat( "{value|1,}" );
                                divisor = 1000000.0;
                                units = "(mn)";
                                maxY /= 1000000.0;
                                minY /= 1000000.0;
                        } else if( maxY > 1000.0 ){
                                yAxis.setLabelFormat( "{value|0,}" );
                        } else if ( maxY > 1.0 ) {
                                yAxis.setLabelFormat( "{value|1,.}" );
                        } else {
                                yAxis.setLabelFormat( "{value|2,.}" );
                        }
        
                        yAxis.setLinearScale( minY, maxY*Y_DATA_SAFETY_MARGIN );
                        
                        Layer layer = xgraph.addBarLayer();
                        
                        double[] vpre = pre.getData();
                        vpre = Utils.divideArray( vpre, divisor );
                        double[] vpost = post.getData();
                        vpost = Utils.divideArray( vpost, divisor );

                        switch ( chartData.getPreOrPost() ) {
                        case pre:
                                layer.addDataSet( vpre, chartDets.getColour3() );
                                break;
                        case post:
                                layer.addDataSet( vpost, chartDets.getColour4() );
                                break;
                        case both:
                                layer.addDataSet( vpre, chartDets.getColour3(), "Pre" );
                                layer.addDataSet( vpost, chartDets.getColour4(), "Post" );
                                break;
                        case abs_change:
                                layer.addDataSet( ChartUtils.absDifferences( vpre, vpost ), chartDets.getColour3() );
                                break;
                        case pct_change:
                                break;
                        }
                        xgraph.xAxis().setLabels( chartData.getLabels() );
                        TextBox xbox = xgraph.xAxis().setLabelStyle( chartDets.getFont() );
                        xbox.setFontAngle( 20.0 );
                        yAxis.setTitle( chartData.getYAxisLabel1()+units, chartDets.getFont(), 8 );
                        TextBox ybox = xgraph.yAxis().setLabelStyle( chartDets.getFont(), yAxisFontSize );
                }
                return graph.makeChart2( Chart.PNG );
        }
        
        public static byte[] drawChart( CommonChartDetails chartDets, ChartData chartData ) throws IOException{
                byte[] buff = null;
                switch( chartData.getChartType()){
                case pie:
                case radar:
                case bar:
                        buff = drawUnivariateChart( chartDets, chartData );
                        break;
                case time_series:
                        if( chartData.isThumbnail()){
                                if( chartData.isOrionTypeThumbnail() ){
                                        buff = makeOrionTypeMiniTimeSeriesPlot( chartDets, chartData );
                                } else {
                                        buff = makeMiniTimeSeriesPlot( chartDets, chartData );
                                }
                        } else {
                                buff = makeMaxiTimeSeriesPlot( chartDets, chartData );
                        }
                        break;
                case lorenz :
                        buff = makeLorenzPlot(chartDets, chartData );
                }
                return buff;
       }
        
        public static void writeChart( CommonChartDetails chartDets, ChartData chartData ) throws IOException{
                byte[] buff = drawChart( chartDets, chartData ); 
                if( buff != null ){
                        Utils.writeFileInBinary( chartData.getFilename(), buff );
                }
        }

}
