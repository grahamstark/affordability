package org.oscr.charts;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.util.logging.Logger;
import java.util.List;

import org.jfree.chart.plot.PiePlot;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.CategoryItemRenderer;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.general.PieDataset;
import org.oscr.data.CommonChartDetails;
import org.oscr.utils.StdColours;
import org.oscr.utils.StdFonts;
import org.oscr.utils.Utils;

public class JFreeChartLibrary{

        private static Logger logger = Logger.getLogger( "ChartLibrary" );

        public static final double Y_DATA_SAFETY_MARGIN = 1.10;

        public static final int TEXT_BOX_WIDTH = 40;

        // define( "chartDets.getThumbnailHeight()", 40 );
        public static final int TRIANGLE_MARGINS_X = 9;

        public static final int TRIANGLE_MARGINS_Y = 9;

        public static final int RED = 0x00EE5555;

        public static final int GREEN = 0x0055DD55;

        public static final int GREY = 0x00888888;

        public static final int SEMI_TRANSPARENT = 0x60000000;

        // from my old Utils
        /**
         * 
         */
        public static boolean nearlyEqual( double a, double b, double tol ) {
                if (a == b)
                        return true;
                double div = ( a == 0.0 ) ? b : a;
                double diff = Math.abs( a - b ) / div;
                return diff < tol;
        }

        /**
         * 
         * @param a
         * @param b
         * @return
         */
        public static boolean nearlyEqual( double a, double b ) {
                return nearlyEqual( a, b, 0.00001 );
        }

        public static byte[] chartToBytes( JFreeChart chart, int width, int height, boolean isSvg ) throws IOException {
                byte[] out;

                if (!isSvg) {
                        ByteArrayOutputStream fo = new ByteArrayOutputStream();
                        ChartUtilities.writeChartAsPNG( fo, chart, width, height, true, 3 );
                        out = fo.toByteArray();
                } else {
                        StringWriter wr = new StringWriter();
                        JPlotUtils.writeChartAsSVG( wr, chart, width, height );
                        out = wr.toString().getBytes();
                }
                return out;
        }

        public static byte[] drawBar( CommonChartDetails chartDets, ChartData chartData, boolean isSvg ) throws IOException {
                JFreeChart chart;
                CategoryDataset barData = new DefaultCategoryDataset();
                double[] data = ( chartData.getPreOrPost() == PreOrPost.pre ) ? chartData.getPreData().get( 0 ).getData() : chartData.getPostData().get( 0 ).getData();
                String[] labels = chartData.getLabels();

                for (int i = 0; i < data.length; i++) {
                        ( (DefaultCategoryDataset) barData ).addValue( data[i], "", labels[i] );
                }
                int width = chartData.isThumbnail() ? chartDets.getThumbnailWidth() : chartDets.getFullWidth();
                int height = chartData.isThumbnail() ? chartDets.getThumbnailHeight() : chartDets.getFullHeight();
                boolean legend = false; // ! chartData.isThumbnail();
                String xAxisTitle = chartData.isThumbnail() ? "" : "";
                String yAxisTitle = chartData.isThumbnail() ? "" : "";
                String header = chartData.isThumbnail() ? "" : chartData.getTitle();
                chart = ChartFactory.createBarChart( 
                                header, 
                                xAxisTitle, 
                                yAxisTitle, 
                                barData, 
                                PlotOrientation.HORIZONTAL, 
                                legend, // legend
                                false, // tooltips ??
                                false ); // urls ??
                CategoryItemRenderer renderer = chart.getCategoryPlot().getRenderer();

                renderer.setSeriesPaint( 0, StdColours.GRAY_BLUE.getColor() );
                chart.getPlot().setBackgroundPaint( Color.WHITE );
                chart.getPlot().setForegroundAlpha( 1.0f );
                chart.setBackgroundPaint( Color.WHITE );
                CategoryAxis domainAxis = chart.getCategoryPlot().getDomainAxis();
                ValueAxis rangeAxis = chart.getCategoryPlot().getRangeAxis();
                        
                if( chartData.isThumbnail() ){
                        chart.setBorderVisible( false );
                        renderer.setItemLabelsVisible( false );
                        domainAxis.setVisible( false );
                        rangeAxis.setVisible( false );
                } else {
                        domainAxis.setLabelPaint( Color.BLACK );
                        domainAxis.setTickLabelPaint( Color.BLACK );
                        domainAxis.setAxisLinePaint( Color.BLACK );
                        domainAxis.setTickLabelFont( StdFonts.GILL_SANS_BOLD_10 );
                        domainAxis.setTickLabelPaint( Color.BLACK );
                        rangeAxis.setTickLabelFont( StdFonts.GILL_SANS_BOLD_10 );
                        rangeAxis.setLabelPaint( Color.BLACK );
                        rangeAxis.setLabelFont( StdFonts.GILL_SANS_BOLD_10 );
                        rangeAxis.setTickLabelPaint( Color.BLACK );
                        rangeAxis.setAxisLinePaint( Color.BLACK );
                        rangeAxis.setTickLabelPaint( Color.BLACK );
                        chart.setTextAntiAlias( true );
                }
                return chartToBytes( chart, width, height, isSvg );
        }

    /*
     * A simple renderer for setting custom colors
     * for a pie chart. From: http://javabeanz.wordpress.com/2007/08/06/creating-pie-charts-using-custom-colors-jfreechart/
     */
    public static class PieRenderer{
        private Color[] color;
       
        public PieRenderer(Color[] color){
            this.color = color;
        }       
       
        public void setColor(PiePlot plot, DefaultPieDataset dataset){
            List <Comparable> keys = dataset.getKeys();
            int aInt;
           
            for (int i = 0; i < keys.size(); i++){
                aInt = i % this.color.length;
                plot.setSectionPaint(keys.get(i), this.color[aInt]);
            }
        }
    }
        
        public static byte[] drawPie( CommonChartDetails chartDets, ChartData chartData, boolean isSvg ) throws IOException {
                JFreeChart chart;
                PieDataset pieData = new DefaultPieDataset();
                double[] data = ( chartData.getPreOrPost() == PreOrPost.pre ) ? chartData.getPreData().get( 0 ).getData() : chartData.getPostData().get( 0 ).getData();
                String[] labels = chartData.getLabels();

                for (int i = 0; i < data.length; i++) {
                        ( (DefaultPieDataset) pieData ).setValue( labels[i], data[i] );
                        
                }
                int width = chartData.isThumbnail() ? chartDets.getThumbnailWidth() : chartDets.getFullWidth();
                int height = chartData.isThumbnail() ? chartDets.getThumbnailHeight() : chartDets.getFullHeight();
                boolean legend = ! chartData.isThumbnail();
                String header = chartData.isThumbnail() ? null : chartData.getTitle();
                chart = ChartFactory.createPieChart( header, pieData, legend, false, false );
                if( chartData.isThumbnail() ){
                        ((PiePlot)chart.getPlot()).setLabelGenerator( null );
                }
                //PieRenderer renderer = new PieRenderer( );
                //renderer.setColor( (PiePlot)chart.getPlot(), pieData );
                // fixme add fonts!!
                chart.setTextAntiAlias( true );

                return chartToBytes( chart, width, height, isSvg );
        }

        public static byte[] drawChart( CommonChartDetails chartDets, ChartData chartData, boolean isSvg ) throws IOException {
                byte[] buff = null;
                switch ( chartData.getChartType() ) {
                case pie:
                        buff = drawPie( chartDets, chartData, isSvg );
                        break;
                case bar:
                        buff = drawBar( chartDets, chartData, isSvg );
                        break;
                case time_series:
                        break;
                case lorenz:
                        break;
                case radar:
                        break;
                }
                return buff;
        }

        public static boolean writeChart( CommonChartDetails chartDets, ChartData chartData, boolean isSvg ) throws IOException {
                isSvg = isSvg & ( ! chartData.isThumbnail() );
                byte[] buff = drawChart( chartDets, chartData, isSvg );
                String ext = isSvg ? ".svg" : ".png";
                if (buff != null) {
                        Utils.writeFileInBinary( chartData.getFilename() + ext, buff );
                        return true;
                }
                return false;
        }
}
