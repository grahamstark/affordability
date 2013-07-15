/**
 * 
 */
package org.oscr.charts;

import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

/**
 * @author graham_s
 *
 */


public class ChartData implements Serializable {
	
	private String title;
        private String filename;
	private String subTitle;
	private List<ChartDataItem> preData;	
	private List<ChartDataItem> postData;
        private String xAxisLabel1;
        private String yAxisLabel1;
        private int startYear;
        private int endYear;
        private boolean orionTypeThumbnail = true;	
        

        
        
        private ChartType chartType;
        private boolean upIsGood;
        private boolean isThumbnail;
        private String modelName;
        private String[] labels;
        private PreOrPost preOrPost;
        private double minValue = Double.MAX_VALUE;
        private double maxValue = Double.MIN_VALUE;
        
        
        
        public double getMinValue(){
                return this.minValue;
        }
        
        public double getMaxValue(){
                return this.maxValue;
        }
        
        
        public PreOrPost getPreOrPost() {
                return preOrPost;
        }

        public void setPreOrPost( PreOrPost preOrPost ) {
                this.preOrPost = preOrPost;
        }

        public String[] getLabels() {
                return labels;
        }

        public void setLabels( String[] labels ) {
                this.labels = labels;
        }

        public boolean isThumbnail(){
                return this.isThumbnail;
        }
        
        public void setIsThumbnail( boolean isThumbnail ){
                this.isThumbnail = isThumbnail;
        }
        
        public boolean upIsGood() {
                return upIsGood;
        }

        public void setUpIsGood( boolean upIsGood ) {
                this.upIsGood = upIsGood;
        }

        public ChartData(){
                this.postData = new ArrayList<ChartDataItem>();
                this.preData = new ArrayList<ChartDataItem>();
        }
        
        public void addPreItem( String title, int colour, double[] data ){
                ChartDataItem item = new ChartDataItem( title, colour, data );
                this.preData.add( item );
                this.checkMinsAndMaxes( data );
        }
        
        private void checkMinsAndMaxes( double[] data ){
                for( double x : data ){
                        if( x > this.maxValue ){
                                this.maxValue = x;
                        }
                        if( x < this.minValue ){
                                this.minValue = x;
                        }
                }
        }
        
        public void checkGlobalMinsAndMaxes(){
                this.minValue = Double.MAX_VALUE;
                this.maxValue = Double.MIN_VALUE;
                for(ChartDataItem item : this.preData ){
                        checkMinsAndMaxes( item.getData());                        
                }
                for(ChartDataItem item : this.postData ){
                        checkMinsAndMaxes( item.getData());                        
                }
        }

        public void addPostItem( String title, int colour, double[] data ){
                ChartDataItem item = new ChartDataItem( title, colour, data );
                this.postData.add( item );
                this.checkMinsAndMaxes( data );
        }
        
	public List <ChartDataItem> getPostData() {
		return postData;
	}
	public void setPostData( List<ChartDataItem> postData) {
		this.postData = postData;
                this.checkGlobalMinsAndMaxes();
	}
	public List <ChartDataItem> getPreData(){
		return preData;
	}
	public void setPreData( List<ChartDataItem> preData) {
		this.preData = preData;
                this.checkGlobalMinsAndMaxes();
	}
	public String getSubTitle() {
		return subTitle;
	}
	public void setSubTitle(String subTitle) {
		this.subTitle = subTitle;
	}
	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}
        public ChartType getChartType() {
                return chartType;
        }
        public void setChartType( ChartType chartType ) {
                this.chartType = chartType;
        }
        public int getEndYear() {
                return endYear;
        }
        public void setEndYear( int endYear ) {
                this.endYear = endYear;
        }
        public int getStartYear() {
                return startYear;
        }
        public void setStartYear( int startYear ) {
                this.startYear = startYear;
        }
        public String getXAxisLabel1() {
                return xAxisLabel1;
        }
        public void setXAxisLabel1( String xAxisLabel1 ) {
                this.xAxisLabel1 = xAxisLabel1;
        }
        
        public String getYAxisLabel1() {
                return yAxisLabel1;
        }
        public void setYAxisLabel1( String yAxisLabel1 ) {
                this.yAxisLabel1 = yAxisLabel1;
        }

        public String getFilename() {
                return filename;
        }

        public void setFilename( String filename ) {
                this.filename = filename;
        }

        public String getModelName() {
                return modelName;
        }

        public void setModelName( String modelName ) {
                this.modelName = modelName;
        }
        
        /**
	 * Returns the value of orionTypeThumbnail.
	 */
	public boolean isOrionTypeThumbnail(){
		return orionTypeThumbnail;
	}

	/**
	 * Sets the value of orionTypeThumbnail.
	 * @param orionTypeThumbnail The value to assign orionTypeThumbnail.
	 */
	public void setIsOrionTypeThumbnail( boolean orionTypeThumbnail ){
		this.orionTypeThumbnail = orionTypeThumbnail;
	}

}