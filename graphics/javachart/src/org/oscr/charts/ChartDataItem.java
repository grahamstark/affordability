package org.oscr.charts;

import java.io.Serializable;

public class ChartDataItem implements Serializable {
        
        private double[] data;
        private String title;
        private int colour;
        
        public ChartDataItem(String title, int colour,double[] data ){
                this.data = data;
                this.title = title;
                this.colour = colour;
        }
        
        public int getColour() {
                return colour;
        }
        public void setColour( int colour ) {
                this.colour = colour;
        }
        public double[] getData() {
                return data;
        }
        public void setData( double[] data ) {
                this.data = data;
        }
        public String getTitle() {
                return title;
        }
        public void setTitle( String title ) {
                this.title = title;
        }
        
        
        
}
