package org.oscr.data;

public class CommonChartDetails {
        
        
        
        private String footer = "copyright(c) ADRS all rights reserved";
        private String font;
        private int[] colour; // chart director colours as ints
        private int thumbnailHeight;
        private int thumbnailWidth;
        private int fullHeight;
        private int fullWidth;
        
        public int getColour1(){
                return this.colour[0];
        }
        public int getColour2(){
                return this.colour[1];
        }
        public int getColour3(){
                return this.colour[2];
        }
        public int getColour4(){
                return this.colour[3];
        }
        
        public int[] getColour() {
                return colour;
        }
        public void setColour( int[] colour ) {
                this.colour = colour;
        }
        public String getFont() {
                return font;
        }
        public void setFont( String font ) {
                this.font = font;
        }
        public String getFooter() {
                return footer;
        }
        public void setFooter( String footer ) {
                this.footer = footer;
        }
        public int getFullHeight() {
                return fullHeight;
        }
        public void setFullHeight( int fullHeight ) {
                this.fullHeight = fullHeight;
        }
        public int getFullWidth() {
                return fullWidth;
        }
        public void setFullWidth( int fullWidth ) {
                this.fullWidth = fullWidth;
        }
        public int getThumbnailHeight() {
                return thumbnailHeight;
        }
        public void setThumbnailHeight( int thumbnailHeight ) {
                this.thumbnailHeight = thumbnailHeight;
        }
        public int getThumbnailWidth() {
                return thumbnailWidth;
        }
        public void setThumbnailWidth( int thumbnailWidth ) {
                this.thumbnailWidth = thumbnailWidth;
        }
                
        
}
