/**
 * This bulk-creates charts using chart definitions produced by the php model driver 
 * code, written to a text file.
 *   
 */
package org.oscr.charts;

import org.oscr.data.ChartDataCommons;
import org.oscr.data.ChartDataDAO;
import org.oscr.data.CommonChartDetails;
import org.oscr.data.CommonChartDetailsDAO;



public class ChartCommons{

        public static final boolean CREATE_SVG = true;
        
        public static final String HTML_FILE_HEADER = 
                "<?xml version='1.0' encoding='UTF-8'?>\n"+
                "<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN'\n"+ 
                "'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'>\n"+ 
                "<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en'>\n"+
                "<head>\n"+
                "<title>Charts Listing</title>\n"+
                "<style type='text/css'>\n"+
                "body{\n"+
                "        font-family:\"Gill Sans\",Arial,Helvetica,Geneva,sans-serif;\n"+
                "        font-size: 8pt;\n"+
                "        color: black;\n"+
                "        padding: 4em"+
                "        background-color:white;\n"+
                "}\n"+
                "table{\n"+
                "        border-collapse: collapse;\n"+ 
                "        text-align: left;\n"+
                "}\n"+
                "a{\n"+
                "      text-decoration:none;\n"+
                "      color: #229911;\n"+
                "}\n"+
                "table a:hover{\n"+
                "      text-decoration: none;\n"+
                "      background:  #aaddbb;\n"+ 
                "}\n"+
                "tr{\n"+
                "      border-bottom: 1px dotted silver;\n"+
                "}\n"+
                "th{\n"+
                "       font-weight: bold;\n"+
                "       color: black;\n"+
                "       text-align: left;\n"+
                "}\n"+
                "</style>\n"+
                "</head>\n"+
                "<body>\n"+
                "<h1>Complete Charts Listing</h1>\n"+
                "<table>\n"+
                "    <tr><th></th colspan='2'><th><th>Start Year</th><th>End Year</th><th>Chart Type</th><th>Thumbnail?</th></tr>\n";
        
        public static String descriptionToHTML( int n, ChartData data, boolean isSvg ){
                String classStr = (( n % 2 ) == 0)? "tableRowEven" : "tableRowOdd";
                int p = data.getFilename().lastIndexOf( '/' ) + 1;
                isSvg = isSvg & ( ! data.isThumbnail() );
                String ext = isSvg ? ".svg" : ".png";
                String filename = data.getFilename().substring( p ) + ext;
                String url = "<a href='"+ filename +"'>"+ data.getTitle() + "</a>";
                String s = "    <tr class='"+classStr+"'>";
                s += "<td>"+ url +"</td><td>"+data.getSubTitle()+"</td>";
                String syear = (data.getStartYear() > 0) ? ""+data.getStartYear() : "All available";
                String eyear = (data.getEndYear() > data.getStartYear()) ? "" : ""+data.getEndYear();
                s += "<td>"+syear+"</td><td>"+eyear+"</td><td>"+data.getChartType()+"</td>";
                char tick = ( data.isThumbnail() )? '\u2714' : 0; // a unicode tick: see http://en.wikipedia.org/wiki/Tick_(checkmark)
                s += "<td>" + tick + "</td></tr>\n";
                return s;
        }

}
