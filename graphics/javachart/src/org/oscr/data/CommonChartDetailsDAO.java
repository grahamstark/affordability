package org.oscr.data;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;


public class CommonChartDetailsDAO {
        
        
        private Connection con;
        private PreparedStatement stmt;
        private PreparedStatement fontStmt;
        
        private static final String COMMON_DETAILS_STMT = "select footer, font, colour1, colour2, colour3, colour4, thumbnail_height, thumbnail_width, full_height, full_width from common_chart_details where name=?";
        private static final String GET_FONT_STMT = "select alias3 from font_type where name=?";

        public CommonChartDetailsDAO() throws SQLException{
                this.con = ChartDataCommons.makeConnection();
                this.stmt = con.prepareStatement ( COMMON_DETAILS_STMT );
                this.fontStmt = con.prepareStatement ( GET_FONT_STMT );
        }
        
        public CommonChartDetails load( String name ) throws SQLException{
                this.stmt.setString(  1, name );
                this.stmt.execute ();
                ResultSet rs = this.stmt.getResultSet ();
                rs.next();
                CommonChartDetails dets = new CommonChartDetails();
                dets.setFooter ( rs.getString (  "footer" ) );
                String fontId = rs.getString (  "font" );
                dets.setFullHeight ( rs.getInt("full_height") );
                dets.setFullWidth ( rs.getInt("full_width") );
                dets.setThumbnailWidth ( rs.getInt("thumbnail_width") );
                dets.setThumbnailHeight ( rs.getInt("thumbnail_height") );
                int[] cols = new int[4];
                for( int c = 0; c < 4; c++ ){
                        String colstr = rs.getString ( "colour"+(c+1) );
                        colstr = colstr.substring ( 1 );// strip leading '#'
                        cols[c] = Integer.parseInt ( colstr, 16 );
                }
                dets.setColour( cols );
                this.fontStmt.setString(1, fontId );
                this.fontStmt.execute ();
                ResultSet frs = this.fontStmt.getResultSet ();
                frs.next();
                String fontAlias = frs.getString( "alias3" );
                dets.setFont ( fontAlias );
                return dets;
                
        }
        
}
