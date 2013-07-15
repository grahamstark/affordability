package org.oscr.data;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.logging.Logger;

import org.oscr.utils.Utils;

public class ChartDataCommons{
        
        public static final String UPDATE_COUNT_STMT = "update run_submission set num_charts_loaded = ? where run_id = ?";
        public static final String UPDATE_STATE_STMT = "update run_submission set run_state = ? where run_id = ?";
        
        public static Connection makeConnection() throws SQLException{
                Connection connection = null;
                Logger logger = Logger.getLogger( "ChartDataCommons" );
                try {
                        // Load the JDBC driver
                        String driverName = "org.gjt.mm.mysql.Driver"; 
                        Class.forName ( driverName );
                        // Create a connection to the database
                        String serverName = "localhost";
                        String mydatabase = "adrs_data";
                        String url = "jdbc:mysql://" + serverName + "/"
                                        + mydatabase; // a JDBC url
                        String username = "root";
                        String password = "iainkath";
                        connection = DriverManager.getConnection ( url,
                                        username, password );
                } catch ( ClassNotFoundException e ) {
                        logger.severe( "mysql class loading failed " + Utils.printStackTrace( e ) );
                }
                return connection;
        }

}
