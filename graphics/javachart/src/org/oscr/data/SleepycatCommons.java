package org.oscr.data;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.logging.Logger;
import java.util.Set;

import org.oscr.charts.ChartData;

import com.sleepycat.bind.EntryBinding;
import com.sleepycat.bind.serial.ClassCatalog;
import com.sleepycat.bind.serial.SerialBinding;
import com.sleepycat.bind.serial.StoredClassCatalog;
import com.sleepycat.bind.tuple.StringBinding;
import com.sleepycat.collections.StoredMap;
import com.sleepycat.je.Database;
import com.sleepycat.je.DatabaseConfig;
import com.sleepycat.je.DatabaseException;
import com.sleepycat.je.DatabaseNotFoundException;
import com.sleepycat.je.Environment;
import com.sleepycat.je.EnvironmentConfig;

public class SleepycatCommons{

        private static StoredClassCatalog JAVA_CATALOG;

        private static final String JAVA_CATALOG_ID = "JAVA_CATALOG_ID";

        private static Environment DB_ENVIRONMENT = null;

        private static final String DB_ENVIRONMENT_LOCATION = "/home/graham_s/VirtualWorlds/projects/oscr/model/database/";

        public static EntryBinding getEntryBindingForClass( Class clazz ) {
                ClassCatalog catalog = getClassCatalog();
                EntryBinding binding = new SerialBinding( catalog, clazz );
                return binding;
        }

        public static EntryBinding getEntryBindingForChartData() {
                return getEntryBindingForClass( ChartData.class );
        }

        public static StoredMap getChartDataMap( Database db ) {
                return new StoredMap( db, new StringBinding(), getEntryBindingForChartData(), true );
        }

        public static Set<String> getAllDatabaseNames() {
                Set s = null;
                try {
                        s = new HashSet<String>( getEnvironment().getDatabaseNames() );
                } catch ( DatabaseException dbe ) {
                        dbe.printStackTrace();

                }
                return s;
        }

        /**
         * @see:
         */
        public static StoredClassCatalog getClassCatalog() {
                if (JAVA_CATALOG == null) {
                        try {
                                DatabaseConfig dbConfig = new DatabaseConfig();
                                // dbConfig.setTransactional(true);
                                dbConfig.setAllowCreate( true );
                                Database catalogDb = getEnvironment().openDatabase( null, JAVA_CATALOG_ID, dbConfig );
                                JAVA_CATALOG = new StoredClassCatalog( catalogDb );
                        } catch ( DatabaseException dbe ) {
                                dbe.printStackTrace();

                        }
                }
                return JAVA_CATALOG;
        }

        /**
         * Open the environment. Singleton method. Allow it to be created if it
         * does not already exist.
         * 
         * @see: http://www.oracle.com/technology/documentation/berkeley-db/je/GettingStartedGuide/Env.html#EnvOpen
         * @see: http://www.oracle.com/technology/documentation/berkeley-db/je/GettingStartedGuide/EnvProps.html
         *       on memory tweaks
         */
        public static synchronized Environment getEnvironment() {
                if (DB_ENVIRONMENT == null) {
                        try {
                                EnvironmentConfig envConfig = new EnvironmentConfig();
                                envConfig.setAllowCreate( true );
                                DB_ENVIRONMENT = new Environment( new File( DB_ENVIRONMENT_LOCATION ), envConfig );
                        } catch ( DatabaseException dbe ) {
                                dbe.printStackTrace();
                        }
                }
                return DB_ENVIRONMENT;
        }

        public static void close() {
                try {
                        if (DB_ENVIRONMENT != null) {
                                DB_ENVIRONMENT.cleanLog();
                                DB_ENVIRONMENT.close();
                        }
                } catch ( DatabaseException dbe ) {
                        dbe.printStackTrace();
                }
        }

        /**
         * Create a database in the default Environment with the name of dbName
         * Disallows duplicates
         */
        public static Database getDatabase( String dbName, boolean allowCreate ) {
                Logger logger = Logger.getLogger( "sleepycatCommons.getDatabase" );
                Database myDatabase = null;
                try {
                        DatabaseConfig dbConfig = new DatabaseConfig();
                        dbConfig.setAllowCreate( allowCreate );
                        logger.finest( "opening database " + dbName ); 
                        myDatabase = getEnvironment().openDatabase( null, dbName, dbConfig );
                } catch ( DatabaseException dbe ) {
                        dbe.printStackTrace();
                }
                return myDatabase;
        }
        
        public static void removeDatabase( String dbName ) throws IOException{
                Logger logger = Logger.getLogger( "sleepycatCommons.removeDatabase" );
                try{
                        Environment env = getEnvironment();
                        env.cleanLog();
                        Database db =  getDatabase( dbName, false );
                        if( db != null ){
                                db.close();
                                env.removeDatabase( null, dbName );
                                logger.fine( "deleted database " + dbName  );
                        } else {
                                logger.info( "attempted to delete database " + dbName + " which doesn't exist " );
                        }
                } catch ( DatabaseNotFoundException dnf ){                        
                        logger.info( "attempted to delete database " + dbName + " which doesn't exist " );
                } catch ( DatabaseException dbe ){
                        throw new IOException( dbe );      
                }
        }
}
