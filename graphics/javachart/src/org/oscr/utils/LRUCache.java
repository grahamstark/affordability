package org.oscr.utils;
/*
 * $Author: $
 * $Revision: $
 *
 */

/**
 * This is a very simple Least Recently Used Cache, borrowed from:
 * http://javaalmanac.com/egs/java.util/coll_Cache.html
 * 
 * @author Graham Stark
 * @date 7/12/2004
 */
import java.util.LinkedHashMap;
import java.util.Map;

public class LRUCache<Key,Val> extends LinkedHashMap implements Map{

        public static final int DEFAULT_MAX_ENTRIES = 1000;
        
        static final long serialVersionUID = 12345l;
        
        private int maxEntries;

        /**
         * constructs a cache with default number of entries.
         *  
         */
        public LRUCache(){
                this( DEFAULT_MAX_ENTRIES );
        }

        /**
         * constructs a cache with maxEntries entries.
         * 
         * @param maxEntries
         */
        public <Key,Val>LRUCache( int maxEntries ){
                super( maxEntries + 1, .75F, true );
                this.maxEntries = maxEntries;
       }

        /**
         * This method is called just after a new entry has been added
         * 
         * @param eldest -
         *                kill this one if we're at capacity.
         */
        public boolean removeEldestEntry( Map.Entry eldest ){
                return size() > this.maxEntries;
        }
}