package org.oscr.servlet;

import java.util.*;

import org.oscr.charts.ChartData;
import org.oscr.utils.LRUCache;


public class Caches{
        
        private static int MAX_CHART_LISTS = 50;
        private static int MAX_CACHED_CHARTS = 2000;
        
        static Map<String, Map<String,ChartData>> MICRO_CHART_LIST_CACHE = new LRUCache<String, Map<String,ChartData>>( MAX_CHART_LISTS );
        static Map<String,byte[]> CHARTS_CACHE = new LRUCache<String,byte[]>( MAX_CACHED_CHARTS );
}
