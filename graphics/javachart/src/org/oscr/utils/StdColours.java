package org.oscr.utils;

/*
 *
 */
import java.awt.Color;

/**
 * a set if standard colours for Graphs etc;
 * 
 * @author Graham Stark
 */
public enum StdColours {
        // start with some bright colours for graphs 
        SCARLET( "SCARLET", new Color( 0xee, 0x44, 0x22 ) ), // red
        ROYAL_BLUE( "ROYAL_BLUE", new Color( 0x22, 0x44, 0xee ) ), // royal blue
        GREEN( "GREEN", Color.GREEN ),
        RICH_PURPLE( "RICH_PURPLE", new Color( 0x9b, 0x44, 0xe2 ) ), // 
        ORANGE( "ORANGE", Color.ORANGE ),
        
        DARK_GRAY_BLUE( "DARK_GRAY_BLUE", new Color( 0x66, 0x66, 0x99 ) ), // sun primary 1
        LIGHT_PURPLE( "LIGHT_PURPLE", new Color( 0xea, 0x9a, 0xd7 ) ), // sun secondary 1
        SUN_PRIMARY_2( "SUN_PRIMARY_2", new Color( 0x99, 0x99, 0xcc ) ), // sun primary 2
        PALE_BROWN( "PALE_BROWN", new Color( 0xdd, 0xbb, 0x99 ) ), 
        PINK( "PINK", new Color( 0xff, 0xcc, 0xbb ) ), 
        SLATE_BLUE("SLATE_BLUE", new Color( 0x38, 0x82, 0x99 ) ), // green dark
        LIGHT_TURQUOISE( "LIGHT_TURQUOISE", new Color( 0x80, 0xba, 0xcc ) ), // green med
        PALE_BLUE( "PALE_BLUE", new Color( 0xaa, 0xcc, 0xee ) ), 
        SUN_PRIMARY_1( "SUN_PRIMARY_1", new Color( 0xbb, 0xcc,0xff ) ), 
        GRAY_BLUE( "GRAY_BLUE", new Color( 0x99, 0xbb, 0xdd ) ),
        PALE_ORANGE( "PALE_ORANGE", new Color( 0xee, 0xcc, 0xaa ) ), 

        VLA_LIGHT_BLUE( "VLA_LIGHT_BLUE", new Color( 0xaa, 0xcc, 0xee ) ), 
        VLA_MED_BLUE( "VLA_MED_BLUE", new Color( 0x99, 0xbb, 0xdd ) ), 
        VLA_DARK_BLUE( "VLA_DARK_BLUE", new Color( 0x66, 0x99, 0xcc ) ), 
        VLA_TEXT_BLUE( "VLA_TEXT_BLUE", new Color( 0x15, 0x24, 0x84 ) ),

        
        SUN_SECONDARY_1( "SUN_SECONDARY_1", new Color( 0x66, 0x66, 0x66 ) ), // sun secondary 1
        SUN_SECONDARY_2( "SUN_SECONDARY_2", new Color( 0x99, 0x99, 0x99 ) ), // sun secondary 2
        VLA_GRAY_1( "VLA_GRAY_1", new Color( 0xcc, 0xcc, 0xcc ) ), // highlight on vla stylesheet
        WHITE( "WHITE", new Color( 0xff, 0xff, 0xff ) ), // highlight on vla stylesheet
        BLACK( "BLACK", new Color( 0x00, 0x00, 0x00 ) ); // highlight on vla stylesheet

        StdColours( String _name, Color _color ) {
                this.colour = _color;
                this.name = _name;
        }

        private final Color colour;

        private final String name;

        public String toString() {
                return this.name;
        }

        public Color getColour() {
                return this.colour;
        }

        public Color getColor() {
                return this.colour;
        }
}