#!/bin/sh
#
# make a jar file out of just our chart class files and the licence
# 
#
cd ../bin
find . -name "*.class" -o -name "*.lic" -o -name "*.ttf" | xargs jar cf ../../javachart.jar
