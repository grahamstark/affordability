#
# copyrigh(c) 2007 Graham Stark (graham.stark@virtual-worlds.biz)
#
# ////////////////////////////////
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
# 
# It is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this software; see the file docs/gpl_v3.  If not, write to
# the Free Software Foundation, Inc., 51 Franklin Street,
# Boston, MA 02110-1301, USA.
# 
# /////////////////////////////
# $Revision: 8611 $
# $Author: graham_s $
# $Date: 2010-02-11 19:10:25 +0000 (Thu, 11 Feb 2010) $
#
"""
 code to generate our ada .ads and .adb files, using Cheetah templating system
 for the most part
"""
from Cheetah.Template import Template
import datetime
import os
import sys
import csv
import re
import string

def censor( s ):
        s =  strip( s )
        table = string.maketrans( "() ", "___" );
        #s = string.capwords( s );
        s = s.translate( table )
        s = s.strip( "_" );
        s = s.replace( "__", "_" );
        return s
        

def strip( string ):
        return string.strip()

class OneCharge:
        def __init__( self, minAge, maxAge, charge ):
                self.minAge = minAge
                self.maxAge = maxAge
                self.charge = charge
        
        def __str__( self ):
                return " minAge %(mi)d maxAge %(ma)d charge %(ch)s" % { 'mi':self.minAge, 'ma':self.maxAge, 'ch':self.charge}
class Schooldesc:
        
        def getCharges( self, expensive ):
                charges = []
                if( self.starty < 12 ) and (self.endy > 12 ):
                        print "start < 12 end > 12 "
                        if( expensive ):
                                charges.append( OneCharge( self.starty, 11, self.min_junior_fee ))
                        else:
                                charges.append( OneCharge( self.starty, 11, self.min_junior_fee ))
                        startYear = 12
                else:
                        startYear = self.starty
                if( expensive ):
                        charges.append( OneCharge( startYear, self.endy, self.max_boarding_fee ))
                else:
                        charges.append( OneCharge( startYear, self.endy, self.max_senior_fee ))
                return charges
                
        
        def __init__( self, string ):
                self.girls = 0
                self.boys = 0
                self.starty = 5
                self.endy = 18
                self.min_junior_fee  = -99.0;
                self.max_senior_fee = -99.0;
                self.max_boarding_fee = -99.0
                if( re.match('.*Co\-ed', string )):
                        self.girls = 1
                        self.boys = 1
                elif( re.match('.*Girls.*', string )):
                        self.girls = 1
                elif ( re.match('.*Boys.*', string )):
                        self.boys = 1
                else: # somebody must go there..
                        self.girls = 1
                        self.boys = 1
                m = re.match( '.* ([0-9]+) *\- *([0-9]+)', string )
                if( m ):
                        self.starty = int(m.group(1))
                        self.endy = int(m.group(2))
                        
        def __str__( self ):
                return " boys %(b)d girls %(g)d %(starty)s %(endy)s" % { 'b':self.boys, 'g':self.girls, 'starty':self.starty, 'endy':self.endy}
                
# def ():
        # template.date = datetime.datetime.now()
        # outfile.write( str(template) )
        # outfile.close()             
        # 
        
sep = os.path.sep        
scriptPath = sys.path[0]
templatesPath = scriptPath+sep+'templates'+sep
csvPath = scriptPath+sep+'csv'+sep
outPath = scriptPath+sep+"out"+sep

outfile = file( outPath + 'public_school_charges.adb', 'w' );
infile = file( csvPath + 'public_school_charges.txt', 'r' );

chargeTemplate = Template( file=templatesPath+"charge.tmpl" )
targetTemplate = Template( file=templatesPath+"target.tmpl" )

appTemplate_cheap = Template( file=templatesPath+"app.tmpl" )

appTemplate_expensive = Template( file=templatesPath+"app.tmpl" )
appTemplate_expensive.restrictions = []
appTemplate_cheap.restrictions = []
regimeTemplate = Template( file=templatesPath+"regime.tmpl" )
allRegimesTemplate = Template( file=templatesPath+"all_regimes.tmpl" )
allRegimesTemplate.function_names = []
appTemplate_expensive.restrictions.append("app.max_children := Child_Range'Last;");
appTemplate_cheap.restrictions.append("app.max_children := 1;" );

while 1:
        n = strip(infile.readline())
        appTemplate_expensive.targets = []
        appTemplate_cheap.targets = []
        regimeTemplate.functions = []
        regimeTemplate.function_names = []
        if not n:
                break
        location = strip(infile.readline())
        descstr = strip(infile.readline())
        print descstr 
        desc = Schooldesc( descstr );
        
        appname = censor( location ) + "_" + n
        regimeTemplate.name = appname
        appTemplate_expensive.name = n + "_"+location + " : " + descstr + " : high cost"
        appTemplate_cheap.name =  n + "_"+location + " : " + descstr + " : low cost"
        appTemplate_expensive.appname =  appname+"_High_Cost"
        appTemplate_cheap.appname = appname + "_Low_Cost"
        
        regimeTemplate.function_names.append( "Construct_" + appname + "_High_Cost")
        regimeTemplate.function_names.append( "Construct_" + appname + "_Low_Cost")
        
        allRegimesTemplate.function_names.append( "Construct_" + appname );
        
        print desc
        desc.min_junior_fee = float(strip(infile.readline()))
        desc.max_senior_fee = float(strip(infile.readline()))

        bs = strip(infile.readline())
        if( len(bs) == 0 ):
                desc.max_boarding_fee = desc.max_senior_fee
        else:
                desc.max_boarding_fee = float(bs)
        print desc.max_boarding_fee
        
        chargeTemplate.freq = '1.0'
        targetTemplate.targetsDescs = []
        chargeTemplate.period = 'annual'
        if( (desc.girls * desc.boys) == 0 ):
                if( desc.girls ):
                        targetTemplate.targetsDescs.append( "sp.Include( target.genders, female );" )
                else:
                        targetTemplate.targetsDescs.append( "sp.Include( target.genders, male );" )

        charges = desc.getCharges( 0 )
        
        for charge in charges:
                description = location + " : ages %(mi)d - %(ma)d - low cost" % { 'mi':charge.minAge, 'ma':charge.maxAge }
                chargeTemplate.charge_name = description  
                chargeTemplate.comment = description
                targetTemplate.name = description
                targetTemplate.min_age = charge.minAge
                targetTemplate.max_age = charge.maxAge                       
                chargeTemplate.amount = charge.charge
                targetTemplate.charges = []
                targetTemplate.charges.append( str( chargeTemplate ))
                appTemplate_cheap.targets.append( str( targetTemplate ))
                
        charges = desc.getCharges( 1 )
        
        for charge in charges:
                description = location + " : ages %(mi)d - %(ma)d - high cost" % { 'mi':charge.minAge, 'ma':charge.maxAge }
                chargeTemplate.charge_name = description  
                chargeTemplate.comment = description
                targetTemplate.name = description
                targetTemplate.min_age = charge.minAge
                targetTemplate.max_age = charge.maxAge                       
                chargeTemplate.amount = charge.charge
                targetTemplate.charges = []
                targetTemplate.charges.append( str( chargeTemplate ))                
                appTemplate_expensive.targets.append( str( targetTemplate ))
        

        
        regimeTemplate.functions.append( str( appTemplate_expensive ))
        regimeTemplate.functions.append( str( appTemplate_cheap ))        
        outfile.write(str( regimeTemplate ));
        
outfile.write(str( allRegimesTemplate ));

