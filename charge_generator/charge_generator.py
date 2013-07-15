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


# def ():
        # template.date = datetime.datetime.now()
        # outfile.write( str(template) )
        # outfile.close()             
        # 
        
sep = os.path.sep        
scriptPath = sys.path[0]
templatesPath = scriptPath+sep+'templates'+sep
csvPath = scriptPath+sep+'csv'+sep
outpath = scriptPath+sep+"out"+sep

outfile = file( outpath + 'charges.adb', 'w' );

chargeTemplate = Template( file=templatesPath+"charge.tmpl" )
targetTemplate = Template( file=templatesPath+"target.tmpl" )
        
reader = csv.reader( open( csvPath+"la_charges_2007_8.csv", "rb" ))

out_lists = {}
out_lists[ 'min' ]= {}
out_lists[ 'min' ]['unemployed'] = []
out_lists[ 'min' ]['adult'] = []
out_lists[ 'min' ]['juvenile'] = []
out_lists[ 'min' ]['senior citizen'] = []
out_lists[ 'av' ]= {}
out_lists[ 'av' ]['unemployed'] = []
out_lists[ 'av' ]['adult'] = []
out_lists[ 'av' ]['juvenile'] = []
out_lists[ 'av' ]['senior citizen'] = []
out_lists[ 'max' ]= {}
out_lists[ 'max' ]['unemployed'] = []
out_lists[ 'max' ]['adult'] = []
out_lists[ 'max' ]['juvenile'] = []
out_lists[ 'max' ]['senior citizen'] = []


for row in reader:
        # print row
        unit = ''
        ctype  = row[0]
        ch1 = str.lower(ctype[0:1])
        whofor =  str.lower(row[1])
        cmin   = row[3]
        cav    = row[4]
        cmax   = row[5]   
        if( str.find( cmin, '.' ) < 0):
                cmin += ".0"
        if( str.find( cav, '.' ) < 0):
                cav += ".0"
        if( str.find( cmax, '.' ) < 0):
                cmax += ".0"
        if( ch1 >= 'a' ) and ( ch1 <= 'z' ) and ( whofor == 'adult' ):
                charge_name = ctype
                unit = ''
        elif ch1 == '(' :
                unit = row[0] 
                
    #print ch1
    
        chargeTemplate.period = 'weekly'
        chargeTemplate.freq = '1.0'
        desc = charge_name + " : " + whofor + " " + unit;
        chargeTemplate.charge_name = desc;
        chargeTemplate.comment = desc
        targetTemplate.name = desc;
        targetTemplate.targetsDescs = []
        targetTemplate.charges = []
        if( whofor == 'senior citizen' ):
                targetTemplate.min_age = 60;
                targetTemplate.max_age = 999;
        elif( whofor == 'unemployed' ):
                targetTemplate.min_age = 16;
                targetTemplate.max_age = 59;
                targetTemplate.targetsDescs.append( "ep.Include( target.employment, unemployed );" )
                targetTemplate.targetsDescs.append( "ep.Include( target.employment, permanently_sick_disabled );" )
        elif( whofor == 'juvenile' ):
                targetTemplate.min_age = 8;
                targetTemplate.max_age = 15;            
        elif( whofor == 'adult' ):
                targetTemplate.min_age = 16;
                targetTemplate.max_age = 59;
                

        chargeTemplate.amount = cmin
        targetTemplate.charges.append( str( chargeTemplate ) )
        out_lists[ 'min' ][whofor].append( str( targetTemplate ))
        
        chargeTemplate.amount = cav
        out_lists[ 'av' ][whofor].append( str( targetTemplate ))
        
        chargeTemplate.amount = cmax
        out_lists[ 'max' ][whofor].append( str( targetTemplate ))


print " -- ======= MIN ========= "
for ch in out_lists[ 'min' ]['unemployed']:
        print ch
for ch in out_lists[ 'min' ]['adult']:
        print ch
for ch in out_lists[ 'min' ]['senior citizen']:
        print ch
for ch in out_lists[ 'min' ]['juvenile']:
        print ch

print "-- ======= AVG =========== "    
for ch in out_lists[ 'av' ]['unemployed']:
        print ch
for ch in out_lists[ 'av' ]['adult']:
        print ch
for ch in out_lists[ 'av' ]['senior citizen']:
        print ch
for ch in out_lists[ 'av' ]['juvenile']:
        print ch

print "-- ======= MAX =========== "    
for ch in out_lists[ 'max' ]['unemployed']:
        print ch
for ch in out_lists[ 'max' ]['adult']:
        print ch
for ch in out_lists[ 'max' ]['senior citizen']:
        print ch
for ch in out_lists[ 'max' ]['juvenile']:
        print ch

