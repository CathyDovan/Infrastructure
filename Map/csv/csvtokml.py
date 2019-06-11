import csv
import simplekml

inputfile = csv.reader(open('Output.csv','r', encoding='ISO-8859-1'))
kml=simplekml.Kml()

for row in inputfile:
    
    theName = ""+row[3]+" ("+row[0]+")"
    theDesc = "<![CDATA["
    theDesc += "<dl class=\"mrgn-tp-md dl-horizontal\">"
    theDesc += "<dt>Asset Type:</dt> <dd>"+row[2]+"</dd>"
    theDesc += "<dt>Program:</dt> <dd>"+row[4]+"</dd>"
    theDesc += "<dt>NAPCS Asset Category:</dt> <dd>"+row[5]+"</dd>"
    theDesc += "<dt>Total Eligible Cost:</dt> <dd>"+row[6]+"</dd>"
    theDesc += "<dt>Federal Contribution:</dt> <dd>"+row[7]+"</dd>"
    theDesc += "<dt>Forecasted Start Date:</dt> <dd>"+row[8]+"</dd>"
    theDesc += "<dt>Forecasted End Date:</dt> <dd>"+row[9]+"</dd>"
    theDesc += "<dt>Jobs created:</dt> <dd>"+row[13]+" ("+row[12]+" direct)</dd>"
    theDesc += "<dt>Value Added:</dt> <dd>"+row[16]+" ("+row[15]+" direct)</dd>"
    theDesc += "<dt>Employee compensation:</dt> <dd>"+row[19]+" ("+row[18]+" direct)</dd>"
    theDesc += "<dt>Imports:</dt> <dd>"+row[21]+"</dd>"

    theDesc += "</dl>"
    theDesc += "]]>"
    
    theCoords = [(row[10],row[11])]
    
    pnt = kml.newpoint(name=theName, description=theDesc, coords=theCoords)
    
kml.save('../kml/infc-map15.kml')
