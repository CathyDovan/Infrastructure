import csv
import simplekml

inputfile = csv.reader(open('y_2016-2017.csv','r', encoding='ISO-8859-1'))
kml=simplekml.Kml()

for row in inputfile:
    
    theName = ""+row[1]+" ("+row[0]+")"
    theDesc = "<![CDATA["
    theDesc += "<dl class=\"mrgn-tp-md dl-horizontal\">"
    theDesc += "<dt>Asset Type:</dt> <dd>"+row[5]+"</dd>"
    theDesc += "<dt>Program:</dt> <dd>"+row[2]+"</dd>"
    theDesc += "<dt>NAPCS Asset Category:</dt> <dd>"+row[4]+"</dd>"
    theDesc += "</dl>"
    theDesc += "]]>"
    
    theCoords = [(row[11],row[12])]
    
    pnt = kml.newpoint(name=theName, description=theDesc, coords=theCoords)
    
kml.save('../kml/cwwfptfi-2016-2017-1.kml')
