import csv
import simplekml

inputfile = csv.reader(open('on_ab.csv','r', encoding='ISO-8859-1'))
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
    
    theCoords = [(row[10],row[11])]
    
    pnt = kml.newpoint(name=theName, description=theDesc, coords=theCoords)
    
kml.save('../kml/on-ab.kml')
